{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Routes.API
  ( swagger
  ) where

import Data.Aeson.Types
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (maybeToList, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (showVersion)
import qualified Network.HTTP.Types as HTTP
import qualified Web.Route.Invertible as R

import Paths_databrary (version)
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Swagger
import Databrary.Web.Routes
import Databrary.Model.Enum
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Action.Route
import Databrary.Controller.Paths
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Party
import Databrary.Controller.Volume

infixl 4 .++, .+
class HasFields o where
  (.++) :: o -> [Pair] -> o
  (.++) = foldl' (.+)
  (.+) :: o -> Pair -> o
  o .+ l = o .++ [l]

instance HasFields [Pair] where
  (.++) = (++)

instance HasFields Object where
  (.+) = flip $ uncurry HM.insert

data Type
  = TypeBoolean
  | TypeInteger
  | TypeNumber
  | TypeString
  -- | TypeArray ...
  | TypeFile
  deriving (Eq, Enum, Bounded, Show)

instance ToJSON Type where
  toJSON = String . T.pack . n . show where
    n ('T':'y':'p':'e':c:r) = toLower c : r
    n s = error ("toJSON Type: " ++ s)

data DataType = DataType
  { _dataType :: Type
  , _dataFormat :: Maybe T.Text
  }

typeInt16, typeInt32, typeString, typePassword, typeEmail, typeURL, typeTimestamp :: DataType
typeInt16 = DataType TypeInteger (Just "int16")
typeInt32 = DataType TypeInteger (Just "int32")
typeString = DataType TypeString Nothing
typePassword = DataType TypeString (Just "password")
typeEmail = DataType TypeString (Just "email")
typeURL = DataType TypeString (Just "url")
typeTimestamp = DataType TypeString (Just "date-time")

dataTypeJSON :: DataType -> [Pair]
dataTypeJSON (DataType t f) = ("type" .= t) : maybeToList (("format" .=) <$> f)

val :: DataType -> T.Text -> Object
val t d = HM.fromList $ ("description" .= d) : dataTypeJSON t

data ParameterLoc
  = InPath
  | InQuery
  | InData
  | InHeader

instance ToJSON ParameterLoc where
  toJSON InPath = String "path"
  toJSON InQuery = String "query"
  toJSON InData = String "formData" -- also "body", sort of
  toJSON InHeader = String "header"

data Parameter
  = Parameter
    { parameterLoc :: ParameterLoc
    , parameterName :: T.Text
    , parameterType :: DataType
    , parameterFields :: [Pair]
    }
  | ParameterRef T.Text

instance HasFields Parameter where
  p .++ l = p{ parameterFields = parameterFields p .++ l }

instance ToJSON Parameter where
  toJSON Parameter{..} = object $
    [ "name" .= parameterName
    , "in" .= parameterLoc
    ] ++ dataTypeJSON parameterType
    ++ parameterFields
  toJSON (ParameterRef r) = Object $ ref ("parameters/" <> r)

pathParameter :: T.Text -> DataType -> T.Text -> Parameter
pathParameter name dt desc = Parameter InPath name dt
  [ "required" .= True
  , "description" .= desc
  ]

queryParameter :: T.Text -> DataType -> Bool -> T.Text -> Parameter
queryParameter name dt empty desc = Parameter InQuery name dt
  [ "description" .= desc
  , "allowEmptyValue" .= empty
  ]

formParameter :: T.Text -> DataType -> Bool -> T.Text -> Parameter
formParameter name dt req desc = Parameter InData name dt
  [ "required" .= req
  , "description" .= desc
  ]

pathParameters :: [Parameter] -> [T.Text]
pathParameters = mapMaybe pp where
  pp Parameter{ parameterLoc = InPath, parameterName = n } = Just n
  pp _ = Nothing

data Response = Response
  { _responseStatus :: Maybe HTTP.Status
  , responseFields :: [Pair]
  }

instance HasFields Response where
  r .++ l = r{ responseFields = responseFields r .++ l }

responseJSON :: Response -> Pair
responseJSON (Response s p) = (maybe "default" (T.pack . show . HTTP.statusCode) s, object p)

okResp :: T.Text -> Value -> Response
okResp desc schema = Response (Just HTTP.ok200)
  [ "description" .= desc
  , "schema" .= schema
  ]

op :: T.Text -> Route r a -> a -> T.Text -> T.Text -> [Parameter] -> [Response] -> (T.Text, Object)
op i rte arg summary desc param resp =
  ( swaggerPath p $ pathParameters param
  , HM.singleton (maybe "unknown" (T.toLower . TE.decodeLatin1 . R.renderParameter) m) $ object
    [ "operationId" .= i
    , "summary" .= summary
    , "description" .= desc
    , "parameters" .= param
    , "responses" .= object (map responseJSON resp)
    ]
  ) where RequestValues m p = routeActionValues rte arg

instance HasFields (T.Text, Object) where
  (t, o) .++ l = (t, o .++ l)
  (t, o) .+ l = (t, o .+ l)

ref, def :: T.Text -> Object
ref r = HM.singleton "$ref" (String ("#/" <> r))
def = ref . ("definitions/" <>)

enum :: forall a . (DBEnum a, ToJSON a) => a -> Pair
enum _ = "enum" .= (map (toJSON . fst) v ++ map (toJSON . snd) v) where
  v :: [(a, String)]
  v = pgEnumValues

readOnly :: HasFields o => o -> o
readOnly = (.+ "readOnly" .= True)

swagger :: Value
swagger = object
  [ "swagger" .= String "2.0"
  , "info" .= object
    [ "title" .= String "Databrary"
    , "version" .= showVersion version
    , "description" .= String "All POST operations accepting formData parameters equivalently accept a JSON object body with corresponding keys, where '.' in field names are replaced by nested objects."
    ]
  , "schemes" .= [String "https"]
  , "consumes" .= map String ["application/json", "application/x-www-form-urlencoded", "multipart/form-data"]
  , "produces" .= [String "application/json"]
  , "definitions" .= object
    [ "Permission" .= object
      [ "description" .= String "Permission level as name or corresponding integer value (default in responses)"
      , enum PermissionNONE
      ]
    , "Identity" .= object
      [ "description" .= String "A party associated with a session"
      , "properties" .= object
        [ -- party properties,
          "csverf" .= val typeString "Authorization token, which must be included verbatim in a \"X-CSVerf\" header in most modifying requests"
        ]
      ]
    , "Party" .= object
      [ "description" .= String "An individual user, group, organization, or other entity"
      , "properties" .= object
        ("id" .= val typeInt32 "Party ID" :
        map (\(n, t, d, p) -> n .= (val t d .++ p)) partyFields ++
        [ "name" .= readOnly (val typeString "Full display name, calculated from concatenating 'prename' and 'sortname'")
        , "email" .= val typeEmail "Email address"
        , "permission" .= readOnly (permDesc "Level of access permission the current user has over this party")

        , "authorization" .= readOnly (permDesc "Site authorization level this party has been granted")
        ])
      , "required" .= map String ["id", "sortname"]
      ]
    , "Volume" .= object
      [ "description" .= String "The top-level organizational unit for data, represnting a dataset, study, or other data package"
      , "properties" .= object
        ("id" .= val typeInt32 "Party ID" :
        map (\(n, t, d) -> n .= val t d) volumeFields ++
        [ "doi" .= readOnly (val (DataType TypeString (Just "doi")) "Automatically assigned [DOI](http://www.doi.org)")
        , "creation" .= readOnly (val typeTimestamp "When volume was originally created")
        , "owners" .= object
          [ "type" .= String "array"
          , "description" .= String "Parties (volume owners) who have ADMIN permission on this volume, in abbreviated form containing only 'id' and 'name' fields"
          , "items" .= def "Party"
          , "readOnly" .= True
          ]
        , "permission" .= readOnly (permDesc "Level of access permission the current user has over this volume")
        , "access" .= object
          [ "type" .= String "array"
          , "description" .= String "Parties (owners, collaborators, or sharing targets) who have been granted permission on this volume"
          , "items" .= object
            [ "description" .= String "A single party (or group) with access to this volume"
            , "properties" .= object
              [ "individual" .= permDesc "Level of access permision granted directly to this user only"
              , "children" .= permDesc "Level of access permision granted to children of (users authorized by) this party, masked by their 'member' permission"
              , "sort" .= val typeInt16 "Sort (display) order of this party in access lists"
              ]
            ]
          , "readOnly" .= True
          ]
        ])
      , "required" .= map String ["id", "name"]
      ]
    ]
  , "parameters" .= object
    [ "csverfHeader" .= Parameter InHeader "X-CSVerf" typeString [ "description" .= String "\"csverf\" value from Identity object (required unless \"csverf\" is included in form data)" ]
    , "csverfForm" .= formParameter "csverf" typeString False "\"csverf\" value from Identity object (required if \"X-CSVerf\" header is not provided)"
    ]
  , "paths" .= HM.fromListWith HM.union
    [ op "get" viewRoot (JSON)
        "No-op"
        "Do nothing beyond standard request processing. Can be used to ensure endpoint is active."
        []
        [ okResp "An empty object" (object ["type" .= String "object"]) ]
    , op "getUser" viewUser ()
        "Current Account"
        "Return the identity of the currently logged-in user from the session cookie."
        []
        [ okResp "The current user" (Object $ def "Identity") ]
    , op "postUser" postUser (JSON)
        "Change Account"
        "Change the account information of the current user."
        ([formParameter "auth" typePassword True "Current (old) password"
        , formParameter "email" typeEmail False "New email address for user"
        , formParameter "password.once" typePassword False "New password for user"
        , formParameter "password.again" typePassword False "New password for user (must match password.once)"
        ] ++ csrfParams)
        [ okResp "The (updated) current party" (Object $ def "Party") ]
    , op "postLogin" postLogin (JSON)
        "Login"
        "Request and issue a new session given a set of credentials."
        [ formParameter "email" typeEmail True "Email address of account to login"
        , formParameter "password" typePassword True "Password of account to login"
        ]
        [ okResp "The authenticated current user" (Object $ def "Identity")
          .+ "headers" .= object [ "set-cookie" .= val typeString "Session cookie" ]
        ]
    , op "postLogout" postLogout (JSON)
        "Logout"
        "Terminate and invalidate the current session."
        []
        [ okResp "The now anonymous identity" (Object $ def "Identity") ]

    , op "getProfile" viewParty (JSON, TargetProfile)
        "Lookup Profile"
        "Lookup information about the current user."
        getPartyParams
        [ okResp "The current party" (Object $ def "Party") ]
    , op "getParty" viewParty (JSON, TargetParty aparty)
        "Lookup Party"
        "Lookup information about a specific party."
        (pparty : getPartyParams)
        [ okResp "The requested party" (Object $ def "Party") ]
    , op "postProfile" postParty (JSON, TargetProfile)
        "Change Profile"
        "Change the profile information of the current user."
        postPartyParams
        [ okResp "The updated party" (Object $ def "Party") ]
    , op "postParty" postParty (JSON, TargetParty aparty)
        "Change Party"
        "Change the profile information of the given party."
        (pparty : postPartyParams)
        [ okResp "The updated party" (Object $ def "Party") ]

    , op "viewVolume" viewVolume (JSON, avolume)
        "Lookup Volume"
        "Lookup information about a specific volume."
        (pvolume :
        [ ])
        [ okResp "The requested volume" (Object $ def "Volume") ]
    ]
  ] where

  permDesc d = def "Permission" .+ "description" .= String d -- doesn't work
  csrfParams = [ParameterRef "csverfHeader", ParameterRef "csverfForm"]

  aparty = Id 0
  pparty = pathParameter "party" typeInt32 "Party ID"
  getPartyParams =
    [ queryParameter "parents" typeString True "Include 'parents' in the response, optionally including 'authorization' in each parent"
      .+ "enum" .= [String "authorization"]
    , queryParameter "children" typeString True "Include 'children' in the response"
    , queryParameter "volumes" typeString True "Include 'volumes' in the response, optionally including 'access' in each volume"
      .+ "enum" .= [String "access"]
    , queryParameter "access" typeString True  "Include 'access' in the response for all volumes with at least the given permission level"
      .++ [enum PermissionNONE, "default" .= String "EDIT"]
    , queryParameter "authorization" typeString True  "Include 'authorization' in the response"
    ]
  postPartyParams =
    map (\(n, t, d, p) -> formParameter n t True d .++ p) partyFields
    ++ [ formParameter "avatar" (DataType TypeFile Nothing) False "Avatar profile image" ]
    ++ csrfParams
  partyFields =
    [ ("sortname", typeString, "Last name or primary sortable name", [])
    , ("prename",  typeString, "First name or any part of name that comes before 'sortname'", [])
    , ("orcid",    typeString, "[ORCID iD](http://en.wikipedia.org/wiki/ORCID)",
        ["pattern" .= String "^[0-9]{4}-?[0-9]{4}-?[0-9]{4}-?[0-9]{3}[0-9X]$"])
    , ("affiliation", typeString, "User-supplied university or organizational affiliation for display purposes", [])
    , ("url", typeURL, "User-supplied external web site", [])
    ]

  avolume = Id 0
  pvolume = pathParameter "volume" typeInt32 "Volume ID"
  volumeFields =
    [ ("name",   typeString, "Title")
    , ("alias",  typeString, "Private internal short name")
    , ("body",   typeString, "Description or abstract")
    ]

  -- slot = Id (SlotId (Id 0) emptySegment)
  -- container = containerSlotId (Id 0)
  -- asset = Id 0
  -- record = Id 0
  -- metric = Id 0
  -- funder = Id 0
  -- tag = TagName ""
