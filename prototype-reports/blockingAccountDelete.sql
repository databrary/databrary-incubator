------------ able to verify easily from web UI:
  -- handle these by deleting them in UI:
-- select * from authorize where child = <PID> or parent = <PID> ; 
  -- handle these by deleting them in UI (after ensuring each volume is still accessible by someone):
-- select * from volume_access where party = <PID> ;
  -- deleting party will automatically delete the avatar, no need to check if present:
-- select * from avatar where party = <PID> ;

-----------
  -- often is present; need to select a replacement user (usually the AI, or equivalent person)
select * from transcode where owner = <PID> ;
  -- rarely present; replace with AI or equivalent person
select * from comment where who = <PID> ;
select * from tag_use where who = <PID> ;
  -- haven't been present yet (rare, ephemeral?). delete these if present?
select * from account_token where account = <PID> ;
select * from login_token where account = <PID> ;  -- possible if registration is incomplete
select * from session where account = <PID> ;
select * from upload where account = <PID> ;
  -- notify preferences - rare to have a value, delete if present (probably automatic, no query check necessary)
select * from notify where target = <PID> ;
  -- probably undelivered or failure when delivering; delete if present? (probably automatic, no query check necessary)
select * from notification where agent = <PID> or party = <PID> or target = <PID> ;


-- TODO: make a failing test case for worst case scenario of delete
