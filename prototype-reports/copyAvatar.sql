COPY (SELECT 
    'cp /nyu/store/' || substr(cast(a.sha1 as varchar(80)), 3, 2) 
     || '/' || right(cast(a.sha1 as varchar(80)), -4) 
  || ' /nyu/stage/reda/avatar/' || a.name || '.' || f.extension[1] 
FROM asset a
    INNER JOIN avatar av on av.asset = a.id
    INNER JOIN format f on a.format = f.id) TO '/tmp/avatar.sh';
