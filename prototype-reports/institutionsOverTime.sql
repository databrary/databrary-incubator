SELECT monthp, sum(cnt) OVER (ORDER BY monthp) as cum_cnt
FROM
       (SELECT  date_trunc('month', a.audit_time) as monthp, count(*) as cnt
        FROM 
               (SELECT * FROM audit.party WHERE id in
                        (
                          SELECT child FROM authorize_view join party on authorize_view.child = party.id
                            WHERE parent = 0 AND child > 4 AND site = 'ADMIN'
                        )
                AND audit_action = 'add') a 
       ) b
