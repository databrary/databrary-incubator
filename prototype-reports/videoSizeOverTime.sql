SELECT audit_time, public.asset.size FROM
    audit.asset JOIN public.asset ON audit.asset.id = public.asset.id
    WHERE audit_action = 'add' AND public.asset.format IN
    (
        SELECT id FROM format WHERE mimetype LIKE 'video%%'
    )
    AND public.asset.duration IS NOT NULL AND public.asset.size > 0
