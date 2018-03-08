SELECT * FROM audit.party WHERE id in
        (
          SELECT child FROM authorize_view join party on authorize_view.child = party.id
            WHERE parent = 0 AND child > 4 AND site = 'EDIT'
        )
AND audit_action = 'add'
