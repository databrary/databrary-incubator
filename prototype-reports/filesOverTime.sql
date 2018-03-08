SELECT * FROM audit.asset JOIN public.slot_asset ON audit.asset.id = public.slot_asset.asset WHERE audit_action = 'add'
