SELECT route, COUNT(route) FROM audit.analytic WHERE route LIKE '%%zip' GROUP BY route
