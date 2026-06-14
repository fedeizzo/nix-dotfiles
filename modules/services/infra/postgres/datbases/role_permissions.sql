DO
$do$
BEGIN
   IF EXISTS (
      SELECT FROM pg_catalog.pg_roles
      WHERE  rolname = 'readaccess_networth') THEN

      RAISE NOTICE 'Role "readaccess_networth" already exists. Skipping.';
   ELSE
      CREATE ROLE readaccess_networth;
      GRANT CONNECT ON DATABASE networth TO readaccess_networth;
      GRANT USAGE ON SCHEMA public TO readaccess_networth;
      GRANT SELECT ON ALL TABLES IN SCHEMA public TO readaccess_networth;
      GRANT readaccess_networth TO networth_ro;
   END IF;
END
$do$;
