FROM postgis/postgis:13-3.0

COPY osm.sql /docker-entrypoint-initdb.d/1-osm.sql
COPY schema.sql /docker-entrypoint-initdb.d/2-schema.sql
COPY data.sql /docker-entrypoint-initdb.d/3-data.sql

ENV PGDATA=/data/postgres
