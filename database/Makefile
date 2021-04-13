build:
	docker-compose build

osm.sql: malopolskie.osm.pbf
	osm2pgsql -U postgres -H localhost -d postgres -W ./malopolskie.osm.pbf
	docker exec -u postgres postgres_container pg_dump --format plain --table public.planet_osm_roads --table public.planet_osm_point --table public.planet_osm_line --table public.planet_osm_polygon > osm.sql

malopolskie.osm.pbf:
	wget https://download.geofabrik.de/europe/poland/malopolskie-latest.osm.pbf -O malopolskie.osm.pbf

.PHONY: build
# vim:ft=make
#
