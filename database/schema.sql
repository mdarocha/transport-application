CREATE SCHEMA transport;

CREATE TABLE transport.user
(
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    surname TEXT NOT NULL,
    phone_number TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,

    PRIMARY KEY(id)
);

CREATE TABLE transport.place
(
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    coords geography(POINT, 4326) NOT NULL,
    friendly_name TEXT,
    owner INTEGER,
    additional_description TEXT,
    place_type INTEGER NOT NULL,

    PRIMARY KEY(id)
);

CREATE TABLE transport.driver_positions
(
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    coords geography(POINT, 4326) NOT NULL,
    time TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
    driver_id INTEGER NOT NULL,

    PRIMARY KEY(id),
    FOREIGN KEY(driver_id) REFERENCES transport.user(id)
);

CREATE TABLE transport.order
(
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    from_place INTEGER NOT NULL,
    to_place INTEGER NOT NULL,
    client_id INTEGER NOT NULL,
    additional_description TEXT,

    PRIMARY KEY(id),
    FOREIGN KEY(from_place) REFERENCES transport.place(id),
    FOREIGN KEY(to_place) REFERENCES transport.place(id),
    FOREIGN KEY(client_id) REFERENCES transport.user(id)
);

CREATE TABLE transport.order_status
(
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    order_id INTEGER NOT NULL,
    start_time TIMESTAMPTZ NOT NULL,
    end_time TIMESTAMPTZ,
    driver_id INTEGER,
    status_type INTEGER NOT NULL,

    PRIMARY KEY(id),
    FOREIGN KEY(order_id) REFERENCES transport.order(id),
    FOREIGN KEY(driver_id) REFERENCES transport.user(id)
);

CREATE FUNCTION transport.address(coords geography) RETURNS text
as
    $$
    select concat(l.name, ' ', coalesce(p."addr:housenumber", n."addr:housenumber"))
    from planet_osm_polygon p
    join planet_osm_point n on (ST_Within(n.way,p.way))
    join planet_osm_line l on (ST_DWithin(n.way,l.way, 50))
    where
        p.building is not null
        and p.building != 'no'
        and (coalesce(p."addr:housenumber", n."addr:housenumber") is not null)
        and l.name is not null
        and l.route is null
        and l.power is null
        and (l.boundary is null or l.boundary = 'administrative')
        order by ST_Transform(coords::geometry, 3857) <-> n.way
        limit 1;
    $$
LANGUAGE SQL
IMMUTABLE
RETURNS NULL ON NULL INPUT;

-- vim:et
