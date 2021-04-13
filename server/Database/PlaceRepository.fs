namespace TransportApplication.Database

open System
open System.Text.RegularExpressions
open TransportApplication.Utils.Misc
open TransportApplication.Utils.Database
open TransportApplication.Utils.Database.Transactions
open TransportApplication.Entities.Place

module PlaceRepository =
    let getDeliverySources (lat : float) (lng : float) (searchRadius : int): Async<Place seq> =
        let sql = """
            select id,
                ST_X(coords::geometry) as longitude,
                ST_Y(coords::geometry) as latitude,
                friendly_name, additional_description,
                transport.address(coords) AS address
            from transport.place
            where ST_DWithin(ST_SETSRID(ST_MakePoint(:lng,:lat), 4326), coords, :radius)
            and place_type = 0
        """
        let data = dict [
            "lng" => lng
            "lat" => lat
            "radius" => searchRadius
        ]

        transaction {
            return! query<Place> sql data
        } |> okOrRaise

    let getAddressLocation (streetname : string) (housenumber : string): Async<GeocodedAddress seq> =
        let sql = """
select
    ST_X(ST_Transform(ST_Centroid(p.way), 4326)) as longitude,
    ST_Y(ST_Transform(ST_Centroid(p.way), 4326)) as latitude,
    concat(l.name, ' ', p."addr:housenumber") as address
from planet_osm_polygon p
join planet_osm_line l on (ST_DWithin(p.way,l.way, 100))
where
    p.building is not null
    and p.building != 'no'
    and p."addr:housenumber" is not null
    and l.route is null
    and l.power is null
    and (l.boundary is null or l.boundary = 'administrative')
    and l.name ilike :streetname
    and p."addr:housenumber" like :housenumber
order by ST_SETSRID(ST_MakePoint(19.9368564, 50.0619474), 3857) <-> p.way
limit 3
        """

        let data = dict [
            "streetname" => streetname
            "housenumber" => housenumber
        ]

        transaction {
            return! query<GeocodedAddress> sql data
        } |> okOrRaise

    let createUserDestination (userId : int) (lat : double) (lng : double) (name : string) (description : string): Async<int> =
        let sql = """
            insert into transport.place
                (coords, friendly_name, additional_description, place_type, owner)
            values
                (ST_SETSRID(ST_MakePoint(:lng,:lat),4326), :name, :desc, 1, :owner)
            returning
                id;
        """

        let data = dict [
            "lat" => lat
            "lng" => lng
            "owner" => userId
            "name" => name
            "desc" => description
        ]

        transaction {
            let! createdId = query<int> sql data
            return createdId |> Seq.head
        } |> okOrRaise
