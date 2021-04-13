namespace TransportApplication.Database

open System
open TransportApplication.Utils.Misc
open TransportApplication.Utils.Database
open TransportApplication.Utils.Database.Transactions
open TransportApplication.Entities.Location

module LocationRepository =
    let setUserLocation (userId : int) (lat : double) (lng : double) (time : DateTimeOffset): Async<unit> =
        let sql = """
            insert into transport.driver_positions
                (coords, time, driver_id)
            values
                (ST_SETSRID(ST_MakePoint(:lng, :lat),4326), to_timestamp(:time), :driver)
        """

        let data = dict [
            "lng" => lng
            "lat" => lat
            "time" => time.ToUnixTimeSeconds ()
            "driver" => userId
        ]

        transaction {
            return! execute sql data
        } |> ignoreOrRaise


    let getNearestDrivers (lat : double) (lng : double): Async<double seq> =
        let sql = """
            select ST_Distance(coords, ST_SETSRID(ST_MakePoint(:lng,:lat), 4326)) as distance
            from (select id, coords, time, driver_id, rank() over
                            (partition by driver_id
                            order by time desc) as rnk
                      from transport.driver_positions) as pos
            where rnk = 1
            and time > now() - interval '5 minutes'
            order by coords <-> ST_SETSRID(ST_MakePoint(:lng,:lat), 4326)
            limit 5
        """

        let data = dict [
            "lat" => lat
            "lng" => lng
        ]

        transaction {
            return! query<double> sql data
        } |> okOrRaise

    let getCurrentPosition (userId : int): Async<Location option> =
        let sql = """
            select ST_X(coords::geometry) as lng, ST_Y(coords::geometry) as lat
            from (select id, coords, time, driver_id, rank() over
                            (partition by driver_id
                            order by time desc) as rnk
                      from transport.driver_positions) as pos
            where rnk = 1
            and time > now() - interval '5 minutes'
            and driver_id = :user
            limit 1
        """

        let data = dict [
            "user" => userId
        ]

        transaction {
            let! res = query<Location> sql data
            return res |> Seq.tryHead
        } |> okOrRaise
