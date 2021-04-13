namespace TransportApplication.Database

open System
open TransportApplication.Utils.Database
open TransportApplication.Utils.Database.Transactions
open TransportApplication.Utils.Misc
open TransportApplication.Entities.Order

module OrderRepository =
    module Dto =
        [<CLIMutable>]
        type JobDto = {
            Id : int
            StartTime : DateTimeOffset
            FromId : int
            FromName : string
            FromAdditionalDescription : string
            FromAddress : string
            FromLng : double
            FromLat : double
            ToId : int
            ToName : string
            ToAdditionalDescription : string
            ToAddress : string
            ToLng : double
            ToLat : double
            RouteDistanceMeters : double
            BeginDistanceMeters : double
            Status : OrderStatusType
            ClientId : int
        }

        let fromDto (j : JobDto): Job =
            {
                Id = j.Id;
                StartTime = j.StartTime.ToUnixTimeMilliseconds();
                RouteDistanceMeters = j.RouteDistanceMeters;
                BeginDistanceMeters = j.BeginDistanceMeters;
                From = {
                    Id = j.FromId;
                    FriendlyName = j.FromName;
                    AdditionalDescription = j.FromAdditionalDescription;
                    Address = j.FromAddress
                    Latitude = j.FromLat;
                    Longitude = j.FromLng;
                };
                To = {
                    Id = j.ToId;
                    FriendlyName = j.ToName;
                    AdditionalDescription = j.ToAdditionalDescription;
                    Address = j.ToAddress;
                    Latitude = j.ToLat;
                    Longitude = j.ToLng;
                };
                Status = j.Status;
                ClientId = j.ClientId;
            }

        [<CLIMutable>]
        type OrderDto = {
            Id : int
            ClientId : int
            FromId : int
            FromName : string
            FromAdditionalDescription : string
            FromAddress : string
            FromLng : double
            FromLat : double
            ToId : int
            ToName : string
            ToAdditionalDescription : string
            ToAddress : string
            ToLng : double
            ToLat : double
            Status : OrderStatusType
        }

        let orderFromDto (j : OrderDto): Order =
            {
                Id = j.Id;
                From = {
                    Id = j.FromId;
                    FriendlyName = j.FromName;
                    AdditionalDescription = j.FromAdditionalDescription;
                    Address = j.FromAddress
                    Latitude = j.FromLat;
                    Longitude = j.FromLng;
                };
                To = {
                    Id = j.ToId;
                    FriendlyName = j.ToName;
                    AdditionalDescription = j.ToAdditionalDescription;
                    Address = j.ToAddress;
                    Latitude = j.ToLat;
                    Longitude = j.ToLng;
                };
                Status = j.Status
            }


    let private insertStatus (status : OrderStatusType) (orderId : int) (driverId : int option) =
        let sql, data =
            match driverId with
            | Some id ->
                ("insert into transport.order_status (order_id, start_time, status_type, driver_id) values (:order, to_timestamp(:start), :status, :driver)"
                , dict [
                    "order" => orderId
                    "driver" => id
                    "status" => status
                    "start" => DateTimeOffset.Now.ToUnixTimeSeconds ()
                ])
            | None ->
                ("insert into transport.order_status (order_id, start_time, status_type) values (:order, to_timestamp(:start), :status)"
                , dict [
                    "order" => orderId
                    "start" => DateTimeOffset.Now.ToUnixTimeSeconds ()
                    "status" => status
                ])
        execute sql data

    let private getOrderClient (orderId : int) =
        let sql = """
            select client_id
            from transport.order as ordr
            join transport.order_status as stat
            on ordr.id = stat.id
            where ordr.id = :order
        """

        let data = dict [
            "order" => orderId
        ]

        query<int> sql data

    let createNewOrder (clientId : int) (fromId : int) (toId : int): Async<unit> =
        let orderSql = """
            insert into transport.order
                (from_place, to_place, client_id)
            values
                (:from, :to, :client)
            returning
                id
        """

        let orderData = dict [
            "from" => fromId
            "to" => toId
            "client" => clientId
        ]

        transaction {
            let! orderId = query<int> orderSql orderData
            return! insertStatus OrderStatusType.SearchingForDriver (orderId |> Seq.head) None
        } |> ignoreOrRaise

    let private deliveredOrder (driverId : int) (orderId : int) =
        let sql =
            """
            select order_id from
            (select *, rank() over (partition by ordr.id order by stat.start_time desc) as rnk
                from transport.order as ordr
                join transport.order_status as stat on ordr.id = stat.order_id) as ordr
            where rnk = 1
            and ordr.order_id = :order
            and driver_id = :driver
            and ordr.status_type = :status;
            """

        let data = dict [
            "order" => orderId
            "driver" => driverId
            "status" => OrderStatusType.Delivering
        ]

        query<int> sql data

    let acceptOrder (driverId : int) (orderId : int): Async<int> =
        transaction {
            let! _ = insertStatus OrderStatusType.Delivering orderId (Some driverId)
            let! clientId = getOrderClient orderId
            return clientId |> Seq.head
        } |> okOrRaise

    let completeOrder (driverId : int) (orderId : int): Async<int> =
        transaction {
            let! order = deliveredOrder driverId orderId
            match order |> Seq.tryHead with
            | Some _ ->
                let! _ = insertStatus OrderStatusType.Completed orderId (Some driverId)
                let! clientId = getOrderClient orderId
                return clientId |> Seq.head
            | None -> failwith "Can't complete older which wasnt accepted!"
        } |> okOrRaise

    let cancelOrder (driverId : int) (orderId : int): Async<int> =
        transaction {
            let! order = deliveredOrder driverId orderId
            match order |> Seq.tryHead with
            | Some _ ->
                let! _ = insertStatus OrderStatusType.Failed orderId (Some driverId)
                let! _ = insertStatus OrderStatusType.SearchingForDriver orderId None
                let! clientId = getOrderClient orderId
                return clientId |> Seq.head
            | None -> failwith "Can't cancel order which wasnt accepted!"
        } |> okOrRaise

    let getCurrentOrder (clientId : int): Async<Order option> =
        let sql = """
                select
                        ordr.order_id as id,
                        ordr.client_id as client_id,
                        ordr.status_type as status,

                        place_from.id as from_id,
                        place_from.friendly_name as from_name,
                        place_from.additional_description as from_additional_description,
                        transport.address(place_from.coords) as from_address,
                        ST_X(place_from.coords::geometry) as from_lng, ST_Y(place_from.coords::geometry) as from_lat,

                        place_to.id as to_id,
                        place_to.friendly_name as to_name,
                        place_to.additional_description as to_additional_description,
                        transport.addresS(place_to.coords) as to_address,
                        ST_X(place_to.coords::geometry) as to_lng, ST_Y(place_to.coords::geometry) as to_lat
                from (select *, rank() over
                                        (partition by stat.order_id
                                        order by stat.start_time desc) 
                      from transport.order as ordr
                      left join transport.order_status as stat on ordr.id = stat.order_id) as ordr
                left join transport.place as place_from on ordr.from_place = place_from.id
                left join transport.place as place_to on ordr.to_place = place_to.id
                where rank = 1
                and client_id = :client
                and status_type in (:status_searching, :status_delivering)
                limit 1
        """

        let data = dict [
            "client" => clientId
            "status_delivering" => OrderStatusType.Delivering
            "status_searching" => OrderStatusType.SearchingForDriver
        ]

        transaction {
            let! dto = query<Dto.OrderDto> sql data
            return dto
            |> Seq.map Dto.orderFromDto
            |> Seq.tryHead
        } |> okOrRaise

    let private jobSql (withClientId : bool) (additional : string): string =
        let clientIdSql =
            if withClientId then
                "ordr.client_id as client_id"
            else
                "-1 as client_id"

        $"""
        select
            ordr.order_id as id,
            ordr.start_time as start_time,
            {clientIdSql},

            place_from.id as from_id,
            place_from.friendly_name as from_name,
            place_from.additional_description as from_additional_description,
            transport.address(place_from.coords) as from_address,
            ST_X(place_from.coords::geometry) as from_lng, ST_Y(place_from.coords::geometry) as from_lat,

            place_to.id as to_id,
            place_to.friendly_name as to_name,
            place_to.additional_description as to_additional_description,
            transport.address(place_to.coords) as to_address,
            ST_X(place_to.coords::geometry) as to_lng, ST_Y(place_to.coords::geometry) as to_lat,

            ST_Distance(place_from.coords, place_to.coords) as route_distance_meters,
            ST_Distance(place_from.coords, ST_SETSRID(ST_MakePoint(:lng,:lat), 4326)) as begin_distance_meters,

            ordr.status_type as status
        from (select *, rank() over
                    (partition by stat.order_id
                    order by stat.start_time desc) 
        from transport.order as ordr
        left join transport.order_status as stat on ordr.id = stat.order_id) as ordr
        left join transport.place as place_from on ordr.from_place = place_from.id
        left join transport.place as place_to on ordr.to_place = place_to.id
        where rank = 1
        {additional}
        """

    let getJobs (lat : double) (lng : double): Async<Job seq> =
        let sql = jobSql false """
            and driver_id is null
            and status_type = :status
            order by start_time desc
        """

        let data = dict [
            "lat" => lat
            "lng" => lng
            "status" => OrderStatusType.SearchingForDriver
        ]

        transaction {
            let! result = query<Dto.JobDto> sql data
            return result |> Seq.map Dto.fromDto
        } |> okOrRaise

    let getCurrentJob (driverId : int): Async<Job option> =
        let sql = jobSql true """
            and driver_id = :driver
            and status_type = :status
            limit 1
        """

        let data = dict [
            "driver" => driverId
            "status" => OrderStatusType.Delivering
            "lat" => 0.0
            "lng" => 0.0
        ]

        transaction {
            let! dto = query<Dto.JobDto> sql data

            return dto
            |> Seq.tryHead
            |> Option.bind (fun j -> Dto.fromDto j |> Some)
        } |> okOrRaise
