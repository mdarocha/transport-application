namespace TransportApplication.Handlers

open System
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Entities.Place
open TransportApplication.Database.PlaceRepository
open TransportApplication.Database.LocationRepository
open TransportApplication.Database.OrderRepository
open TransportApplication.Utils.Authentication.Http
open TransportApplication.Utils.Validation

module PlaceOrder =
    module Models =
        [<CLIMutable>]
        type LocationQuery = {
            Lat : float
            Lng : float
        }

        type AutocompleteAddress = {
            Address : string
            Latitude : double
            Longitude : double
        }

        [<CLIMutable>]
        type CreatePlaceRequest = {
            Latitude : double
            Longitude : double
            Name : string
            Description: string
        }

        type CreatePlaceResponse = {
            Id : int
        }

        [<CLIMutable>]
        type OrderInfoQuery = {
            Lat : float
            Lng : float
            PlaceId : int
        }

        type DeliveryTime
            = NoDrivers
            | Slow
            | Quick

        type OrderInfo = {
            Price : int
            DeliveryTime : DeliveryTime
            Currency : string
        }

        [<CLIMutable>]
        type OrderRequest = {
            FromId : int
            ToId : int
        }

    module Logic =
        open Models

        let decodeAddress (query : string): string*string =
            let words = query.Split [|' '|] |> Seq.filter (fun v -> not (String.IsNullOrWhiteSpace v))
            let houseNumber =
                words
                |> Seq.tryLast
                |> Option.bind
                    (fun v ->
                        try
                            int v |> Some
                        with _ -> None)

            match houseNumber with
            | Some number ->
                (words |> Seq.take (Seq.length words - 1) |> String.concat " ", number.ToString())
            | None ->
                (words |> String.concat " ", "%")

        let deliveryTime (distances: double seq): DeliveryTime =
            if Seq.length distances = 0 then
                NoDrivers
            else if Seq.head distances < 5000.0 then
                Quick
            else
                Slow

        let deliveryPrice =
            ( 500, "$" )


    module Endpoints =
        open Models
        open Logic
        open TransportApplication.Handlers.WebSocket

        let places (next : HttpFunc) (ctx: HttpContext): HttpFuncResult =
            task {
                match ctx.TryBindQueryString<LocationQuery>() with
                | Ok loc ->
                    let! placeList = getDeliverySources loc.Lat loc.Lng 5000
                    return! (Successful.OK placeList) next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let autocomplete (next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
            task {
                match ctx.GetQueryStringValue "q" with
                | Ok q ->
                    let (street, house) = decodeAddress q
                    let! places = getAddressLocation street house
                    return! (Successful.OK places) next ctx
                | Error _ -> return! (RequestErrors.BAD_REQUEST "Missing query parameter") next ctx
            }

        let createDestination (next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
            task {
                let! args = ctx.TryBindJsonAsync<CreatePlaceRequest> ()
                let userId = ctx.UserId ()
                match args with
                | Ok place ->
                    let! createdId = createUserDestination userId place.Latitude place.Longitude place.Name place.Description
                    return! (Successful.OK { Id = createdId }) next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let orderInfo (next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
            task {
                match ctx.TryBindQueryString<OrderInfoQuery>() with
                | Ok q ->
                    let! driverDistances = getNearestDrivers q.Lat q.Lng

                    let time = deliveryTime driverDistances
                    let price, currency = deliveryPrice

                    let info: OrderInfo = { Price = price; DeliveryTime = time; Currency = currency }

                    return! (Successful.OK info) next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let createOrder (next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
            task {
                let! args = ctx.TryBindJsonAsync<OrderRequest>()
                match args with
                | Ok order ->
                    let userId = ctx.UserId()
                    let! currentOrder = getCurrentOrder userId
                    match currentOrder with
                    | None ->
                        do! createNewOrder userId order.FromId order.ToId
                        do! Senders.sendMessageToAllDrivers RefreshJobs
                        return! (Successful.OK ()) next ctx
                    | Some _ ->
                        return! (RequestErrors.BAD_REQUEST "Order already in progress!") next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

    let handlers: HttpHandler =
        mustHaveRole "is_client" >=> choose [
            GET >=> choose
                [ route "/places" >=> Endpoints.places
                  route "/autocomplete" >=> Endpoints.autocomplete
                  route "/info" >=> Endpoints.orderInfo
                ]
            POST >=> choose [
                route "/create" >=> Endpoints.createOrder
                route "/createPlace" >=> Endpoints.createDestination
            ]
        ]
