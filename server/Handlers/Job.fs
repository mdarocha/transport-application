namespace TransportApplication.Handlers

open System
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Entities.Order
open TransportApplication.Database.OrderRepository
open TransportApplication.Database.LocationRepository
open TransportApplication.Utils.Authentication.Http
open TransportApplication.Utils.Validation

module Job =
    module Models =
        [<CLIMutable>]
        type JobArg =
            { JobId : int }

    module Endpoints =
        open Models
        open TransportApplication.Handlers.WebSocket

        let available (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! position = ctx.UserId() |> getCurrentPosition
                match position with
                | Some pos ->
                    let! jobs = getJobs pos.Lat pos.Lng
                    return! (Successful.OK jobs) next ctx
                | None ->
                    return! (RequestErrors.BAD_REQUEST "No recent position is available!") next ctx
            }

        let accept (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! args = ctx.TryBindJsonAsync<JobArg>()
                match args with
                | Ok { JobId = id } ->
                    let userId = ctx.UserId ()
                    let! currentJob = getCurrentJob userId

                    match currentJob with
                    | None ->
                        let! clientId = acceptOrder userId id
                        do! Senders.sendMessageToAllDrivers RefreshJobs
                        do! Senders.sendMessageToUser clientId <| OrderStatusUpdate OrderStatusType.Delivering
                        return! (Successful.OK ()) next ctx
                    | Some _ ->
                        return! (RequestErrors.BAD_REQUEST "Order already in progress!") next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let complete (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! args = ctx.TryBindJsonAsync<JobArg>()
                match args with
                | Ok { JobId = id } ->
                    let! clientId = completeOrder (ctx.UserId()) id
                    do! Senders.sendMessageToUser clientId <| OrderStatusUpdate OrderStatusType.Completed
                    return! (Successful.OK ()) next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let cancel (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let! args = ctx.TryBindJsonAsync<JobArg>()
                match args with
                | Ok { JobId = id } ->
                    let! clientId = cancelOrder (ctx.UserId()) id
                    do! Senders.sendMessageToAllDrivers RefreshJobs
                    do! Senders.sendMessageToUser clientId <| OrderStatusUpdate OrderStatusType.SearchingForDriver
                    return! (Successful.OK ()) next ctx
                | Error err ->
                    return! (RequestErrors.BAD_REQUEST err) next ctx
            }

        let current (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let userId = ctx.UserId ()
                let! currentJob = getCurrentJob userId
                match currentJob with
                | Some job -> return! (Successful.OK job) next ctx
                | None -> return! (Successful.OK false) next ctx
            }

    let handlers: HttpHandler =
        mustHaveRole "is_driver" >=> choose [
            GET >=> choose
                [ route "/available" >=> Endpoints.available
                  route "/current" >=> Endpoints.current
                ]
            POST >=> choose
                [ route "/accept" >=> Endpoints.accept
                  route "/complete" >=> Endpoints.complete
                  route "/cancel" >=> Endpoints.cancel
                ]
        ]
