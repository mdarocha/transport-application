namespace TransportApplication.Handlers

open System
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Database.OrderRepository
open TransportApplication.Utils.Authentication.Http

module Tracker =
    module Models =
        type EventStatus =
            {
                HasOrder : bool
                HasJob : bool
            }


    module Endpoints =
        open Models

        let status (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let userId = ctx.UserId ()
                let! currentOrder = getCurrentOrder userId
                let! currentJob = getCurrentJob userId
                let hasOrder =
                    match currentOrder with
                    | Some _ -> true
                    | None -> false
                let hasJob =
                    match currentJob with
                    | Some _ -> true
                    | None -> false

                return! (Successful.OK { HasOrder = hasOrder; HasJob = hasJob }) next ctx
            }

        let currentOrder (next : HttpFunc) (ctx : HttpContext): HttpFuncResult =
            task {
                let userId = ctx.UserId ()
                let! currentOrder = getCurrentOrder userId
                match currentOrder with
                | Some order -> return! (Successful.OK order) next ctx
                | None -> return! (RequestErrors.BAD_REQUEST "No order at the time") next ctx
            }

    let handlers: HttpHandler =
        mustBeLoggedIn >=> choose [
            GET >=> route "/status" >=> Endpoints.status
            mustHaveRole "is_client" >=> choose [
                GET >=> route "/current" >=> Endpoints.currentOrder
            ]
        ]
