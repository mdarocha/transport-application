namespace TransportApplication.Handlers

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Utils.Configuration
open TransportApplication.Utils.Misc
open TransportApplication.Entities

module WebSocket =
    type SocketConnection =
        {
            Id : Guid
            Socket : WebSocket
            UserId : int option
            IsDriver : bool
        }

    let mutable private sockets = list<SocketConnection>.Empty

    module private Utils =
        let getSocketById (socketId : Guid): SocketConnection option =
            sockets
            |> Seq.filter (fun s -> s.Id = socketId)
            |> Seq.tryHead

    type Message
        = Authenticate of token : string
        | AuthenticationResult of success : bool
        | LocationUpdate of lat : double * lng : double * time : DateTimeOffset
        | RefreshJobs
        | OrderStatusUpdate of status : Order.OrderStatusType

    module Message =
        open Newtonsoft.Json
        open Newtonsoft.Json.Linq

        [<CLIMutable>]
        type MessageStruct =
            {
                Type : string
                Data : JObject
            }

        let decodeMessage (data : string): Result<Message, exn> =
            try
                let json = JsonConvert.DeserializeObject<MessageStruct> data
                let msg =
                    match json.Type with
                    | "Authenticate" -> json.Data.["token"].Value<string> () |> Authenticate
                    | "AuthenticationResult" -> json.Data.["success"].Value<bool> () |> AuthenticationResult
                    | "LocationUpdate" ->
                        let lat = json.Data.["lat"].Value<double> ()
                        let lng = json.Data.["lng"].Value<double> ()
                        let time = json.Data.["time"].Value<int64> () |> DateTimeOffset.FromUnixTimeMilliseconds
                        LocationUpdate (lat, lng, time)
                    | "RefreshJobs" -> RefreshJobs
                    | "OrderStatusUpdate" ->
                        let status = json.Data.["status"].Value<Order.OrderStatusType> ()
                        OrderStatusUpdate status
                    | _ -> failwith $"Unknown message type! {json.Type}"
                Ok msg
            with
            | ex -> Error ex

        let encodeMessage (msg : Message): string =
            let data =
                match msg with
                | Authenticate token ->
                    { Type = "Authenticate"
                    ; Data = JObject.FromObject({| token = token |})
                    }
                | AuthenticationResult success ->
                    { Type = "AuthenticationResult"
                    ; Data = JObject.FromObject({| success = success |})
                    }
                | LocationUpdate (lat, lng, timestamp) ->
                    { Type = "LocationUpdate"
                    ; Data = JObject.FromObject({| lat = lat; lng = lng; time = timestamp.ToUnixTimeMilliseconds() |})
                    }
                | RefreshJobs ->
                    { Type = "RefreshJobs"
                    ; Data = JObject.FromObject({| |})
                    }
                | OrderStatusUpdate status ->
                    { Type = "OrderStatusUpdate"
                    ; Data = JObject.FromObject({| status = status |})
                    }
            JsonConvert.SerializeObject(data)

    module private SocketHandlers =
        let handleConnected (socket : WebSocket): Task<Guid> =
            task {
                let conn: SocketConnection =
                    { Id = Guid.NewGuid(); Socket = socket; UserId = None; IsDriver = false }
                do sockets <- conn :: sockets
                return conn.Id
            }

        let handleDisconnected (socketId : Guid) =
            task {
                do sockets <- Seq.filter (fun s -> s.Id <> socketId) sockets |> Seq.toList
            }

        let sendMessage (socketId : Guid) (msg : Message) =
            task {
                let msg = Message.encodeMessage msg
                let socket = Seq.filter (fun s -> s.Id = socketId) sockets |> Seq.tryHead
                match socket with
                | Some s ->
                    let segment = new ArraySegment<byte>(Encoding.UTF8.GetBytes msg)
                    if s.Socket.State = WebSocketState.Open then
                        do! s.Socket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None)
                    else
                        do! handleDisconnected socketId
                | None -> ()
            }

        let sendMessageToSockets (msg : Message) (sockets : Guid seq) =
            task {
                let sockets = sockets |> Seq.map(fun s -> sendMessage s msg)
                Task.WaitAll [| for t in sockets -> t :> Task |]
            }

    module Senders =
        let sendMessageToAllDrivers (msg : Message) =
            task {
                do!
                    sockets
                    |> Seq.filter (fun s -> s.IsDriver)
                    |> Seq.map (fun s -> s.Id)
                    |> SocketHandlers.sendMessageToSockets msg
            }

        let sendMessageToUser (userId : int) (msg : Message) =
            task {
                do!
                    sockets
                    |> Seq.filter (fun s -> s.UserId = Some userId)
                    |> Seq.map (fun s -> s.Id)
                    |> SocketHandlers.sendMessageToSockets msg
            }

    module private MessageHandlers =
        open Utils
        open SocketHandlers
        open Senders
        open TransportApplication.Database.LocationRepository
        open TransportApplication.Database.OrderRepository
        open TransportApplication.Utils.Authentication.Jwt

        let authenticate (conf : Settings) (socketId : Guid) (token : string) =
            let disconnect =
                fun () ->
                    task {
                        match getSocketById socketId with
                        | Some s ->
                            s.Socket.Abort ()
                            do! handleDisconnected socketId
                        | _ -> printf "can't abort socket id: %A" socketId
                    }

            task {
                let valid, claims = validateJwtToken conf token
                if valid then
                    try
                        let userId =
                            claims
                            |> Seq.filter (fun c -> c.Type = "sub")
                            |> Seq.map (fun c -> c.Value)
                            |> Seq.head
                            |> int

                        let isDriver =
                            claims
                            |> Seq.filter (fun c -> c.Type = "is_driver")
                            |> Seq.map (fun c -> c.Value)
                            |> Seq.head
                            |> stringToBool

                        sockets <- Seq.map (fun s->
                            if s.Id = socketId then
                                { s with UserId = Some userId; IsDriver = isDriver }
                            else
                                s) sockets |> Seq.toList

                        printfn "socket %A auth, id %d, isDriver %A" socketId userId isDriver

                        do! Message.AuthenticationResult true |> sendMessage socketId
                    with
                    | ex ->
                        printf "auth error: %A" ex
                        do! disconnect ()
                else
                    printf "token not valid"
                    do! disconnect ()
            }

        let locationUpdate (userId : int) (isDriver : bool) (lat : double) (lng : double) (time : DateTimeOffset) =
            task {
                do! setUserLocation userId lat lng time
                if isDriver then
                    let! currentJob = getCurrentJob userId
                    match currentJob with
                    | Some { ClientId = clientId } ->
                        let msg = LocationUpdate (lat, lng, time)
                        do! sendMessageToUser clientId msg
                    | _ ->
                        ()
            }

        let handleMessage (conf : Settings) (socketId : Guid) (msg : string) =
            task {
                let msg = Message.decodeMessage msg
                match msg with
                | Ok (Authenticate token) -> do! authenticate conf socketId token
                | Ok (LocationUpdate (lat, lng, time)) ->
                    let socket = getSocketById socketId
                    match socket with
                    | Some { UserId = Some id; IsDriver = isDriver } ->
                        do! locationUpdate id isDriver lat lng time
                    | _ -> printfn "sending location update to non authenticated socket!"
                | Error ex -> printfn "msg decode error: %s" ex.Message
                | _ -> printf "can't handle this type of msg: %A" msg
            }

    module Middleware =
        open Microsoft.AspNetCore.Builder
        open Microsoft.AspNetCore.Http
        open SocketHandlers
        open MessageHandlers

        type WebSocketMiddleware(next : RequestDelegate) =
            member _.Invoke(ctx : HttpContext) =
                task {
                    if ctx.Request.Path = PathString("/ws") then
                        if ctx.WebSockets.IsWebSocketRequest then
                            try
                                let! socket = ctx.WebSockets.AcceptWebSocketAsync()
                                let! connId = handleConnected socket

                                let handleReceived = ctx.GetSettings() |> handleMessage

                                let buffer = Array.zeroCreate<byte> 4096
                                let segment = new ArraySegment<byte>(buffer)

                                while socket.State = WebSocketState.Open do
                                    let! result = socket.ReceiveAsync(segment, CancellationToken.None)

                                    match result.MessageType with
                                    | WebSocketMessageType.Text ->
                                        do! Encoding.UTF8.GetString buffer |> handleReceived connId
                                        Array.Clear(buffer, 0, 4096)
                                    | WebSocketMessageType.Close -> do! handleDisconnected connId
                                    | _ -> ()
                            with
                            | ex -> printf "socket exception: %s" ex.Message
                        else
                            ctx.Response.StatusCode <- 400
                    else
                        return! next.Invoke(ctx)
                }

        type IApplicationBuilder with
            member this.UseWebSocketHandler () =
                this.UseWebSockets()
                    .UseMiddleware<WebSocketMiddleware>()
