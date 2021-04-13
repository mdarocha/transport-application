namespace TransportApplication.Entities

open System
open TransportApplication.Entities.Place

module Order =
    type OrderStatusType
        = SearchingForDriver = 0
        | Delivering = 1
        | Completed = 2
        | Failed = 3

    [<CLIMutable>]
    type Order =
        {
            Id : int
            From : Place
            To : Place
            Status : OrderStatusType
        }

    type Job =
        {
            Id : int
            ClientId : int
            From : Place
            To : Place
            StartTime : int64
            RouteDistanceMeters : double
            BeginDistanceMeters : double
            Status : OrderStatusType
        }
