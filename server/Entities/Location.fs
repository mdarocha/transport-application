namespace TransportApplication.Entities

open System

module Location =
    [<CLIMutable>]
    type Location =
        {
            Lat : double
            Lng : double
        }
