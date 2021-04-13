namespace TransportApplication.Entities

open System

module Place =
    [<CLIMutable>]
    type Place =
        {
            Id : int
            FriendlyName : string
            AdditionalDescription : string
            Address : string
            Latitude : double
            Longitude : double
        }

    [<CLIMutable>]
    type GeocodedAddress =
        {
            Address : string
            Latitude : double
            Longitude : double
        }

