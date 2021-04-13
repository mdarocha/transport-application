namespace TransportApplication.Entities

open System

module User =
    [<CLIMutable>]
    type User =
        {
            Id : int
            Name : string
            Surname : string
            PhoneNumber : string
            Email : string
            PasswordHash : string
        }
