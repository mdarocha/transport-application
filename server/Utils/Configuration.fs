namespace TransportApplication.Utils

open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Giraffe

module Configuration =
    [<CLIMutable>]
    type Settings =
        {
            ConnectionString : string
            JwtSecret : string
            Audience : string
            Issuer : string
            MapboxToken : string
        }

    let mutable GlobalConnectionString = ""

    type HttpContext with
        member this.GetSettings (): Settings =
            this.GetService<Settings>()

    type IServiceCollection with
        member this.GetSettings (): Settings =
            let sp = this.BuildServiceProvider()
            let conf = sp.GetService<IConfiguration>()

            let settings: Settings = {
                    ConnectionString = ""
                    JwtSecret = ""
                    Audience = ""
                    Issuer = ""
                    MapboxToken = ""
                }

            ConfigurationBinder.Bind(conf, settings)
            GlobalConnectionString <- settings.ConnectionString

            if String.length settings.JwtSecret < 16 then
                failwith "JwtSecret conf is too short. To generate a key, at least 16 characters is required"
            else
                settings

        member this.AddSettings (settings : Settings): IServiceCollection =
            this.AddSingleton<Settings>(settings)
