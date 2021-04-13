open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Cors
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open TransportApplication.Handlers
open TransportApplication.Utils.Configuration
open TransportApplication.Utils.Authentication.Jwt
open TransportApplication.Handlers.WebSocket.Middleware

let routes =
    choose [
        subRoute "/api"
            (choose [
                subRoute "/auth" Authentication.handlers
                subRoute "/order" PlaceOrder.handlers
                subRoute "/job" Job.handlers
                subRoute "/tracker" Tracker.handlers
            ])
        Root.handlers
    ]

let errorHandler (isDev : bool) (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "Unhandled exception has occurred while executing request")
    clearResponse
    >=> ServerErrors.INTERNAL_ERROR (if isDev then ex.Message else "Internal server error occured")

let configureCors (conf : Settings) (builder : Infrastructure.CorsPolicyBuilder) =
    builder.AllowAnyHeader()
           .AllowAnyMethod()
           .WithOrigins(conf.Audience, conf.Issuer) |> ignore

let configureJwt (conf : Settings) (services : IServiceCollection) =
    services
        .AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
        .AddJwtBearer(fun opts ->
            opts.MapInboundClaims <- false
            opts.TokenValidationParameters <- tokenValidationParams conf)
    |> ignore

let configureApp (ctx : WebHostBuilderContext) (app: IApplicationBuilder) =
    let isDev = ctx.HostingEnvironment.IsDevelopment()

    app.UseCors()
       .UseWebSocketHandler()
       .UseAuthentication()
       .UseStaticFiles()
       .UseGiraffeErrorHandler(errorHandler isDev)
       .UseGiraffe(routes) |> ignore

let configureServices (services: IServiceCollection) =
    let settings = services.GetSettings()

    configureJwt settings services

    services
        .AddSettings(settings)
        .AddCors(fun opts -> opts.AddDefaultPolicy(configureCors settings))
        .AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    let filter (l : LogLevel) = l >= LogLevel.Debug
    builder.AddFilter(filter)
           .AddConsole()
           .AddDebug()
    |> ignore

let configureWebHost (builder : IWebHostBuilder) =
    builder
        .Configure(configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        |> ignore

[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(Action<IWebHostBuilder> configureWebHost)
        .Build()
        .Run()
    0
