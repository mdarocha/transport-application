namespace TransportApplication.Handlers

open System
open System.Collections.Generic
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open FSharp.Control.Tasks.V2.ContextInsensitive
open TransportApplication.Utils.Configuration

module Root =
    let private confDeclaration (conf : IDictionary<string, string>): string =
        conf
        |> Seq.map (|KeyValue|)
        |> Seq.map (fun (k,v) -> String.Format("window.{0}='{1}';", k, v))
        |> String.concat ""

    module View =
        open Giraffe.ViewEngine

        let index (assetsUrl : string) (conf : IDictionary<string, string>) =
            html [] [
                head [] [
                    title [] [ str "TransportApplication" ]
                    meta [
                        _name "viewport"
                        _content "width=device-width, initial-scale=1.0"
                    ]
                    link [
                        _rel "stylesheet"
                        _href "https://fonts.googleapis.com/css2?family=Open+Sans:ital@0;1&family=Pacifico&display=swap"
                    ]
                    script [] [ confDeclaration conf |> rawText ]
                ]
                body [] [
                    noscript [] [
                        span [] [ str "This site requires JavaScript to work. Please enable it" ]
                    ]
                    script [ _src <| String.Format("{0}/index.js", assetsUrl) ] []
                    script [ _src "https://use.fontawesome.com/b4bbf71aa6.js" ] []
                ]
            ]

        let render (ctx: HttpContext) =
            let env = ctx.GetHostingEnvironment()
            let isDev = env.IsDevelopment()

            let request = ctx.Request
            let baseUrl = String.Format("{0}://{1}{2}", request.Scheme, request.Host, request.PathBase)

            let assetsUrl =
                if isDev then
                    String.Format("{0}://{1}:1234", request.Scheme, request.Host.Host)
                else
                    baseUrl

            let conf = dict [
                "API_URL", String.Format("{0}/api", baseUrl);
                "WS_URL", String.Format("{0}://{1}{2}/ws", "ws", request.Host, request.PathBase);
                "MAPBOX_TOKEN", ctx.GetService<Settings>().MapboxToken
            ]

            index assetsUrl conf |> RenderView.AsBytes.htmlDocument

    let indexHandler (_next : HttpFunc) (ctx : HttpContext) : HttpFuncResult =
        task {
            ctx.SetStatusCode 200
            ctx.SetHttpHeader "Content-Type" "text/html;charset=utf-8"

            let html = View.render ctx
            return! ctx.WriteBytesAsync html
        }

    let handlers: HttpHandler =
        routeStartsWith "/" >=> indexHandler
