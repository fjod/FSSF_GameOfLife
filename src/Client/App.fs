module App

open Elmish
open Elmish.React
open Browser

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let timer initial =
    let sub dispatch =
        window.setInterval((fun _ -> dispatch Index.Tick), 3000) |> ignore
    Cmd.ofSub sub



Program.mkSimple Index.init Index.update Index.view
|> Program.withSubscription timer
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
