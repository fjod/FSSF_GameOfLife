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
        window.setInterval(dispatch ,  1000) |> ignore
    Cmd.ofSub sub

Program.mkProgram Index.init Index.update Index.view
|> Program.withSubscription timer
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
