module App

open Elmish
open Elmish.React
open Browser
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
|> Program.withSubscription (fun model -> Cmd.ofSub(fun dispatch -> window.setInterval((fun _ -> dispatch Tick), 1000) |> ignore))
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
