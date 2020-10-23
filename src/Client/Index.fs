module Index

open System
open Elmish
open Fable.Remoting.Client
open Shared.GameOfLifeTypes
open Shared
open Shared.Utils

type Model = { Grid: CellGrid }

type Msg = Tick of DateTime

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init (): Model * Cmd<Msg> =
    let model = GetRandomCellGrid 50
    let grid = ToCellGrid model 50
    let model = { Grid = grid }
    (model, Cmd.ofMsg (Tick DateTime.Now))




let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Tick _ -> { model with Grid = CalculateTick model.Grid.Cells }, Cmd.none //here we recalculate state of cells



open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [] [
        img [ Src "/favicon.png"; Alt "FSSF" ]
        h3 [] [ str "Game of Life FSSF" ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary
                Hero.IsFullHeight ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [ navBrand ]
            ]
        ]
        Hero.body [] []
    ]
