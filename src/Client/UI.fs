module Index

open System
open Elmish
open Fable.Remoting.Client
open Fulma
open Shared.GameOfLifeTypes
open Shared
open Shared.Utils

type Model = { Grid: CellGrid }

type Msg = Tick

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init (): Model =
    let model = GetRandomCellGrid 10
    let grid = ToCellGrid model 10
    let model = { Grid = grid }
    model


let update (msg: Msg) (model: Model): Model =
    match msg with
    | Tick ->
        { model with
              Grid = CalculateTick model.Grid }

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [] [
        img [ Src "/favicon.png"; Alt "FSSF" ]
        h1 [] [ str "Game of Life FSSF" ]
    ]

let IsEqual (x: int) (y: int) = Math.Abs(x - y) = 0

let renderView (cells: Cell list) x y =
    let cell =
        List.find (fun (c: Cell) -> (snd c).X = x && (snd c).Y = y) cells
    //List.find (fun (c: Cell) -> IsEqual (snd c).X x && IsEqual (snd c).Y y) cells

    match fst cell with
    | Dead ->
        td [ Style [ Background "white"
                     FontSize 50 ] ] [
            str "***"
        ]
    | Alive ->
        td [ Style [ Background "black"
                     FontSize 50 ] ] [
            str " * "
        ]
    | DeadOnNextTick ->
        td [ Style [ Background "white"
                     FontSize 50 ] ] [
            str " * "
        ]

let generateGrid (model: Model) =
    let listOfCols = [ 0 .. model.Grid.Size - 1 ]
    let listOfCols2 = [ 0 .. model.Grid.Size - 1 ]

    table [ Style [] ] [
        thead [] [
            tr [] [
                yield th [] []
                for col in listOfCols -> th [] [ str (string col) ]
            ]
        ]
        tbody [] [
            for row in listOfCols ->
                tr [] [
                    yield th [] [ str (string row) ]
                    for col2 in listOfCols2 -> renderView model.Grid.Cells row col2
                ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary
                Hero.IsFullHeight ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [ navBrand ]
            ]
        ]
        Hero.body [] [
                Columns.columns [] [
                Column.column [  ] [

                    generateGrid model
                ]
                Column.column [ Column.Width(Screen.All, Column.Is2) ] [

                    Button.button [ Button.Size IsLarge
                                    Button.Color IsInfo
                                    Button.OnClick(fun _ -> dispatch Tick) ] [
                        str "Dispatch next Tick"
                    ]
                ]
            ]
        ]
    ]
