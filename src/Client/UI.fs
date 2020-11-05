module Index

open System
open Elmish
open Fable.Remoting.Client
open Fulma
open Shared.GameOfLifeTypes
open Shared
open Shared.Utils

type Model =
    { Grid: CellGrid
      ElapsedTicks: int
      NewGridSize: int }

type Msg =
    | Tick
    | Restart
    | SetNewGridSize of int

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let initWithSize size : Model =
    let model = GetRandomCellGrid size
    let grid = ToCellGrid model size
    let model = { Grid = grid; ElapsedTicks = 0; NewGridSize = size }
    model
let init (): Model =
    initWithSize 10



let update (msg: Msg) (model: Model): Model =
    let newTicks = model.ElapsedTicks + 1
    match msg with
    | Tick ->
        { model with
              Grid = CalculateTick model.Grid
              ElapsedTicks = newTicks }
    | Restart -> initWithSize model.NewGridSize
    | SetNewGridSize value ->
        initWithSize value

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [] [
        img [ Src "/favicon.png"; Alt "FSSF" ]
        h1 [] [ str "Game of Life FSSF" ]
    ]



let renderView (cells: Cell list) x y =
    let cell =
        List.find (fun (c: Cell) -> (snd c).X = x && (snd c).Y = y) cells

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
                Column.column [ Column.Width(Screen.All, Column.Is8) ] [

                    generateGrid model
                ]
                Column.column [ Column.Width(Screen.All, Column.Is2) ] [
                    Button.button [ Button.Size IsLarge
                                    Button.Color IsInfo
                                    Button.OnClick(fun _ -> dispatch Tick) ] [
                        str "Dispatch next Tick"
                    ]
                    Button.button [ Button.Size IsLarge
                                    Button.Color IsGrey
                                    Button.Disabled true ] [
                        str ("Elapsed Ticks : " + (string model.ElapsedTicks))
                    ]

                    Button.button [ Button.Size IsLarge
                                    Button.Color IsDanger
                                    Button.OnClick(fun _ -> dispatch Restart

                                        ) ] [
                        str "Restart game"
                    ]
                    br []
                    div [] [
                        h1 [ Style[FontSize 25
                                   Color "black"
                                   ]] [ str "Set the grid size:" ]
                        input [ Class "input"
                                Value model.NewGridSize
                                Type "number"
                                OnInput(fun ev ->
                                    let intVal = int(ev.Value)
                                    dispatch (SetNewGridSize intVal))
                                    ]
                    ]
                ]
            ]
        ]
    ]
