// #I sets where #load directives will be relative to. In this case Script1.fsx is in my Client directory
// so __SOURCE_DIRECTORY__ is also the source directory, and then I can load files in my Shared directories
// by using ..\Shared\ in the path.
#I __SOURCE_DIRECTORY__

#load @"..\Shared\Shared.fs"
#load @"..\Shared\GameOfLifeTypes.fs"
#load @"..\Shared\Utils.fs"

open System
open Shared.GameOfLifeTypes
open Shared
open Shared.Utils

type Model = { Grid: CellGrid }

type Msg = Tick of DateTime

let iter f (model:byref<Model>) =
    model <- f model
    model

let init (): Model =
    let model = GetRandomCellGrid 10
    let grid = ToCellGrid model 10
    let model = { Grid = grid }
    model

let update (msg: Msg) (model: Model): Model =
    match msg with
    | Tick _ ->
        { model with
              Grid = CalculateTick model.Grid.Cells }

let mutable model = init()
iter (update (Tick DateTime.Now)) &model
