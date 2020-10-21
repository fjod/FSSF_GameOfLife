module Shared.Utils

open System.Drawing
open Shared.GameOfLifeTypes


let CreateCell x y isDead : Cell=
    if isDead then
        Dead, Point(x,y)
    else
        Alive, Point(x,y)

let GetRandomCellGrid size =
    let rnd = System.Random()
    seq{
        for j in 0.. size do
            for i in 0 .. size -> CreateCell i j (rnd.NextDouble() > 0.5)
     } |> List.ofSeq

let ToCellGrid cells size : CellGrid =
    {
        Cells = cells
        Size = size
    }


