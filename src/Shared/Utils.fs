﻿module Shared.Utils

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

let cellFinder _x _y (cell : Cell) =
            System.Math.Abs((snd cell).X-  _x) < 1 && System.Math.Abs((snd cell).Y-  _y) < 1

let GetNeighbourCells (cell:Cell) (cells: Cell list) : Option<Cell> list =

    //find cell around selected cells one by one; this method returns one cell next to the selected
    let findNeighbourCell x y  : Option<Cell> =
        let ret = List.find (cellFinder x y) cells
        let amount = List.length cells / 2
        match x, y with
            | (var1, var2) when var1<=0 || var2 <= 0 -> None
            | (var1, var2) when var1>= amount || var2 >= amount -> None
            | _, _ -> Some ret

    let Row = [(snd cell).X-1..(snd cell).X+1]
    let Col = [(snd cell).Y-1..(snd cell).Y+1]
    seq {
        for i in Row do
            for j in Col do
                findNeighbourCell i j
    } |> List.ofSeq


