module Shared.Utils


open Shared.GameOfLifeTypes

let CreatePoint (x: int) (y: int): Point = { X = x; Y = y }

let CreateCell x y isDead: Cell =
    if isDead then Dead, CreatePoint x y else Alive, CreatePoint x y


let CreateRandomCell x y (rnd: System.Random): Cell =
    let isAlive = rnd.NextDouble() > 0.5
    CreateCell x y isAlive

//create random grid which is bigger than actual field
//https://math.stackexchange.com/questions/1699282/conways-game-of-life-borders-rules
let GetRandomCellGrid size =
    let correctSize = (size - 1) * 2
    let invertSize = (size - 1) * -1
    let rnd = System.Random()
    seq {
        for j in invertSize .. correctSize do
            for i in invertSize .. correctSize -> CreateRandomCell i j rnd
    }
    |> List.ofSeq

let ToCellGrid cells size: CellGrid =
    { Cells = cells
      Size = size
      UpperBound = (size - 1) * 2
      LowerBound = (size - 1) * -1 }

let cellFinder _x _y (cell: Cell) = (snd cell).X = _x && (snd cell).Y = _y

let GetNeighbourCells (cell: Cell) (cells: CellGrid): Option<Cell> list =

    //find cell around selected cells one by one; this method returns one cell next to the selected
    let findNeighbourCell (p: Point): Option<Cell> =

        match p.X, p.Y with
        | (var1, var2) when var1 < cells.LowerBound || var2 < cells.LowerBound -> None
        | (var1, var2) when var1
                            >= cells.UpperBound
                            || var2 >= cells.UpperBound -> None
        | _, _ -> Some(List.find (cellFinder p.X p.Y) cells.Cells)

    let Row = [ (snd cell).X - 1 .. (snd cell).X + 1 ]
    let Col = [ (snd cell).Y - 1 .. (snd cell).Y + 1 ]

    seq {
        for i in Row do
            for j in Col do
                CreatePoint i j
    }
    |> List.ofSeq
    |> List.except [ CreatePoint (snd cell).X (snd cell).Y ]
    |> List.map findNeighbourCell


let ChangeCellStateBasedOnNeighbours (cells: Option<Cell> list) (cell: Cell): Cell =
    let CellIsAlive _cell =
        match _cell with
        | Some _ -> true
        | None -> false

    //Any live cell with two or three live neighbours survives.
    //Any dead cell with three live neighbours becomes a live cell.
    //All other live cells die in the next generation. Similarly, all other dead cells stay dead.

    let amountOfAliveCellsAround =
        List.filter CellIsAlive cells
        |> List.filter (fun cell -> fst cell.Value = Alive)

    match amountOfAliveCellsAround.Length, fst cell with
    | (count, Alive) when count > 1 && count < 4 -> (Alive, snd cell)
    | (count, Dead) when count = 3 -> (Alive, snd cell)
    | (_, Alive) -> (DeadOnNextTick, snd cell)
    | _ -> (Dead, snd cell)

let testCalc (grid: CellGrid) =
    seq {
        for currentCell in grid.Cells do
            match fst currentCell with
            | DeadOnNextTick -> (Dead, snd currentCell)
            | _ ->
                let neighBours = GetNeighbourCells currentCell grid
                ChangeCellStateBasedOnNeighbours neighBours currentCell
    }

let CalculateTick (grid: CellGrid): CellGrid =
    let gridCells = List.ofSeq (testCalc grid)
    ToCellGrid gridCells grid.Size
