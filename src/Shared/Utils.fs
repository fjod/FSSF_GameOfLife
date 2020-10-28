module Shared.Utils


open Shared.GameOfLifeTypes

let CreatePoint (x:int) (y:int) : Point =
    {
     X = x
     Y = y
    }

let CreateCell x y isDead : Cell=
    if isDead then
        Dead, CreatePoint x y
    else
        Alive, CreatePoint x y

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
            let IsEqual (x: int) (y: int) = System.Math.Abs(x - y)= 0
            IsEqual (snd cell).X _x  && IsEqual (snd cell).Y _y

let GetNeighbourCells (cell:Cell) (cells: Cell list) : Option<Cell> list =

    //find cell around selected cells one by one; this method returns one cell next to the selected
    let findNeighbourCell x y  : Option<Cell> =

        let amount = System.Math.Sqrt (float cells.Length)

        match x, y with
            | (var1, var2) when var1 <0|| var2 < 0 -> None
            | (var1, var2) when var1>= (int amount) || var2 >= (int amount) -> None
            | _, _ -> Some (List.find (cellFinder x y) cells)

    let Row = [(snd cell).X-1..(snd cell).X+1]
    let Col = [(snd cell).Y-1..(snd cell).Y+1]
    seq {
        for i in Row do
            for j in Col do
                findNeighbourCell i j
    } |> List.ofSeq

let ChangeCellStateBasedOnNeighbours (cells: Option<Cell> list) (cell: Cell ) : Cell=
    let CellIsAlive _cell =
        match _cell with
        |Some _ -> true
        |None -> false

    let amountOfAliveCellsAround = List.filter CellIsAlive cells
    match amountOfAliveCellsAround.Length with
    | (count) when count > 1 || count < 4 -> (Alive, snd cell)
    | _  ->  (Dead, snd cell)

let CalculateTick (grid: CellGrid) : CellGrid =
//Any live cell with two or three live neighbours survives.
//Any dead cell with three live neighbours becomes a live cell.
//All other live cells die in the next generation. Similarly, all other dead cells stay dead.
    let amount = System.Math.Sqrt (float grid.Cells.Length)
    let invert ((status, coords): Cell) =
        match status with
        | Alive -> Dead, coords
        | Dead -> Alive, coords
    ToCellGrid (grid.Cells |> List.map invert) grid.Size
//    let newCells = seq{
//        for currentCell in cells do
//            let neighBours = GetNeighbourCells currentCell cells
//            ChangeCellStateBasedOnNeighbours neighBours currentCell
//    }
//    let gridCells = List.ofSeq newCells
//    let amount = System.Math.Sqrt (float cells.Length)
//    ToCellGrid gridCells (int amount)


