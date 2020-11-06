namespace Shared.GameOfLifeTypes
type Point =
    {
        X:int
        Y:int
    }

type CellStatus = //Discriminated Union
    | Dead
    | Alive
    | DeadOnNextTick

type Cell =    //Tuple
    CellStatus*Point

type CellGrid =  //Record
    { Cells: Cell [][]
      Size: int
      LowerBound : int
      UpperBound : int
      }




