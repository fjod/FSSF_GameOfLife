namespace Shared.GameOfLifeTypes
type Point =
    {
        X:int
        Y:int
    }

type CellStatus = //Discriminated Union
    | Dead
    | Alive

type Cell =    //Tuple
    CellStatus*Point

type CellGrid =  //Record
    { Cells: Cell list
      Size: int }




type FigureName = //when user wants to start with some predefined figure
    |Glider
    |LWSS
    |Blinker
    |Toad

type StartFigure = //also we can start with randomized grid and see what happens
    |FigureName
    |RandomFigure


type Figure = { //record to store predefined type cell' coordinates and statuses
    FigureType : FigureName
    Cells : Cell list
}