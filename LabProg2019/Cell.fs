module LabProg2019.Cell

/// <summary>
/// Type Cell
/// </summary>
/// <param name="_x">Cell x-coordinate</param>
/// <param name="_y">Cell y-coordinate</param>
/// <param name="_mazeW">Maze width</param>
/// <param name="_mazeH">Maze height</param>
type Cell (_x: int, _y: int, _mazeW: int, _mazeH: int) =
    member val x = _x with get,set
    member val y = _y with get,set
    member val mazeW = _mazeW
    member val mazeH = _mazeH
    member val isVisited = false with get,set
    member val isExit = false with get,set
    member val isPath = false with get,set
    member val isDeadEnd = false with get,set

    /// <summary>
    /// Checks and return the possible cell neighbours
    /// </summary>
    /// <param name="maze">Cell matrix</param>
    /// <returns>A Cell list containing cell neighbours</returns>
    member this.neighbors (maze: Cell[,]) : Cell list =
        [   
            if checkMatrixBounds (this.x - 2, this.y,this.mazeW,this.mazeH) then maze.[this.x - 2, this.y]
            if checkMatrixBounds (this.x + 2, this.y,this.mazeW,this.mazeH) then maze.[this.x + 2, this.y]
            if checkMatrixBounds (this.x, this.y - 2,this.mazeW,this.mazeH) then maze.[this.x, this.y - 2]
            if checkMatrixBounds (this.x, this.y + 2,this.mazeW,this.mazeH) then maze.[this.x, this.y + 2]
        ] |> List.filter (fun cell -> not cell.isVisited)
    
    /// <summary>
    /// Checks and return the possible cell children (non-wall neighbouring cells)
    /// </summary>
    /// <param name="maze">Cell matrix</param>
    /// <param name="parent">Parent cell</param>
    /// <returns>A Cell list containing the cell's sons</returns>
    member this.children (maze: Cell[,], parent: Cell) : Cell list =
        [   
            if checkMatrixBounds (this.x - 1, this.y,this.mazeW,this.mazeH) then maze.[this.x - 1, this.y]
            if checkMatrixBounds (this.x + 1, this.y,this.mazeW,this.mazeH) then maze.[this.x + 1, this.y]
            if checkMatrixBounds (this.x, this.y - 1,this.mazeW,this.mazeH) then maze.[this.x, this.y - 1]
            if checkMatrixBounds (this.x, this.y + 1,this.mazeW,this.mazeH) then maze.[this.x, this.y + 1]
        ] |> List.filter (fun cell -> cell.isVisited && cell <> parent && not cell.isDeadEnd)

