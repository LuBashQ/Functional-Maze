(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System.Collections.Generic
open Gfx
exception InvalidInsertionException 


/// <summary>
/// Type Cell
/// </summary>
/// <param name="_x">Cell abscissa</param>
/// <param name="_y">Cell ordinate</param>
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
    /// Check and return the possible cell neighbors
    /// </summary>
    /// <param name="maze">Cell matrix</param>
    /// <returns>A Cell list containing cell neighbors</returns>
    member this.neighbors (maze: Cell[,]) =
        [   
            if checkMatrixBounds (this.x - 2, this.y,this.mazeW,this.mazeH) then maze.[this.x - 2, this.y]
            if checkMatrixBounds (this.x + 2, this.y,this.mazeW,this.mazeH) then maze.[this.x + 2, this.y]
            if checkMatrixBounds (this.x, this.y - 2,this.mazeW,this.mazeH) then maze.[this.x, this.y - 2]
            if checkMatrixBounds (this.x, this.y + 2,this.mazeW,this.mazeH) then maze.[this.x, this.y + 2]
        ] |> List.filter (fun cell -> not cell.isVisited)
    
    /// <summary>
    /// Check and return the possible cell children (that are the non-wall near cells)
    /// </summary>
    /// <param name="maze">Cell matrix</param>
    /// <param name="parent">Parent cell</param>
    /// <returns>A Cell list containing cell son</returns>
    member this.children (maze: Cell[,], parent: Cell) =
        [   
            if checkMatrixBounds (this.x - 1, this.y,this.mazeW,this.mazeH) then maze.[this.x - 1, this.y]
            if checkMatrixBounds (this.x + 1, this.y,this.mazeW,this.mazeH) then maze.[this.x + 1, this.y]
            if checkMatrixBounds (this.x, this.y - 1,this.mazeW,this.mazeH) then maze.[this.x, this.y - 1]
            if checkMatrixBounds (this.x, this.y + 1,this.mazeW,this.mazeH) then maze.[this.x, this.y + 1]
        ] |> List.filter (fun cell -> cell.isVisited && cell <> parent && not cell.isDeadEnd)


/// <summary>
/// Type Maze
/// </summary>
/// <param name="width">Maze with</param>
/// <param name="heigth">Maze height</param>
type Maze (width, height) =
    
    let rnd = rnd_int 1 (width - 1)
    
    ///<summary>Return a valid column for the creation of the exit</summary>
    let yAxisPosition = if  rnd % 2 <> 0 then rnd else rnd - 1

    member val width = width with get
    member val height = height with get
    member val maze = Array2D.init width height (fun x y -> Cell (x,y,width,height)) with get,set
    member val pixelMap = Array2D.init width height (fun x y -> pixel.empty)
    member this.start = this.maze.[1,1]
    member this.finish = this.maze.[width - 1,yAxisPosition]
    member val player = Cell(1,1,width,height) with get,set


    /// <summary>
    /// Convert a cell matrix into array of CharInfo
    /// </summary>
    /// <returns>An array of CharInfo</returns>
    member this.convertToPixel () =
        for x = 0 to (width-1) do
            for y = 0 to (height-1) do
                if this.maze.[x,y].isVisited then
                    if this.maze.[x,y].isPath then 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkMagenta),x,y)
                else 
                    this.pixelMap.SetValue (pixel.create('\219',Color.White),x,y)
        
        toArray this.pixelMap
    

    member this.isInArea (diameter:int) =
        let radius = diameter/2
        let x,y = this.player.x,this.player.y
        
        let area = [
            for i = -radius+1 to radius-1 do
                for j = -radius+1 to radius-1 do
                    if checkMatrixBounds(y+j,x+i,this.width,this.height) then
                        yield this.maze.[y+j,x+i]
        ]

        let grayArea = [
            for i = -radius to radius do
                for j = -radius to radius do
                    if checkMatrixBounds(y+j,x+i,this.width,this.height) then
                        yield this.maze.[y+j,x+i]
        ]

        let darkArea = [
            for i = -radius-1 to radius+1 do
                for j = -radius-1 to radius+1 do
                    if checkMatrixBounds(y+j,x+i,this.width,this.height) then
                        yield this.maze.[y+j,x+i]
        ]



        let rec aux ls acc =
            match ls with
            | [] -> acc
            | x::xs -> aux xs (List.filter ((<>) x) acc)
        
        let a = aux area grayArea

        area,a,aux a darkArea
        

    member this.convertAreaToPixel (diameter:int) =
        let area,grayArea,darkArea = this.isInArea diameter
        for x = 0 to (width-1) do
            for y = 0 to (height-1) do
                if List.exists ((=) this.maze.[x,y]) area then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create('\219',Color.DarkMagenta),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create('\219',Color.White),x,y)
                else if List.exists ((=) this.maze.[x,y]) grayArea then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create('\219',Color.DarkMagenta),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create('\219',Color.Gray),x,y)
                else if List.exists ((=) this.maze.[x,y]) darkArea then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create('\219',Color.DarkMagenta),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkGray),x,y)
                
                else
                        this.pixelMap.SetValue (pixel.create('\219',Color.Black),x,y)
                
                
        
        toArray this.pixelMap

    /// <summary>
    /// "recursive backtracker" algorithm implementation, take from
    ///  https://en.wikipedia.org/wiki/Maze_generation_algorithm
    /// </summary>
    /// <exception cref="InvalidInsertionException">
    /// It is raised when current and next cell has neither abscissa nor ordinate in common
    /// </exception>
    member private this.initialize () =                                                 
        let stack = new Stack<Cell>()
        stack.Push(this.start)
        while stack.Count > 0 do
            let current = stack.Pop()
            this.maze.[current.x,current.y].isVisited <- true
            let n = current.neighbors (this.maze)
            if n.Length > 0 then
                stack.Push(current)
                let next = n.[rnd_int 0 (n.Length - 1)]
                this.maze.[next.x,next.y].isVisited <- true
                stack.Push(next)
                if current.x = next.x then
                    if current.y = next.y + 2 then                                      
                        this.maze.[current.x,current.y - 1].isVisited <- true
                    else
                        this.maze.[current.x,current.y + 1].isVisited <- true
                else if current.y = next.y then 
                    if current.x = next.x + 2 then                                      
                        this.maze.[current.x - 1,current.y].isVisited <- true
                    else
                        this.maze.[current.x + 1,current.y].isVisited <- true
                 else
                    raise InvalidInsertionException


    /// <summary>
    /// Convert a pixel array into a sprite
    /// </summary>
    /// <returns>A sprite containing the maze</returns>
    /// <seealso cref="this.convertToPixel"/>
    member this.toSprite (t:int,?diameter:int) =
        match t with
        | 0 -> sprite(image(width,height,this.convertToPixel ()),0,0,0)
        | _ -> sprite(image(width,height,this.convertAreaToPixel diameter.Value),0,0,0)

    
    /// <summary>
    /// Generete and set maze entry and exit
    /// </summary>
    /// <seealso cref="this.initialize"/>
    member this.generate () =
        this.initialize ()
        this.maze.[this.finish.x,this.finish.y].isExit <- true
        this.maze.[this.finish.x,this.finish.y].isVisited <- true
