module LabProg2019.Maze
open System.Collections.Generic
open Gfx
open Cell
exception InvalidInsertionException 


/// <summary>
/// Type Maze
/// </summary>
/// <param name="width">Maze width</param>
/// <param name="heigth">Maze height</param>
type Maze (_width: int, _height: int) =
    
    let rnd = rnd_int 1 (_width - 1)
    
    ///<summary>Returns a valid column for the creation of the exit</summary>
    let yAxisPosition = if  rnd % 2 <> 0 then rnd else rnd - 1

    member val width = _width with get
    member val height = _height with get
    member val maze = Array2D.init _width _height (fun x y -> Cell (x,y,_width,_height)) with get,set
    member val pixelMap = Array2D.init _width _height (fun x y -> pixel.empty)
    member this.start = this.maze.[1,1]
    member this.finish = this.maze.[_width - 1,yAxisPosition]
    member val player = Cell(1,1,_width,_height) with get,set


    /// <summary>
    /// Converts a cell matrix into array of CharInfo
    /// </summary>
    /// <returns>An array of CharInfo</returns>
    member this.convertToPixel () : External.CharInfo[] =
        for x = 0 to (_width-1) do
            for y = 0 to (_height-1) do
                if this.maze.[x,y].isVisited then
                    if this.maze.[x,y].isPath then 
                        this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.path_color),x,y)
                else 
                    this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.wall_color),x,y)
        
        toArray this.pixelMap
    
    /// <summary>
    /// Creates the area of visibility during an hardore game
    /// </summary>
    /// <param name="visibility">The area of visibility</param>
    /// <returns>A tuple representing the directly visible area, the slighty and almost completely out of sight cells</returns>
    member private this.getAreas (visibility:int) : (Cell list * Cell list * Cell list) =
        let radius = visibility/2
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
        
        let grey = aux area grayArea
        let dark = aux grey darkArea
        
        area,grey,dark

    /// <summary>
    /// Converts a cell matrix into array of CharInfo, based on a certain visibility
    /// </summary>
    /// <param name="visibility">The area of visibility</param>
    /// <returns>An array of CharInfo</returns>
    member this.convertAreaToPixel (visibility: int) : External.CharInfo[] =
        let area,grayArea,darkArea = this.getAreas visibility
        for x = 0 to (_width-1) do
            for y = 0 to (_height-1) do
                if List.exists ((=) this.maze.[x,y]) area then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.road_color),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.wall_color),x,y)
                else if List.exists ((=) this.maze.[x,y]) grayArea then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.road_color),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.soft_dark_area),x,y)
                else if List.exists ((=) this.maze.[x,y]) darkArea then
                    if this.maze.[x,y].isVisited then
                        if this.maze.[x,y].isPath then 
                            this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.road_color),x,y)
                    else 
                        this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.dark_area),x,y)
                
                else
                        this.pixelMap.SetValue (pixel.create(Config.filled_pixel_char,Config.road_color),x,y)
                
        toArray this.pixelMap

    /// <summary>
    /// "recursive backtracker" algorithm implementation, take from
    ///  https://en.wikipedia.org/wiki/Maze_generation_algorithm
    /// </summary>
    /// <exception cref="InvalidInsertionException">
    /// It is raised when current and next cell has neither abscissa nor ordinate in common
    /// </exception>
    member private this.initialize () : unit =                                                 
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
    /// Converts a pixel array into a sprite
    /// </summary>
    /// <param name="t">The index of the operation</param>
    /// <param name="visibility">The visibility of the game</param>
    /// <returns>A sprite containing the maze</returns>
    member this.toSprite (t: int,?visibility: int) : sprite =
        match t with
        | 0 -> sprite(image(_width,_height,this.convertToPixel ()),0,0,0)
        | _ -> sprite(image(_width,_height,this.convertAreaToPixel visibility.Value),0,0,0)

    
    /// <summary>
    /// Generetes and set maze entry and exit
    /// </summary>
    member this.generate () : unit =
        this.initialize ()
        this.maze.[this.finish.x,this.finish.y].isExit <- true
        this.maze.[this.finish.x,this.finish.y].isVisited <- true
