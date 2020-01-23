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
/// Tipo Cella
/// </summary>
/// <param name="_x">L'ascissa della cella</param>
/// <param name="_y">L'ordinata della cella</param>
/// <param name="_mazeW">La larghezza del labirinto</param>
/// <param name="_mazeH">L'altezza del labirinto</param>
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
    /// Controlla e ritorna i possibili vicini della cella
    /// </summary>
    /// <param name="maze">La matrice di celle in cui si trova</param>
    /// <returns>Una Cell list contenente i vicini della cella</returns>
    member this.neighbors (maze: Cell[,]) =
        [   
            if checkMatrixBounds (this.x - 2, this.y,this.mazeW,this.mazeH) then maze.[this.x - 2, this.y]
            if checkMatrixBounds (this.x + 2, this.y,this.mazeW,this.mazeH) then maze.[this.x + 2, this.y]
            if checkMatrixBounds (this.x, this.y - 2,this.mazeW,this.mazeH) then maze.[this.x, this.y - 2]
            if checkMatrixBounds (this.x, this.y + 2,this.mazeW,this.mazeH) then maze.[this.x, this.y + 2]
        ] |> List.filter (fun cell -> not cell.isVisited)
    
    /// <summary>
    /// Controlla e ritorna i possibili figli della cella (ovvero le celle non-muro vicine)
    /// </summary>
    /// <param name="maze">La matrice di celle in cui si trova</param>
    /// <param name="parent">La cella padre</param>
    /// <returns>Una Cell list contenente i figli della cella</returns>
    member this.children (maze: Cell[,], parent: Cell) =
        [   
            if checkMatrixBounds (this.x - 1, this.y,this.mazeW,this.mazeH) then maze.[this.x - 1, this.y]
            if checkMatrixBounds (this.x + 1, this.y,this.mazeW,this.mazeH) then maze.[this.x + 1, this.y]
            if checkMatrixBounds (this.x, this.y - 1,this.mazeW,this.mazeH) then maze.[this.x, this.y - 1]
            if checkMatrixBounds (this.x, this.y + 1,this.mazeW,this.mazeH) then maze.[this.x, this.y + 1]
        ] |> List.filter (fun cell -> cell.isVisited && cell <> parent && not cell.isDeadEnd)


/// <summary>
/// Tipo Maze
/// </summary>
/// <param name="width">La larghezza del labirinto</param>
/// <param name="heigth">L'altezza del labirinto</param>
type Maze (width, height) =
    
    let rnd = rnd_int 1 (width - 1)
    
    ///<summary>Ricava una colonna disponibile per la creazione dell'uscita</summary>
    let yAxisPosition = if  rnd % 2 <> 0 then rnd else rnd - 1

    member val width = width with get
    member val height = height with get
    member val maze = Array2D.init width height (fun x y -> Cell (x,y,width,height)) with get,set
    member val pixelMap = Array2D.init width height (fun x y -> pixel.empty)
    member this.start = this.maze.[1,1]
    member this.finish = this.maze.[width - 1,yAxisPosition]


    /// <summary>
    /// Converte una matrice di Cell in un array di CharInfo
    /// </summary>
    /// <returns>Una un array di CharInfo</returns>
    member this.convertToPixel () =
        for x = 0 to (width-1) do
            for y = 0 to (height-1) do
                if this.maze.[x,y].isVisited then
                    if this.maze.[x,y].isPath then 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkMagenta),x,y)
                else 
                    this.pixelMap.SetValue (pixel.create('\219',Color.White),x,y)
        
        toArray this.pixelMap
    

    /// <summary>
    /// Implementazione dell'algoritmo "recursive backtracker", 
    /// preso da https://en.wikipedia.org/wiki/Maze_generation_algorithm
    /// </summary>
    /// <exception cref="InvalidInsertionException">Viene lanciata quando la cella corrente e quella successiva
    /// non hanno nè ascissa nè ordinata in comune
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
    /// Converte un array di pixel in uno sprite 
    /// </summary>
    /// <returns>Uno sprite contenente il labirinto</returns>
    /// <seealso cref="this.convertToPixel"/>
    member this.toSprite () =
        sprite(image(width,height,this.convertToPixel ()),0,0,0)
    
    /// <summary>
    /// Genera e imposta entrata e uscita del labirinto
    /// </summary>
    /// <seealso cref="this.initialize"/>
    member this.generate () =
        this.initialize ()
        this.maze.[this.finish.x,this.finish.y].isExit <- true
        this.maze.[this.finish.x,this.finish.y].isVisited <- true
