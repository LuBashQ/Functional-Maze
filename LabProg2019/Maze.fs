(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System.Collections.Generic
open External
open Gfx
open Engine
open System
open System.Text



type Cell (_x: int, _y: int, _mazeW: int, _mazeH: int) =
    member val x = _x with get,set
    member val y = _y with get,set
    member val mazeW = _mazeW
    member val mazeH = _mazeH
    member val isVisited = false with get,set
    member val isEntrance = false with get,set
    member val isExit = false with get,set
    member val isPath = false with get,set
    member val isDeadEnd = false with get,set

    member this.neighbors (maze: Cell[,]) =
        [   
            if checkMatrixBounds (this.x - 2, this.y,this.mazeW,this.mazeH) then maze.[this.x - 2, this.y]
            if checkMatrixBounds (this.x + 2, this.y,this.mazeW,this.mazeH) then maze.[this.x + 2, this.y]
            if checkMatrixBounds (this.x, this.y - 2,this.mazeW,this.mazeH) then maze.[this.x, this.y - 2]
            if checkMatrixBounds (this.x, this.y + 2,this.mazeW,this.mazeH) then maze.[this.x, this.y + 2]
        ] |> List.filter (fun cell -> not cell.isVisited)

    member this.children (maze: Cell[,], parent: Cell) =
        [   
            if checkMatrixBounds (this.x - 1, this.y,this.mazeW,this.mazeH) then maze.[this.x - 1, this.y]
            if checkMatrixBounds (this.x + 1, this.y,this.mazeW,this.mazeH) then maze.[this.x + 1, this.y]
            if checkMatrixBounds (this.x, this.y - 1,this.mazeW,this.mazeH) then maze.[this.x, this.y - 1]
            if checkMatrixBounds (this.x, this.y + 1,this.mazeW,this.mazeH) then maze.[this.x, this.y + 1]
        ] |> List.filter (fun cell -> cell.isVisited && cell <> parent && not cell.isDeadEnd)

exception InvalidInsertionException 

type Maze (width, height) =
    
    let random = Random()
    let rnd = random.Next(1,width)
    let y = if  rnd % 2 <> 0 then rnd else rnd - 1
    let s,e = random.Next(4),random.Next(4)
    member val width = width with get
    member val height = height with get

    member val maze = Array2D.init width height (fun x y -> Cell (x,y,width,height)) with get,set
    member val pixelMap = Array2D.init width height (fun x y -> pixel.empty)
    
    member private this.getHole (n) =
        [   
            this.maze.[1,y]
            this.maze.[y,1]
            this.maze.[(height - 2), y]
            this.maze.[y,width - 2]
        ].[n]
    
    member this.start = this.getHole (s)
    member this.finish = this.getHole (e)

    member this.convertToPixel () =
        for x = 0 to (width-1) do
            for y = 0 to (height-1) do
                if this.maze.[x,y].isVisited then
                    if this.maze.[x,y].isEntrance then 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkCyan),x,y)
                    else if this.maze.[x,y].isExit then 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkYellow),x,y)
                    else if this.maze.[x,y].isPath then 
                        this.pixelMap.SetValue (pixel.create('\219',Color.DarkGreen),x,y)
                    else
                    this.pixelMap.SetValue (pixel.create('\219',Color.Black),x,y)
                else 
                    this.pixelMap.SetValue (pixel.create('\219',Color.White),x,y)
        
        toArray this.pixelMap
    
    member private this.initialize () =                                                 //Creation of maze
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
                    if current.y = next.y + 2 then                                      //next is on the left
                        this.maze.[current.x,current.y - 1].isVisited <- true
                    else
                        this.maze.[current.x,current.y + 1].isVisited <- true
                else if current.y = next.y then 
                    if current.x = next.x + 2 then                                      //next is down
                        this.maze.[current.x - 1,current.y].isVisited <- true
                    else
                        this.maze.[current.x + 1,current.y].isVisited <- true
                 else
                    raise InvalidInsertionException
    
    member this.generate () =
        this.initialize ()
        this.maze.[this.finish.x,this.finish.y].isVisited <- true
        this.maze.[this.finish.x,this.finish.y].isExit <- true
        this.maze.[this.start.x,this.start.y].isVisited <- true
        this.maze.[this.start.x,this.start.y].isEntrance <- true

        sprite(image(width,height,this.convertToPixel ()),0,0,0)

