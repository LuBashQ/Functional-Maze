﻿module LabProg2019.Scene
open Gfx
open System
open Engine
open Actions
open Maze
exception EmptyQueueException

/// <summary>
/// Type SceneManager
/// </summary>
/// <param name="_gameStates">Game supported states</param>
/// <param name="_engine">Game engine</param>
type SceneManager (_gameStates: State list, _engine: engine) =

    member val gameStates = _gameStates with get,set
    member val currentScene = None with get,set
    member val engine = _engine with get


    /// <summary>
    /// Reset a state
    /// </summary>
    /// <param name="s">Current state</param>
    /// <returns>A new default state</returns>
    member private this.resetState s =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,size,v) -> Game(n,Some player,None,mv,None,size,v)
        | _ -> s

    /// <summary>
    /// Change maze size
    /// </summary>
    /// <param name="s">Current state</param>
    /// <param name="size">New size</param>
    /// <param name="pos">Cursor position in the menu</param>
    /// <returns>New state with modified size</returns>
    member private this.setSize (s,size,?pos) =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,_,v) -> Game(n,Some player,None,mv,None,(size,size),v)
        | Menu(n,bg,mv,ls,a,size) -> Menu(n,bg,mv,ls,pos.Value,size)
        | _ -> s
    
    /// <summary>
    /// Change the radius of visible maze (in hard mode)
    /// </summary>
    /// <param name="s">Current status</param>
    /// <param name="visibility"> TO-----------------------------------------------------------MODIFY </param>
    /// <param name="pos">Position of the sprite</param>
    /// <returns>A new state, that comes from the changes on the actual state</returns>
    member private this.setVisiblity (s,visibility,?pos) =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,s,v) -> Game(n,Some player,None,mv,None,s,visibility)
        | Menu(n,bg,mv,ls,a,size) -> Menu(n,bg,mv,ls,pos.Value,size)
        | _ -> s

    /// <summary>
    /// Change the visibility of all game states
    /// </summary>
    /// <param name="v">TO----------------------------------------------------------------------------MODIFY</param>
    /// <param name="pos">Position of the sprite</param>
    member private this.setGameVisibility v pos =
        this.gameStates <- List.map(fun (s:State) -> this.setVisiblity (s,v,pos)) this.gameStates 
   
   /// <summary>
    /// Set new size to all game states
    /// </summary>
    /// <param name="size">New size</param>
    /// <param name="pos">New cursor position in the menu</param>
    member private this.setGameSize size pos =
        this.gameStates <- List.map(fun (s:State) -> this.setSize (s,size,pos)) this.gameStates 

    /// <summary>
    /// Reset all game states
    /// </summary>
    member private this.resetGame () =
        this.gameStates <- List.map(fun (s:State) -> this.resetState s) this.gameStates 

    /// <summary>
    /// Search a state in the game states list
    /// </summary>
    /// <param name="name">State name</param>
    /// <returns>A list with the requested state</returns>
    member this.getScene name =
        List.find (fun (s:State) -> s.name = name) this.gameStates
    
    /// <summary>
    /// Remove a state form the game states list 
    /// </summary>
    /// <param name="name">The state name</param>
    /// <returns>A list with all game states without the removed one</returns>
    member this.deleteScene name =
        List.filter (fun (s:State) -> s.name <> name) this.gameStates

    /// <summary>
    /// Set the current state
    /// </summary>
    /// <param name="name">State name</param>
    member private this.setCurrentScene name =
        this.currentScene <- Some (this.getScene name)
    
    /// <summary>
    /// Check for the if exists a state in the game states list
    /// </summary>
    /// <param name="name">State name</param>
    /// <returns>A boolean that express the research result</returns>
    member this.isPresent name =
        List.exists (fun (e:State) -> e.name = name) this.gameStates

    
    /// <summary>
    /// Add a new state to game states list
    /// </summary>
    /// <param name="name">State name</param>
    /// <param name="move">The function to execute at every cycle of execution (Game only)</param>
    /// <param name="game">Value that express if state is Game or Menu</param>
    /// <param name="bg">Background state</param>
    /// <param name="pg">Player state</param>
    /// <param name="name">State name</param>
    /// <param name="maze">Data structure representing the maze</param>
    /// <param name="voices">Menu elements</param>
    /// <param name="active">Cursor position in menu</param>
    /// <param name="menu">The function to execute at every cycle of execution (Menu only)</param>
    /// <param name="size">Window or maze size</param>
    member private this.addScene (n:string, move: (State->ConsoleKeyInfo->State), 
        game: bool, ?bg:sprite, ?pg:sprite, ?maze: Maze, ?voices: string list, ?active: int, 
        ?menu:(State->ConsoleKeyInfo->wronly_raster->string list->(int*int)->State), ?size: int*int, ?visibility: int) : unit =
        
        let state = 
            match game with
            | true -> Game(n,pg,bg,move,maze,size.Value,visibility.Value)
            | _ -> Menu(n,bg,menu.Value,voices.Value,active.Value,size.Value)

        this.gameStates <- List.append this.gameStates [state]


    /// <summary>
    /// Change the current scene
    /// </summary>
    /// <param name="name">State name</param>
    member this.changeScene (name,?wr) =
        this.setCurrentScene name
        
        //clear all sprites
        this.engine.removeAll ()
        match this.currentScene.Value with
        | Game(pg = p) -> 
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo()))
            this.engine.register_sprite p.Value
            this.engine.register_sprite this.currentScene.Value.background
        | Menu(_,bg,_,t,_,_) -> 
            if bg <> None then
                this.engine.register_sprite bg.Value; 
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo(),wr.Value,t))
        | Text(_,bg,mv,vs,_,_,_) -> 
            if bg <> None then
                this.engine.register_sprite bg.Value
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo(),wr.Value,vs))


    /// <summary>
    /// Check the pressed key and execute the corresponding action 
    /// </summary>
    /// <param name="engine">Game engine</param>
    /// <param name="key">Pressed key</param>
    /// <param name="wr">Write only raster used for screen writing</param>
    member this.execute (engine: engine) (key:ConsoleKeyInfo option) (wr: wronly_raster) = 
        
        if this.currentScene.IsNone then 
            this.changeScene ("help",wr)  //in order to start the program with help tab
        else
            match key.Value.Key with
                | ConsoleKey.R -> match this.currentScene.Value with
                                    | Game(_,p,_,_,m,s,v) -> 
                                        if this.isPresent "solve" then ()
                                        else
                                            this.addScene ("solve",showSolution,true,pg=p.Value,maze=m.Value,size=s,visibility=v)
                                            this.changeScene ("solve",wr)
                                            this.gameStates <- this.deleteScene "solve"
                                    | Menu(_,_,_,t,_,_) -> this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,t))
                                    | _ -> ()
                | ConsoleKey.Q ->
                    engine.quit ()
                | ConsoleKey.M -> 
                    this.changeScene ("menu",wr)
                | ConsoleKey.F -> match this.currentScene.Value with
                                    | Menu(active = a) | Text(active = a) -> 
                                            let option = List.item a this.currentScene.Value.text
                                            match this.currentScene.Value.name with
                                            | "size" -> 
                                                Log.msg "%A" option 
                                                if option="OPTIONS" then 
                                                    this.changeScene ("options", wr)
                                                else
                                                    this.setGameSize (int option + 1) a
                                                    this.changeScene ("size",wr)
                                            | "visibility" -> 
                                                if option="OPTIONS" then 
                                                    this.changeScene ("options", wr)
                                                else
                                                    this.setGameVisibility (int option + 1) a
                                                    this.changeScene ("visibility",wr)
                                            | _ -> 
                                                let op = option.ToLower()
                                                if op = "quit" then engine.quit()
                                                else
                                                    this.changeScene (op,wr)
                                    | _ -> ()
                | _ -> 
                        match this.currentScene.Value with
                        | Game(_,_,_,_,_,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move key.Value)
                            let p = this.currentScene.Value.player
                            let m = this.currentScene.Value.maze
                            if int p.Value.x = m.finish.y && int p.Value.y = m.finish.x then
                                this.changeScene("win",wr)
                            else
                                this.gameStates <- this.deleteScene this.currentScene.Value.name
                                let scene = this.currentScene.Value
                                match scene with
                                | Game(n,pg,bg,mv,maze,size,v) ->
                                    this.resetGame ()
                                    this.addScene (n,mv,true,bg.Value,pg.Value,maze.Value,size=size,visibility=v)
                                    this.changeScene (n,wr)
                                | _ -> ()
                        | Menu(_,_,_,t,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,t))
                        | Text(_,_,_,vs,_,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,vs))
            ()
