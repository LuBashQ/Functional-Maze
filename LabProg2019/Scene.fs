module LabProg2019.Scene
open Gfx
open System
open Engine
open Actions
open Maze
exception EmptyQueueException


type SceneManager (_gameStates: State list, _engine: engine) =

    member val gameStates = _gameStates with get,set
    member val currentScene = None with get,set
    member val engine = _engine with get

    member private this.resetState s =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,size) -> Game(n,Some player,None,mv,None,size)
        | _ -> s

    member private this.resetGame () =
        this.gameStates <- List.map(fun (s:State) -> this.resetState s) this.gameStates 

    member this.getScene name =
        List.find (fun (s:State) -> s.name = name) this.gameStates
    
    member this.deleteScene name =
        List.filter (fun (s:State) -> s.name <> name) this.gameStates

    member private this.setCurrentScene name =
        this.currentScene <- Some (this.getScene name)
    
    member this.isPresent name =
        List.exists (fun (e:State) -> e.name = name) this.gameStates

    member private this.addScene (n:string, move: (State->ConsoleKeyInfo->State), 
        game: bool, ?bg:sprite, ?pg:sprite, ?maze: Maze, ?voices: string list, ?active: int, 
        ?menu:(State->ConsoleKeyInfo->wronly_raster->string list->(int*int)->State), ?size: int*int) : unit =
        
        let state = 
            match game with
            | true -> Game(n,pg,bg,move,maze,size.Value)
            | _ -> Menu(n,bg,menu.Value,voices.Value,active.Value,size.Value)

        this.gameStates <- List.append this.gameStates [state]


    member this.changeScene (name,?wr) =
        this.setCurrentScene name
        
        this.engine.removeAll ()
        match this.currentScene.Value with
        | Game(pg = p) -> 
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo()))
            this.engine.register_sprite p.Value
            this.engine.register_sprite this.currentScene.Value.background
        | Menu(_,bg,_,t,_,_) -> 
            this.engine.register_sprite bg.Value; 
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo(),wr.Value,t))


    member this.execute (engine: engine) (key:ConsoleKeyInfo option) (wr: wronly_raster) = 
        if this.currentScene.IsNone || key.IsNone then 
            this.changeScene ("menu",wr)
        else
            match key.Value.Key with
            | ConsoleKey.R -> match this.currentScene.Value with
                                | Game(_,p,_,_,m,s) -> 
                                    if this.isPresent "solve" then ()
                                    else
                                        this.addScene ("solve",showSolution,true,pg=p.Value,maze=m.Value,size=s)
                                        this.changeScene ("solve",wr)
                                        this.gameStates <- this.deleteScene "solve"
                                | Menu(_,_,_,t,_,_) -> this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,t))
            | ConsoleKey.Q -> engine.quit ()
            | ConsoleKey.M -> 
                this.changeScene ("menu",wr)
            | ConsoleKey.F -> match this.currentScene.Value with
                                | Menu(active = a) -> let name = (List.item a this.currentScene.Value.text).ToLower ()
                                                      //Continue non definito ancora, serve per prevenire il reset del gioco
                                                      if not (name = "continue") then this.resetGame ()
                                                      this.changeScene (name,wr)
                                | _ -> ()
            | _ -> this.currentScene <- Some(
                    match this.currentScene.Value with
                    | Game(_,_,_,_,_,_) -> this.currentScene.Value.move key.Value
                    | Menu(_,_,_,t,_,_) -> this.currentScene.Value.move (key.Value,wr,t))
        
            ()
