module LabProg2019.Scene
open Gfx
open System
open Engine
open Maze
exception EmptyQueueException

type State =
    | Game of name:string * pg:sprite * bg:sprite * move:(State -> ConsoleKeyInfo -> State) * maze: Maze
    | Menu of name:string * bg:sprite * move:(State -> ConsoleKeyInfo -> wronly_raster -> string list -> State) * voices:string list * active: int

    member this.name =
        match this with
        | Game(name = n) | Menu(name = n) -> n.ToLower()

    member this.player =
        match this with
        | Game(pg = p) -> p
        | _ -> failwith "Player non definito"

    member this.background =
        match this with
        | Game(bg = n) -> n
        | Menu(bg = n) -> n

    member this.active =
        match this with
        | Menu(active = a) -> a
        | _ -> -1

    member this.maze =
        match this with
        | Game(maze = m) -> m
        | _ -> failwith "Maze non definito"

    member this.text =
        match this with
        | Menu(voices = t) -> t
        | _ -> []

    member this.move (k,?r:wronly_raster,?s:string list) =
        match this with
        | Game(move = n) -> n this k
        | Menu(move = n) -> n this k r.Value this.text


type SceneManager (_states: State list, _engine: engine) =

    member val states = _states with get,set
    member val currentScene = None with get,set
    member val engine = _engine with get

    member this.getScene name =
        List.find (fun (s:State) -> s.name = name) this.states
    
    member this.deleteScene name =
        List.filter (fun (s:State) -> s.name <> name) this.states

    member private this.setCurrentScene name =
        this.currentScene <- Some (this.getScene name)

    member this.changeScene (name,?wr) =
        this.setCurrentScene name
        
        this.engine.removeAll ()
        match this.currentScene.Value with
        | Game(_,pg,bg,_,_) -> 
            this.engine.register_sprite pg
            this.engine.register_sprite bg
        | Menu(_,bg,_,t,_) -> 
            this.engine.register_sprite bg; 
            this.currentScene <- Some (this.currentScene.Value.move (ConsoleKeyInfo(),wr.Value,t))


    member this.execute (engine: engine) (key:ConsoleKeyInfo option) (wr: wronly_raster) = 
        
        if this.currentScene.IsNone || key.IsNone then 
            this.changeScene ("menu",wr)
        else
            match key.Value.Key with
            | ConsoleKey.Q -> engine.quit ()
            | ConsoleKey.M -> this.changeScene ("menu",wr)
            | ConsoleKey.F -> match this.currentScene.Value with
                                | Menu(active = a) -> let name = (List.item a this.currentScene.Value.text).ToLower ()
                                                      this.changeScene (name,wr)
                                | _ -> ()

            | _ -> this.currentScene <- Some(
                    match this.currentScene.Value with
                    | Game(_,_,_,_,_) -> this.currentScene.Value.move key.Value
                    | Menu(_,_,_,t,_) -> this.currentScene.Value.move (key.Value,wr,t))
        
            ()
