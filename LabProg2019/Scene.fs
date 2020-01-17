module LabProg2019.Scene
open Gfx
open System
open Engine
open Maze

type State =
    | Game of name:string * pg:sprite * bg:sprite * move:(State -> ConsoleKeyInfo -> State)
    | Menu of name:string * bg:sprite * move:(State -> ConsoleKeyInfo -> State)

    member this.name =
        match this with
        | Game(name = n) | Menu(name = n) -> n

    member this.player =
        match this with
        | Game(pg = p) -> p
        | _ -> failwith ""

    member this.background =
        match this with
        | Game(bg = n) -> n
        | Menu(bg = n) -> n

    member this.move k =
        match this with
        | Game(move = n) | Menu(move = n) -> n this k


exception EmptyQueueException


type SceneManager (_states: State list, _engine: engine) =

    member val states = _states with get,set
    member val currentScene = List.head _states with get,set
    member val engine = _engine with get

    member this.getScene name =
        List.find (fun (s:State) -> s.name = name) this.states
    
    member this.deleteScene name =
        List.filter (fun (s:State) -> s.name <> name) this.states

    member private this.setCurrentScene name =
        this.currentScene <- this.getScene name

    member this.changeScene name =
        this.setCurrentScene name
        
        this.engine.removeAll ()
        match this.currentScene with
        | Game(_,pg,bg,_) -> 
            this.engine.register_sprite pg
            this.engine.register_sprite bg
        | Menu(_,bg,_) -> this.engine.register_sprite bg


    member this.execute (engine: engine) (key:ConsoleKeyInfo) (wr: wronly_raster) = 
        
        match key.Key with
        | ConsoleKey.Q -> engine.quit ()
        | ConsoleKey.M -> this.changeScene "menu"
        | ConsoleKey.G -> this.changeScene "maze"
        | _ -> this.currentScene <- this.currentScene.move key
        
        ()
      
