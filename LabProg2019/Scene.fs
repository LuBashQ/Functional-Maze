module LabProg2019.Scene
open Gfx
open System
open Engine
open Actions
open Maze
exception EmptyQueueException

/// <summary>
/// Tipo SceneManager
/// </summary>
/// <param name="_gameStates">Gli stati supportati dal gioco</param>
/// <param name="_engine">Il motore di gioco</param>
type SceneManager (_gameStates: State list, _engine: engine) =

    member val gameStates = _gameStates with get,set
    member val currentScene = None with get,set
    member val engine = _engine with get


    /// <summary>
    /// Esegue il reset di uno stato
    /// </summary>
    /// <param name="s">Lo stato attuale</param>
    /// <returns>Un nuovo stato default</returns>
    member private this.resetState s =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,size) -> Game(n,Some player,None,mv,None,size)
        | _ -> s

    /// <summary>
    /// Cambia le dimensioni del labirinto 
    /// </summary>
    /// <param name="s">Lo stato attuale</param>
    /// <param name="size">La nuova grandezza</param>
    /// <param name="pos">La nuova posizione del cursore nel menù</param>
    /// <returns>Un nuovo stato con la grandezza modificata</returns>
    member private this.setSize (s,size,?pos) =
        let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)
        match s with
        | Game(n,_,_,mv,_,_) -> Game(n,Some player,None,mv,None,(size,size))
        | Menu(n,bg,mv,ls,a,size) -> Menu(n,bg,mv,ls,pos.Value,size)
        | _ -> s
    
    /// <summary>
    /// Imposta una nuova grandezza a tutti gli stati del gioco
    /// </summary>
    /// <param name="size">La nuova grandezza</param>
    /// <param name="pos">La nuova posizione del cursore nel menù</param>
    member private this.setGameSize size pos =
        this.gameStates <- List.map(fun (s:State) -> this.setSize (s,size,pos)) this.gameStates 

    /// <summary>
    /// Esegue il reset di tutti gli stati del gioco
    /// </summary>
    member private this.resetGame () =
        this.gameStates <- List.map(fun (s:State) -> this.resetState s) this.gameStates 

    /// <summary>
    /// Cerca uno stato all'interno della lista degli stati di gioco
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    /// <returns>Una lista contenente lo stato cercato</returns>
    member this.getScene name =
        List.find (fun (s:State) -> s.name = name) this.gameStates
    
    /// <summary>
    /// Rimuove uno stato dall'interno della lista degli stati di gioco
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    /// <returns>Una lista contenente gli stati del gioco escluso quello rimosso</returns>
    member this.deleteScene name =
        List.filter (fun (s:State) -> s.name <> name) this.gameStates

    /// <summary>
    /// Imposta lo stato corrente
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    member private this.setCurrentScene name =
        this.currentScene <- Some (this.getScene name)
    
    /// <summary>
    /// Controlla l'esistenza di uno stato all'interno della lista degli stati di gioco
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    /// <returns>Un boolean rappresentante l'esito della ricerca</returns>
    member this.isPresent name =
        List.exists (fun (e:State) -> e.name = name) this.gameStates

    
    /// <summary>
    /// Aggiunge un nuovo stato alla lista degli stati di gioco
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    /// <param name="move">La funzione da eseguire ad ogni ciclo di esecuzione (per Game)</param>
    /// <param name="game">Valore indicante se lo stato è Game o Menu</param>
    /// <param name="bg">Il background dello stato</param>
    /// <param name="pg">Il giocatore dello stato</param>
    /// <param name="name">Il nome dello stato</param>
    /// <param name="maze">La struttura dati rapprensentante il labirinto</param>
    /// <param name="voices">Le voci di menù</param>
    /// <param name="active">La posizione del cursore nel menù</param>
    /// <param name="menu">La funzione da eseguire ad ogni ciclo di esecuzione (per Menu)</param>
    /// <param name="size">La grandezza della finestra o del labirinto</param>
    member private this.addScene (n:string, move: (State->ConsoleKeyInfo->State), 
        game: bool, ?bg:sprite, ?pg:sprite, ?maze: Maze, ?voices: string list, ?active: int, 
        ?menu:(State->ConsoleKeyInfo->wronly_raster->string list->(int*int)->State), ?size: int*int) : unit =
        
        let state = 
            match game with
            | true -> Game(n,pg,bg,move,maze,size.Value)
            | _ -> Menu(n,bg,menu.Value,voices.Value,active.Value,size.Value)

        this.gameStates <- List.append this.gameStates [state]


    /// <summary>
    /// Cambia la scena attuale
    /// </summary>
    /// <param name="name">Il nome dello stato</param>
    member this.changeScene (name,?wr) =
        this.setCurrentScene name
        
        //pulisce tutti gli sprite
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
    /// Controlla il testo premuto ed esegue la funzione adeguata
    /// </summary>
    /// <param name="engine">Il motore di gioco</param>
    /// <param name="key">Il tasto premuto</param>
    /// <param name="wr">Il write only raster adibito alla scrittura a schermo</param>
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
                                    | _ -> ()
                | ConsoleKey.Q ->
                    engine.quit ()
                | ConsoleKey.M -> 
                    this.changeScene ("menu",wr)
                | ConsoleKey.F -> match this.currentScene.Value with
                                    | Menu(active = a) | Text(active = a) -> 
                                        if this.currentScene.Value.name = "size" then
                                            if  not (a = this.currentScene.Value.text.Length - 1) then
                                                let size = int (List.item a this.currentScene.Value.text) + 1
                                                this.setGameSize size a
                                                this.changeScene ("size",wr)
                                            else
                                                let name = (List.item a this.currentScene.Value.text).ToLower ()
                                                this.changeScene (name,wr)
                                        else
                                            let name = (List.item a this.currentScene.Value.text).ToLower ()
                                            //Continue non definito ancora, serve per prevenire il reset del gioco
                                            if not (name = "continue") then this.resetGame ()
                                            this.changeScene (name,wr)
                                    | _ -> ()
                | _ -> 
                        match this.currentScene.Value with
                        | Game(_,_,_,_,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move key.Value)
                            let p = this.currentScene.Value.player
                            let m = this.currentScene.Value.maze
                            if int p.Value.x = m.finish.y && int p.Value.y = m.finish.x then
                                this.changeScene("vittoria",wr)
                            else
                                this.currentScene <- Some (this.currentScene.Value.move key.Value)
                        | Menu(_,_,_,t,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,t))
                        | Text(_,_,_,vs,_,_,_) -> 
                            this.currentScene <- Some (this.currentScene.Value.move (key.Value,wr,vs))
            ()
