module LabProg2019.Actions

open Gfx
open System
open Maze
open TreeMaze


/// <summary>
/// Unione discriminata di Game e Menu che rappresenta la struttura dati principale del gioco
/// </summary>
/// <param name="name">Il nome dello stato</param>
/// <param name="pg">Il giocatore dello stato</param>
/// <param name="bg">Il background dello stato</param>
/// <param name="move">La funzione che deve eseguire ad ogni ciclo di esecuzione</param>
/// <param name="maze">La struttura dati del labirinto</param>
/// <param name="size">La granzedda della finestra di gioco o del labirinto</param>
/// <param name="voices">Lista contenente le voci di menù</param>
/// <param name="active">Un numero indicativo della posizione del cursore nel menù</param>
type State =
    | Game of name:string * pg:sprite option * bg:sprite option * move:(State -> ConsoleKeyInfo -> State) * maze: Maze option * size: (int * int)
    | Menu of name:string * bg:sprite option * move:(State -> ConsoleKeyInfo -> wronly_raster -> string list -> (int*int) -> State) * voices:string list * active: int * size: (int*int)

    /// <summary>
    /// Il nome dello stato
    /// </summary>
    /// <returns>Una stringa indicante il nome lo stato</returns>
    member this.name =
        match this with
        | Game(name = n) | Menu(name = n) -> n.ToLower()
    
    /// <summary>
    /// Il background dello stato
    /// </summary>
    /// <returns>Uno sprite indicante il background dello stato</returns>
    member this.background =
        match this with
        | Game(bg = n) -> n.Value
        | Menu(bg = n) -> n.Value

    /// <summary>
    /// La voce di menù attiva
    /// </summary>
    /// <returns>Un intero indicante la voce di menù attiva</returns>
    member this.active =
        match this with
        | Menu(active = a) -> a
        | _ -> -1
    
    /// <summary>
    /// Il giocatore dello stato
    /// </summary>
    /// <returns>Uno sprite indicante il giocatore dello stato</returns>
    member this.player =
        match this with
        | Game(pg = p) -> p
        | _ -> failwith "Player non definito"

    /// <summary>
    /// La struttura dati del labirinto contenuta nello stato
    /// </summary>
    /// <returns>La struttura dati del labirinto dello stato</returns>
    member this.maze =
        match this with
        | Game(maze = m) -> m.Value
        | _ -> failwith "Maze non definito"

    /// <summary>
    /// Le voci di menù dello stato
    /// </summary>
    /// <returns>Una lista di stringhe rappresentanti le voci di menù</returns>
    member this.text =
        match this with
        | Menu(voices = t) -> t
        | _ -> []

    /// <summary>
    /// La grandezza della finestra
    /// </summary>
    /// <returns>Una coppia di interi rappresentanti la grandezza della finestra</returns>
    member this.size =
        match this with
        | Game(size = (x,y)) -> x,y
        | Menu(size = (x,y)) -> x,y

    /// <summary>
    /// Esecuzione dell'azione assegnata allo stato
    /// </summary>
    /// <param name="k">Il tasto premuto</param>
    /// <param name="r">Il write only raster adibito alla scrittura su schermo</param>
    /// <param name="s">La lista di voci di menù</param>
    /// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
    member this.move (k,?r:wronly_raster,?s:string list) =
        match this with
        | Game(move = n) -> n this k
        | Menu(move = n;size = s) -> n this k r.Value this.text s



/// <summary>
/// Controlla se il movimento eseguito è consentito
/// </summary>
/// <param name="st">Lo stato attuale</param>
/// <param name="dx">Lo spostamento orizzontale</param>
/// <param name="dy">Lo spostamento verticale</param>
/// <returns>Una coppia di float indicante il movimento da eseguire</returns>
let check_bounds (st:State) (dx: float, dy: float)=
    let vertical = int (st.player.Value.x + dx)
    let horizontal = int (st.player.Value.y + dy)
    let w,h = st.size

    if vertical >= 0 && vertical <= w && horizontal >= 0 && horizontal <= h then 
        if (st.maze.maze.[horizontal,vertical].isVisited) then dx,dy
        else 0.,0.
    else
        0.,0.

/// <summary>
/// Sposta lo sprite in base al tasto premuto
/// </summary>
/// <param name="st">Lo stato attuale</param>
/// <param name="key">Il testo premuto</param>
/// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
let movePlayer (st: State) (key: ConsoleKeyInfo): State =
    let dx,dy =
        match key.Key with
        | ConsoleKey.W | ConsoleKey.UpArrow -> 0.,-1.
        | ConsoleKey.S | ConsoleKey.DownArrow -> 0.,1.
        | ConsoleKey.A | ConsoleKey.LeftArrow -> -1.,0.
        | ConsoleKey.D | ConsoleKey.RightArrow -> 1.,0.
        | _ -> 0.,0.

    let x,y = check_bounds st (dx,dy)
    st.player.Value.move_by (x,y)
    st

/// <summary>
/// Genera un nuovo labirinto
/// </summary>
/// <param name="st">Lo stato attuale</param>
/// <param name="key">Il testo premuto</param>
/// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
let generateMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    let background = Some (maze.toSprite())
    let newState = Game("play",st.player,background,movePlayer,Some maze,(w,h))
    movePlayer newState key


/// <summary>
/// Genera un nuovo labirinto risolto
/// </summary>
/// <param name="st">Lo stato attuale</param>
/// <param name="key">Il testo premuto</param>
/// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
let solveMaze (st: State) (key: ConsoleKeyInfo) : State =
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    solveRecursive maze |> ignore
    let background = Some (maze.toSprite())
    let newState = Game(st.name,st.player,background,movePlayer,Some maze,st.size)
    movePlayer newState key

/// <summary>
/// Scrive sulla console di gioco
/// </summary>
/// <param name="s">La stringa da scrivere</param>
/// <param name="index">L'indice, indicante la posizione della voce di menù</param>
/// <param name="wr">Il write only raster adibito alla scrittura su schermo</param>
/// <param name="size">La grandezza della finestra di gioco</param>
let drawText (s: string) (index: int) (color:ConsoleColor) (wr: wronly_raster) (size: int*int)  : unit =
    let x,y = size
    wr.draw_text((sprintf "%s\n\n" s),x/2-2,(y/2 + index),color)


/// <summary>
/// Scrive sulla console di gioco
/// </summary>
/// <param name="s">La stringa da scrivere</param>
/// <param name="index">L'indice, indicante la posizione della voce di menù</param>
/// <param name="wr">Il write only raster adibito alla scrittura su schermo</param>
/// <param name="size">La grandezza della finestra di gioco</param>
/// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
let rec showMenu (st: State) (key: ConsoleKeyInfo)  (wr: wronly_raster) (ls: string list) (size: int*int) : State =
    
    // Permette lo scorrimento delle voci di menù dall'alto verso il basso e viceversa in modo circolare
    let idx = match key.Key with
                | ConsoleKey.S | ConsoleKey.DownArrow -> (st.active + 1) % (List.length ls)
                | ConsoleKey.W | ConsoleKey.UpArrow -> abs (((st.active - 1) + List.length ls) % (List.length ls))
                | _ -> st.active
    
    let rec aux ls i idx=
        match ls with
        | [] -> ()
        | x::xs -> if i = idx then 
                    drawText x i ConsoleColor.Cyan wr size
                   else
                    drawText x i ConsoleColor.White wr size
                   aux xs (i+1) idx

    aux ls 0 idx
    Menu(st.name,None,showMenu,st.text,idx,size)

/// <summary>
/// Mostra la soluzione del labirinto attuale
/// </summary>
/// <param name="st">Lo stato attuale</param>
/// <param name="key">Il testo premuto</param>
/// <returns>Un nuovo stato, risultato della modifica di quello attuale</returns>
let showSolution (st: State) (key: ConsoleKeyInfo) : State =
    let solved = solveRecursive st.maze
    let background = Some (solved.toSprite())
    let newState = Game(st.name,st.player,background,movePlayer,Some solved,st.size)
    movePlayer newState key
