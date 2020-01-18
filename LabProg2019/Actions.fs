module LabProg2019.Actions

open Gfx
open System
open Maze
open TreeMaze

type State =
    | Game of name:string * pg:sprite option * bg:sprite option * move:(State -> ConsoleKeyInfo -> State) * maze: Maze option * size: (int * int)
    | Menu of name:string * bg:sprite option * move:(State -> ConsoleKeyInfo -> wronly_raster -> string list -> State) * voices:string list * active: int

    member this.name =
        match this with
        | Game(name = n) | Menu(name = n) -> n.ToLower()

    member this.player =
        match this with
        | Game(pg = p) -> p
        | _ -> failwith "Player non definito"

    member this.background =
        match this with
        | Game(bg = n) -> n.Value
        | Menu(bg = n) -> n.Value

    member this.active =
        match this with
        | Menu(active = a) -> a
        | _ -> -1

    member this.maze =
        match this with
        | Game(maze = m) -> m.Value
        | _ -> failwith "Maze non definito"

    member this.text =
        match this with
        | Menu(voices = t) -> t
        | _ -> []

    member this.size =
        match this with
        | Game(size = (x,y)) -> x,y
        | _ -> failwith "Size non definito"

    member this.move (k,?r:wronly_raster,?s:string list) =
        match this with
        | Game(move = n) -> n this k
        | Menu(move = n) -> n this k r.Value this.text

let check_bounds (st:State) (dx: float, dy: float)=
    let vertical = int (st.player.Value.x + dx)
    let horizontal = int (st.player.Value.y + dy)
    let w,h = st.size

    if vertical >= 0 && vertical <= w && horizontal >= 0 && horizontal <= h then 
        if (st.maze.maze.[horizontal,vertical].isVisited) then dx,dy
        else 0.,0.
    else
        0.,0.

let movePlayer (st: State) (key: ConsoleKeyInfo): State =
    let dx,dy =
        match key.Key with
        | ConsoleKey.W -> 0.,-1.
        | ConsoleKey.S -> 0.,1.
        | ConsoleKey.A -> -1.,0.
        | ConsoleKey.D -> 1.,0.
        | _ -> 0.,0.

    let x,y = check_bounds st (dx,dy)
    st.player.Value.move_by (x,y)
    st

let generateMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    let background = Some (maze.toSprite())
    let newState = Game(st.name,st.player,background,movePlayer,Some maze,st.size)
    movePlayer newState key

let solveMaze (st: State) (key: ConsoleKeyInfo) : State =
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    solveRecursive maze |> ignore
    let background = Some (maze.toSprite())
    let newState = Game(st.name,st.player,background,movePlayer,Some maze,st.size)
    movePlayer newState key

let drawText (s: string) (index: int) (color:ConsoleColor) (wr: wronly_raster)  : unit =
    wr.draw_text((sprintf "%s\n" s),0,index,color)

let rec showMenu (st: State) (key: ConsoleKeyInfo)  (wr: wronly_raster) (ls: string list) : State =
    
    let idx = match key.Key with
                | ConsoleKey.S -> abs (st.active - 1) % (List.length ls)
                | ConsoleKey.W -> (st.active + 1) % (List.length ls)
                | _ -> st.active
    
    
    let rec aux ls i idx=
        match ls with
        | [] -> ()
        | x::xs -> if i = idx then 
                    drawText x i ConsoleColor.Cyan wr
                   else
                    drawText x i ConsoleColor.White wr
                   aux xs (i+1) idx

    aux ls 0 idx
    Menu(st.name,Some st.background,showMenu,st.text,idx)

let showSolution (st: State) (key: ConsoleKeyInfo) : State =
    let solved = solveRecursive st.maze
    let background = Some (solved.toSprite())
    let newState = Game(st.name,st.player,background,movePlayer,Some solved,st.size)
    movePlayer newState key


