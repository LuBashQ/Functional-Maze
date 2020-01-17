module LabProg2019.Actions

open Scene
open Gfx
open System
open TreeMaze
open Maze

let movePlayer (st: State) (key: ConsoleKeyInfo): State =
    let dx,dy =
        match key.Key with
        | ConsoleKey.W -> 0.,-1.
        | ConsoleKey.S -> 0.,1.
        | ConsoleKey.A -> -1.,0.
        | ConsoleKey.D -> 1.,0.
        | _ -> 0.,0.

    //let x,y = check_bounds st (dx,dy)
    st.player.move_by (dx,dy)
    st

//TODO: non fanno nulla LOL
let rec generateMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),st.maze.start.x,st.maze.start.y,0)
    let state = Game(st.name,player,st.maze.toSprite(),movePlayer,st.maze)
    state

let rec solveMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let solvedMaze = solveRecursive(st.maze)
    let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),solvedMaze.start.x,solvedMaze.start.y,0)
    let state = Game(st.name,player,solvedMaze.toSprite(),movePlayer,solvedMaze)
    state

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
    Menu(st.name,st.background,showMenu,st.text,idx)



