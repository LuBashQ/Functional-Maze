module LabProg2019.Game

open System
open Maze
open Gfx
open Engine
open TreeMaze
open Scene

//let createInstructions (st:state, screen: wronly_raster) = 
//    let instructions = "HELP PAGE - COMMAND INSTRUCTIONS:\n- Move with 'w'-'a'-'s'-'d' keys -> up-left-down-right\n- Press 'f' for enter\n- Press 'm' for the menu\n- Press '1' to show the solution, if you aren't able to find it \n- Press '2' to create another maze\n\n ENJOY THE GAME :-)"
//    screen.draw_text ((sprintf "%s"instructions), 0, 0, Color.White, Color.Black)
    

let main () =     
    let W = 51
    let H = 51
    let engine = new engine (W+1, H+1)

    //let clear (st:state) =
    //    engine.remove_and_unregister_sprite (st.background)
    //    engine.remove_and_unregister_sprite (st.player)


    //let showSolution (st: state) (key: ConsoleKeyInfo option) =
    //    let resolvedMaze = solveRecursive st.maze
    //    clear (st)
    //    let newBackground = engine.create_and_register_sprite(image(W,H,resolvedMaze.convertToPixel ()),1,1,0)
    //    let player = engine.create_and_register_sprite(image.rectangle(1,1,pixel.filled Color.Red),resolvedMaze.start.y + 1,resolvedMaze.start.x + 1,1)
    //    st.background <- newBackground
    //    st.player <- player
    //    st.maze <- resolvedMaze 
    //    st

    //let showHelp (st: state, screen: wronly_raster) = 
    //    clear (st)
    //    createInstructions (st, screen)
    //    st

    //let rec resetMaze (st: state) (key: ConsoleKeyInfo option) =
    //    let maze = Maze(W,H)
    //    let entrance,exit = maze.generate ()
    //    clear (st)
    //    if entrance.x = exit.x && entrance.y = exit.y then
    //        resetMaze (st) None
    //    else
    //        let background = engine.create_and_register_sprite(image(W,H,maze.convertToPixel ()),1,1,0)
    //        let player = engine.create_and_register_sprite(image.rectangle(1,1,pixel.filled Color.Red),entrance.y + 1,entrance.x + 1,1)
    //        st.player <- player
    //        st.background <- background
    //        st.maze <- maze
    //        st
    
    //let check_bounds (st : State) (dx: float, dy: float)=
    //    let vertical = int (st.player.x + dx) - 1 //offset
    //    let horizontal = int (st.player.y + dy) - 1 //offset

    //    if vertical >= 0 && vertical <= W && horizontal >= 0 && horizontal <= H then 
    //        if (st.maze.maze.[horizontal,vertical].isVisited) then dx,dy
    //        else 0.,0.
    //    else
    //        0.,0.
    
    //let movePlayer (st: state) (key: ConsoleKeyInfo option)  =
    //    let dx, dy =
    //        match key.Value.KeyChar with 
    //        | 'w' -> 0., -1.
    //        | 's' -> 0., 1.
    //        | 'a' -> -1., 0.
    //        | 'd' -> 1., 0.
    //        | _   -> 0., 0.
    //    let x,y = check_bounds st (dx,dy)
    //    st.player.move_by (x, y)
    //    st                                           
    
    //let s = showSolution (resetMaze state.Default None) None
    //let solution = new Scene(s,"solution",movePlayer,Game)
    //let manager = new SceneManager ([solution])
    //manager.changeScene "solution" showSolution

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

    let moveMenu (st: State) (key: ConsoleKeyInfo) : State =
        Log.debug "MENU"
        st
        
    let m = Maze(21,21).generate ()

    let maze = Game("maze",sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,0),m,movePlayer)
    let menu = Menu("menu",sprite(image.rectangle(10,10,pixel.filled Color.Blue),0,0,1),moveMenu)
                   
    let sceneManager = SceneManager([maze; menu],engine)
    sceneManager.changeScene "maze"
    
    engine.loop_on_key <| sceneManager.execute engine
