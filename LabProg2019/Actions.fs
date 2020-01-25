module LabProg2019.Actions

open Gfx
open System
open Maze
open TreeMaze
open Cell

exception PropertyNotImplementedException

/// <summary>
/// Discriminated union of Game and Menu that represent the game main data structure
/// </summary>
/// <param name="name">State name</param>
/// <param name="pg">State player</param>
/// <param name="bg">State background</param>
/// <param name="move">The function to execute at every cycle of execution </param>
/// <param name="maze">Maze data stucture/param>
/// <param name="size">Window or maze size</param>
/// <param name="voices">List with menu items</param>
/// <param name="active">A value that indicate the cursor postion in the menu</param>
type State =
    | Game of name:string * pg:sprite option * bg:sprite option * 
    move:(State -> ConsoleKeyInfo -> State) * maze: Maze option * size: (int * int) * visibility: int
    | Menu of name:string * bg:sprite option * 
    move:(State -> ConsoleKeyInfo -> wronly_raster -> string list -> (int*int) -> State) * 
    voices:string list * active: int * size: (int*int)
    | Text of name:string * bg:sprite option * 
    move:(State -> ConsoleKeyInfo -> wronly_raster -> string list -> (int * int) -> (int*int) ->State) * 
    voices: string list * size: (int*int) * pos: (int*int) * active: int
    /// <summary>
    /// Gets the state's name
    /// </summary>
    /// <returns>A string with state name</returns>
    member this.name : string =
        match this with
        | Game(name = n) | Menu(name = n) | Text(name = n) -> n.ToLower()
    
    member this.visibility = 
        match this with
        | Game(visibility = v) -> v
        | _ -> raise PropertyNotImplementedException

    /// <summary>
    /// Gets the state's background
    /// </summary>
    /// <returns>A sprite indicating background state</returns>
    member this.background : sprite =
        match this with
        | Game(bg = n) -> n.Value
        | Menu(bg = n) -> n.Value
        | Text(bg = n) -> n.Value
        
    /// <summary>
    /// Gets the state's active menu voice
    /// </summary>
    /// <returns>A int indicating the menu voice</returns>
    member this.active : int =
        match this with
        | Menu(active = a) | Text(active = a) -> a
        | _ -> raise PropertyNotImplementedException
    
    /// <summary>
    /// Gets the state's player
    /// </summary>
    /// <returns>A sprite indicating the state player</returns>
    member this.player : sprite =
        match this with
        | Game(pg = p) -> p.Value
        | _ -> raise PropertyNotImplementedException

    /// <summary>
    /// Gets the state's maze data structure
    /// </summary>
    /// <returns>Maze data structure of the state</returns>
    member this.maze : Maze =
        match this with
        | Game(maze = m) -> m.Value
        | _ -> raise PropertyNotImplementedException

    /// <summary>
    /// Gets the state's menu voices
    /// </summary>
    /// <returns>A string list representing menu items</returns>
    member this.text : string list =
        match this with
        | Menu(voices = t) | Text(voices = t) -> t
        | _ -> raise PropertyNotImplementedException

    /// <summary>
    /// Window size
    /// Gets the maze's game size for Game or the window's size for Menu and Text
    /// </summary>
    /// <returns>A couple of int representing the size</returns>
    member this.size : int*int =
        match this with
        | Game(size = (x,y)) -> x,y
        | Menu(size = (x,y)) -> x,y
        | Text(size = (x,y)) -> x,y

    /// <summary>
    /// Gets the the state's text position
    /// </summary>
    /// <returns>A couple of int representing the text position</returns>
    member this.position : int*int =
        match this with
        | Text(pos = (x,y)) -> x,y
        | _ -> raise PropertyNotImplementedException

    /// <summary>
    /// Executes the state's assigned action
    /// </summary>
    /// <param name="k">Pressed key</param>
    /// <param name="r">Write only raster used for screen writing</param>
    /// <param name="s">Menu items list</param>
    /// <returns>A new state, that comes from the changes on the actual state</returns>
    member this.move (k,?r:wronly_raster,?s:string list) : State =
        match this with
        | Game(move = n) -> n this k
        | Menu(move = n;size = s) -> n this k r.Value this.text s
        | Text(move= n;size = s; pos = p) -> n this k r.Value this.text s p


/// <summary>
/// Checks if the movement is allowed
/// </summary>
/// <param name="st">Current state</param>
/// <param name="dx">Horizontal displacement</param>
/// <param name="dy">Vertical displacement</param>
/// <returns>A couple of float indicating the action to execute</returns>
let check_bounds (st:State) (dx: float, dy: float) : float*float =
    let vertical = int (st.player.x + dx)
    let horizontal = int (st.player.y + dy)
    let w,h = st.size

    if vertical >= 0 && vertical <= w && horizontal >= 0 && horizontal <= h then 
        if (st.maze.maze.[horizontal,vertical].isVisited) then dx,dy
        else 0.,0.
    else
        0.,0.

/// <summary>
/// Writes the state's menu voices
/// </summary>
/// <param name="s">The string to write</param>
/// <param name="index">The index of the item in the menu</param>
/// <param name="wr">Write only raster used for screen writing</param>
/// <param name="size">Game window size</param>
let drawMenuText (s: string) (index: int) (color:ConsoleColor) (wr: wronly_raster) (size: int*int) : unit =
    let x,y = size
    wr.draw_text((sprintf "%s\n\n" s),x/2-3,(y/2 + index),color)

/// <summary>
/// Writes the state's text message
/// </summary>
/// <param name="s">The string to write</param>
/// <param name="index">The index of the item in the menu</param>
/// <param name="wr">Write only raster used for screen writing</param>
/// <param name="size">Game window size</param>
let drawText (s: string) (index: int) (color:ConsoleColor) (wr: wronly_raster) (size: int*int) (pos: int*int) : unit =
    let x,y = pos
    wr.draw_text((sprintf "%s\n\n" s),x,y,color)


/// <summary>
/// Draws all menu voices of a given state
/// </summary>
/// <param name="st">The given state</param>
/// <param name="key">The pressed key</param>
/// <param name="wr">Write only raster used for screen writing</param>
/// <param name="ls">the list of menu voices to be written</param>
/// <param name="size">Game window size</param>
/// <param name="pos">The position that marks be beginning of the word</param>
/// <returns>A new state with the modified index</returns>
let rec showText (st: State) (key: ConsoleKeyInfo)  (wr: wronly_raster) (ls: string list) (size: int*int) (pos: int*int) : State =
    let idx = ls.Length - 1
    let rec aux ls i idx pos=
        match ls with
        | [] -> ()
        | s::xs -> 
            let x,y = pos
            if i = idx then 
                drawMenuText s i ConsoleColor.Cyan wr size
            else
                drawText s i ConsoleColor.White wr size pos
                aux xs (i+1) idx (x,y+2)

    aux ls 0 idx pos
    Text(st.name,None,showText,st.text,st.size,st.position,idx)

/// <summary>
/// Moves the sprite according to the key pressed
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let movePlayer (st: State) (key: ConsoleKeyInfo) : State =
    let dx,dy =
        match key.Key with
        | ConsoleKey.W | ConsoleKey.UpArrow -> 0.,-1.
        | ConsoleKey.S | ConsoleKey.DownArrow -> 0.,1.
        | ConsoleKey.A | ConsoleKey.LeftArrow -> -1.,0.
        | ConsoleKey.D | ConsoleKey.RightArrow -> 1.,0.
        | _ -> 0.,0.

    let x,y = check_bounds st (dx,dy)
    st.player.move_by (x,y)
    st

/// <summary>
/// Draws a portion of the screen based on the visibility range
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let drawSubRegion (st: State) : State =
    let maze = st.maze
    let background = maze.toSprite (1,st.visibility)
    match st with
    | Game(n,pg,bg,mv,maze,size,v) -> 
        Game(n,pg,Some background,mv,maze,size,v)
    | _ -> failwith ""


/// <summary>
/// Moves the sprite according to the key pressed and draws the corresponding subregion of the maze
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let rec hardModeMove (st: State) (key: ConsoleKeyInfo) : State =
    let dx,dy =
        match key.Key with
        | ConsoleKey.W | ConsoleKey.UpArrow -> 0.,-1.
        | ConsoleKey.S | ConsoleKey.DownArrow -> 0.,1.
        | ConsoleKey.A | ConsoleKey.LeftArrow -> -1.,0.
        | ConsoleKey.D | ConsoleKey.RightArrow -> 1.,0.
        | _ -> 0.,0.

    let x,y = check_bounds st (dx,dy)
    st.player.move_by (x,y)
    let px,py = int (ceil st.player.x), int (ceil st.player.y)
    st.maze.player <- Cell(px,py,st.maze.width,st.maze.height)
    drawSubRegion st


/// <summary>
/// Generates a new maze
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let generateMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    let player = Cell(int st.player.x, int st.player.y,maze.width,maze.height)
    maze.player <- player
    let background = maze.toSprite 0
    let newState = Game("play",Some st.player,Some background,movePlayer,Some maze,(w,h),st.visibility)
    movePlayer newState key

/// <summary>
/// Generates a new maze with limited visibility
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let generateHardcoreMaze (st:State) (key: ConsoleKeyInfo) : State = 
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    let player = Cell(int st.player.x, int st.player.y,maze.width,maze.height)
    maze.player <- player
    let background = maze.toSprite (1,st.visibility)
    let newState = Game("play",Some st.player,Some background,hardModeMove,Some maze,(w,h),st.visibility)
    hardModeMove newState key

/// <summary>
/// Generates a new solved maze
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let solveMaze (st: State) (key: ConsoleKeyInfo) : State =
    let w,h = st.size
    let maze = new Maze(w,h)
    maze.generate ()
    solveRecursive maze |> ignore
    let background = Some (maze.toSprite(0))
    let newState = Game(st.name,Some st.player,background,movePlayer,Some maze,st.size,st.visibility)
    movePlayer newState key


/// <summary>
/// Writes in the game console
/// </summary>
/// <param name="s"String to write</param>
/// <param name="index">Index indicating menu item</param>
/// <param name="wr">Write only raster used for screen writing</param>
/// <param name="size">Game window size</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let rec showMenu (st: State) (key: ConsoleKeyInfo)  (wr: wronly_raster) (ls: string list) (size: int*int) : State =
    
    // Allow the scrolling of menu items from top to bottom and the other way around in a circolar way
    let idx = match key.Key with
                | ConsoleKey.S | ConsoleKey.DownArrow -> (st.active + 1) % (List.length ls)
                | ConsoleKey.W | ConsoleKey.UpArrow -> abs (((st.active - 1) + List.length ls) % (List.length ls))
                | _ -> st.active
    
    let rec aux ls i idx=
        match ls with
        | [] -> ()
        | x::xs -> if i = idx then 
                    drawMenuText x i ConsoleColor.Cyan wr size
                   else
                    drawMenuText x i ConsoleColor.White wr size
                   aux xs (i+1) idx

    aux ls 0 idx
    Menu(st.name,None,showMenu,st.text,idx,size)

/// <summary>
/// Shows the current maze solution
/// </summary>
/// <param name="st">Current state</param>
/// <param name="key">Key pressed</param>
/// <returns>A new state, that comes from the changes on the actual state</returns>
let showSolution (st: State) (key: ConsoleKeyInfo) : State =
    let solved = solveRecursive st.maze
    let background = Some (solved.toSprite(0))
    let newState = Game(st.name,Some st.player,background,movePlayer,Some solved,st.size,st.visibility)
    movePlayer newState key

