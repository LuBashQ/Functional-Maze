module LabProg2019.Game

open Maze
open Gfx
open Engine
open Scene
open Actions

let main () =     
    let W = 51
    let H = 51
    let engine = new engine (W,H)
    let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)

    let maze = Game("play",Some player,None,generateMaze,None,(W,H))
    let solved = Game("solved",Some player,None,solveMaze,None,(W,H))
    let menu = Menu("menu",None,showMenu,["PLAY";"SOLVED";"OPTIONS"],0,(W,H))
    let options = Menu("options",None,showMenu,["SIZE";"MENU"],0,(W,H))     
    let size = Menu("size",None,showMenu,["20";"30";"40";"50";"OPTIONS"],3,(W,H))  

    let sceneManager = SceneManager([menu;maze;solved;options;size],engine)

    engine.loop_on_key <| sceneManager.execute engine
