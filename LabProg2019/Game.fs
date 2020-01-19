module LabProg2019.Game

open Maze
open Gfx
open Engine
open Scene
open Actions

let main () =     
    let W = 31
    let H = 31
    let engine = new engine (W,H)
    let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)

    let maze = Game("play",Some player,None,generateMaze,None,(W,H))
    let solved = Game("solved",Some player,None,solveMaze,None,(W,H))
    let menu = Menu("menu",Some (sprite(image.rectangle(0,0,pixel.filled Color.Blue),0,1,-10)),showMenu,["PLAY";"SOLVED"],0,(W,H))
            
    let sceneManager = SceneManager([menu;maze;solved],engine)

    engine.loop_on_key <| sceneManager.execute engine
