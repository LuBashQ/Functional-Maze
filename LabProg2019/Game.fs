module LabProg2019.Game

open Maze
open Gfx
open Engine
open Scene
open Actions
open TreeMaze

let main () =     
    let W = 51
    let H = 51
    let engine = new engine (W+1, H+1)
    let m = Maze(21,21)
    m.generate()

    let maze = Game("play",sprite(image.rectangle(1,1,pixel.filled Color.Red),m.start.x,m.start.y,0),m.toSprite(),movePlayer,m)
    let menu = Menu("menu",sprite(image.rectangle(0,0,pixel.filled Color.Blue),0,1,-10),showMenu,["PLAY";"MENU"],0)
                   
    let sceneManager = SceneManager([menu;maze],engine)

    engine.loop_on_key <| sceneManager.execute engine
