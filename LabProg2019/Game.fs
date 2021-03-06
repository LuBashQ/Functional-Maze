﻿(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Game.fs: game entry point
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Game

open Gfx
open Engine
open StateManager
open Actions

let main () =     
    
    let W = 51
    let H = 51
    let engine = new engine (W,H)
    let player = sprite(image.rectangle(1,1,pixel.filled Color.Red),1,1,1)

    // Game states definition
    let maze = Game("normal",Some player,None,generateMaze,None,(W,H),0)
    let hardcoreMaze = Game("hardcore",Some player,None,generateHardcoreMaze,None,(W,H),5)
    let solved = Game("solved",Some player,None,solveMaze,None,(W,H),0)
    let menu = Menu("menu",None,showMenu,["NORMAL";"HARDCORE";"SOLVED";"OPTIONS";"HELP";"QUIT"],0,(W,H))
    let options = Menu("options",None,showMenu,["SIZE";"VISIBILITY";"MENU"],0,(W,H))     
    let size = Menu("size",None,showMenu,["4";"10";"20";"30";"40";"50";"OPTIONS"],3,(W,H))  
    let visibility = Menu("visibility",None,showMenu,["3";"5";"7";"9";"11";"13";"OPTIONS"],1,(W,H)) 
    let help = Text("help",None,showText,["|  MAZE GAME - INTRUCTIONS  |";"USE W-A-S-D or ARROWS to move";"USE 'F' to execute";"USE 'R' to solve maze"; 
    "USE 'Q' or QUIT to quit";"NORMAL to play a standard game";"HARDCORE to have a challenge";"OPTIONS to change settings";"MENU"],(W,H),(W/5+2,15),0)
    let win = Text("win",None,showText,["VICTORY!";"MENU"],(W,H),(W/2-5,H/2-2),0)

    let sceneManager = StateManager([menu;maze;solved;options;size;help;win;hardcoreMaze;visibility],engine)

    // Game execution
    engine.loop_on_key <| sceneManager.execute engine
