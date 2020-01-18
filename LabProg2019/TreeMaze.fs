module LabProg2019.TreeMaze

open System.Collections.Generic
open Maze

[<NoComparison>]
type Tree = {
    parent : Tree option
    value : Cell
    mutable children : Cell list
}

let rec getChildren (root: Tree, maze: Cell[,]) =
    if (root.parent = None) then root.children <- root.value.children(maze,root.value)
    else root.children <- root.value.children(maze,root.parent.Value.value)

let rec retrievePath (root: Tree option, maze: Maze) =
    if (root.Value.parent = None) then [root.Value.value]
    else root.Value.value::retrievePath(root.Value.parent,maze)


let buildPath (root: Tree option, maze: Maze) =
    if root = None then 
        maze
    else
        let path = retrievePath (root, maze)
        for p in path do
            maze.maze.[p.x,p.y].isPath <- true
        maze

let solveIterative (maze: Maze) =
    let tree = {
        parent = None; value = maze.start; children = []
    }
    let mutable final = None
    let stack = Stack<Tree>()
    getChildren(tree,maze.maze)
    stack.Push (tree) 
    while stack.Count > 0 do
        let current = stack.Pop ()
        getChildren(current,maze.maze) 
        for c in current.children do
            let childTree = {
                parent = Some current
                value = c
                children = []
            }
            if childTree.value.isExit then final <- Some childTree
            else
                stack.Push (childTree)
    
    buildPath (final,maze)

let rec findHierarchy (maze: Maze, queue: Queue<Tree>) =
    if queue.Count = 0 then None
    else
        let root = queue.Dequeue()
        getChildren (root,maze.maze)

        match root.children with
        | child::xs -> if child.isExit then 
                            Some {parent = Some root; value = child; children = []}
                       else
                            queue.Enqueue({parent = Some root; value = child; children = []})
                            findHierarchy(maze,queue)
        | _ ->  maze.maze.[root.value.x,root.value.y].isDeadEnd <- true
                if not (root.parent = None) then
                    queue.Enqueue(root.parent.Value)
                else
                    queue.Enqueue(root)
                findHierarchy (maze,queue)

let rec solveRecursive (maze: Maze)=
    let tree = {
        parent = None; value = maze.start; children = []
    }
    let q = Queue<Tree>()
    q.Enqueue(tree)
    let endChild = findHierarchy (maze, q)
    buildPath (endChild, maze)

