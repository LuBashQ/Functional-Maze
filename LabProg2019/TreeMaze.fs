module LabProg2019.TreeMaze

open System.Collections.Generic
open Maze


/// <summary>
/// Type Tree
/// </summary>
/// <param name="parent">Parent node</param>
/// <param name="value">Cell in the node</param>
/// <param name="children">Cell children</param>
[<NoComparison>]
type Tree = {
    parent : Tree option
    value : Cell
    mutable children : Cell list
}

/// <summary>
/// Get all children of a tree node
/// </summary>
/// <param name="node">Tree node</param>
/// <param name="maze">Matrix that identify the maze</param>
let getChildren (node: Tree, maze: Cell[,]) =
    if (node.parent = None) then node.children <- node.value.children(maze,node.value)
    else node.children <- node.value.children(maze,node.parent.Value.value)


/// <summary>
/// Get recursively all parent cell of the maze exit leaf
/// </summary>
/// <param name="leaf">Tree leaf</param>
/// <returns>Cell list</returns>
let rec retrievePath (leaf: Tree option) =
    if (leaf.Value.parent = None) then [leaf.Value.value]
    else leaf.Value.value::retrievePath(leaf.Value.parent)

/// <summary>
/// Modify the matrix cells to modify the path
/// </summary>
/// <param name="root">Tree root</param>
/// <param name="maze">Maze data structure</param>
/// <returns>Modified maze data structure</returns>
let buildPath (root: Tree option, maze: Maze) =
    if root = None then 
        maze
    else
        let path = retrievePath (root)
        for p in path do
            maze.maze.[p.x,p.y].isPath <- true
        maze

/// <summary>
/// Iterative solving algotihm based on n-ary tree
/// </summary>
/// <param name="maze">Maze data structure</param>
/// <returns>Maze modified data structure</returns>
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


/// <summary>
/// In-depth searching algorithm of n-ary tree, used to generate the hierarchy parent-child
/// </summary>
/// <param name="maze">Maze data structure</param>
/// <param name="queue">A tail</param>
/// <returns>The leaf corresponding to the maze exit</returns>
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


/// <summary>
/// Recursive solving algorithm for the maze, using n-ary tree
/// </summary>
/// <param name="maze">Maze data structure</param>
/// <returns>Modified maze data structure</returns>
let rec solveRecursive (maze: Maze)=
    let tree = {
        parent = None; value = maze.start; children = []
    }
    let q = Queue<Tree>()
    q.Enqueue(tree)
    let endChild = findHierarchy (maze, q)
    buildPath (endChild, maze)

