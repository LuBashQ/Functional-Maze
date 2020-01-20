module LabProg2019.TreeMaze

open System.Collections.Generic
open Maze


/// <summary>
/// Tipo Tree
/// </summary>
/// <param name="parent">Il nodo padre</param>
/// <param name="value">La cella contenuta nel nodo</param>
/// <param name="children">I figli della cella</param>
[<NoComparison>]
type Tree = {
    parent : Tree option
    value : Cell
    mutable children : Cell list
}

/// <summary>
/// Ricava tutti i figli di un nodo dell'albero
/// </summary>
/// <param name="node">Il nodo dell'albero</param>
/// <param name="maze">La matrice che identifica il labirinto</param>
let getChildren (node: Tree, maze: Cell[,]) =
    if (node.parent = None) then node.children <- node.value.children(maze,node.value)
    else node.children <- node.value.children(maze,node.parent.Value.value)


/// <summary>
/// Ricava ricorsivamente tutte le celle padre della foglia corrispondente all'uscita del labirinto
/// </summary>
/// <param name="leaf">La foglia dell'albero</param>
/// <returns>Una lista di celle</returns>
let rec retrievePath (leaf: Tree option) =
    if (leaf.Value.parent = None) then [leaf.Value.value]
    else leaf.Value.value::retrievePath(leaf.Value.parent)

/// <summary>
/// Modifica le celle della matrice in modo da rapprensentare la strada 
/// </summary>
/// <param name="root">La radice dell'albero</param>
/// <param name="maze">La struttura dati del labirinto</param>
/// <returns>la struttura dati del labirinto modificata</returns>
let buildPath (root: Tree option, maze: Maze) =
    if root = None then 
        maze
    else
        let path = retrievePath (root)
        for p in path do
            maze.maze.[p.x,p.y].isPath <- true
        maze

/// <summary>
/// Algoritmo iterativo di risoluzione del labirinto basato sull'utilizzo di un albero n-ario
/// </summary>
/// <param name="maze">La struttura dati del labirinto</param>
/// <returns>la struttura dati del labirinto modificata</returns>
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
/// Algoritmo di ricerca in profondità di un albero n-ario, usato per generare la gerarchia padre-figlio
/// </summary>
/// <param name="maze">La struttura dati del labirinto</param>
/// <param name="queue">Una coda</param>
/// <returns>La foglia corrispondente all'uscita del labirinto</returns>
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
/// Algoritmo ricorsivo di risoluzione del labirinto, usando un albero n-ario
/// </summary>
/// <param name="maze">La struttura dati del labirinto</param>
/// <returns>la struttura dati del labirinto modificata</returns>
let rec solveRecursive (maze: Maze)=
    let tree = {
        parent = None; value = maze.start; children = []
    }
    let q = Queue<Tree>()
    q.Enqueue(tree)
    let endChild = findHierarchy (maze, q)
    buildPath (endChild, maze)

