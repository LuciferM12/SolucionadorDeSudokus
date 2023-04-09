open Busqueda
open System
let estado = [
    [2; 5; 0; 0; 3; 0; 9; 0; 1];
    [0; 1; 0; 0; 0; 4; 0; 0; 0];
    [4; 0; 7; 0; 0; 0; 2; 0; 8];
    [0; 0; 5; 2; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 9; 8; 1; 0; 0];
    [0; 4; 0; 0; 0; 3; 0; 0; 0];
    [0; 0; 0; 3; 6; 0; 0; 7; 2];
    [0; 7; 0; 0; 0; 0; 0; 0; 3];
    [9; 0; 3; 0; 0; 0; 6; 0; 4]
]

let estado3 = [
    [0; 2; 0; 0; 0; 0; 0; 0; 9];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 5; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 8; 0; 0; 0; 0; 0; 6; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0]
]

let t0 = System.DateTime.UtcNow
match capitulo3.busquedaArbol DFS.estrategia (Sudoku.problema estado3) with
    | Some n -> 
        let sol = capitulo3.acciones n
        printfn "Solucion: %A" n.estado
    | None -> printfn "Solucion no encontrada :("
let delta = System.DateTime.UtcNow - t0
printfn "Tiempo transcurrido: %A " delta
