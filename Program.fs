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

let rec verTabla list = 
    list
    |> List.iter (fun list ->
        list
        |> List.iter (fun i ->
            printf "%A  " i
        )
        printfn ""
    )
let rec leer (i : int) (j : int) (list : int list)=
    match j with
    | 81 -> list
    | _->
        printf "Ingresa el valor (%A, %A) :" (i) (j%9)
        let num = System.Int32.TryParse (Console.ReadLine())
        match num with
        | (true, x) -> leer (j / 9) (j + 1) (list @ [x])
        | (false, x) -> 
            printf "------------------Valor invalido-----------------------\n"
            leer i j list

let estados = leer 0 0 [] |> List.chunkBySize 9
printfn "\n%A" estados
(*
let t0 = System.DateTime.UtcNow
match capitulo3.busquedaArbol DFS.estrategia (Sudoku.problema estado3) with
    | Some n -> 
        let sol = capitulo3.acciones n
        printfn "Solucion: %A" n.estado
    | None -> printfn "Solucion no encontrada :("
let delta = System.DateTime.UtcNow - t0
printfn "Tiempo transcurrido: %A " delta*)