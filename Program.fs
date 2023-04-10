open Busqueda
open System
//Casos de prueba
let casoPrueba1 = [
    [0; 6; 0; 1; 0; 4; 0; 5; 0];
    [0; 0; 8; 3; 0; 5; 6; 0; 0];
    [2; 0; 0; 0; 0; 0; 0; 0; 1];
    [8; 0; 0; 4; 0; 7; 0; 0; 6];
    [0; 0; 6; 0; 0; 0; 3; 0; 0];
    [7; 0; 0; 9; 0; 1; 0; 0; 4];
    [5; 0; 0; 0; 0; 0; 0; 0; 2];
    [0; 0; 7; 2; 0; 6; 9; 0; 0];
    [0; 4; 0; 5; 0; 8; 0; 7; 0]
]

let casoPrueba2 = [
    [0 ;0 ;0 ;0 ;0 ;4 ;9 ;0 ;0];
    [0 ;0 ;5 ;3 ;2 ;0 ;0 ;0 ;0];
    [2; 0; 0; 0; 0; 6; 0; 4; 0];
    [8; 0; 4; 0; 0; 0; 0; 6; 0];
    [0; 5; 0; 0; 6; 0; 0; 1; 0];
    [0; 1; 0; 0; 0; 0; 3; 0; 9];
    [0; 2; 0; 8; 0; 0; 0; 0; 6];
    [0; 0; 0; 0; 7; 9; 1; 0; 0];
    [0; 0; 9; 5; 0; 0; 0; 0; 0]
]

let casoPrueba3 = [
    [0; 0; 9; 0; 2; 8; 0; 0; 0];
    [0; 8; 0; 0; 0; 0; 9; 0; 0];
    [0; 7; 0; 0; 5; 0; 0; 0; 0];
    [0; 3; 8; 9; 0; 0; 1; 0; 5];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [6; 0; 4; 0; 0; 5; 2; 9; 0];
    [0; 0; 0; 0; 4; 0; 0; 6; 0];
    [0; 0; 6; 0; 0; 0; 0; 3; 0];
    [0; 0; 0; 7; 3; 0; 5; 0; 0]
]
//Funcion que imprime la tabla con formato
let rec verTabla list = 
    list
    |> List.iter (fun list ->
        list
        |> List.iter (fun i ->
            printf "%A  " i
        )
        printfn ""
    )
//Funcion para la entrada de datos
let rec leer (i : int) (j : int) (list : int list)=
    match j with
    | 81 -> List.chunkBySize 9 list
    | _->
        printf "Ingresa el valor (%A, %A) :" (i) (j%9)
        let num = System.Int32.TryParse (Console.ReadLine())
        match num with
        | (true, x) -> leer (j / 9) (j + 1) (list @ [x])
        | (false, x) -> 
            printf "------------------Valor invalido-----------------------\n"
            leer i j list
//Newthon raphson para aproximar la ramifcacion de la busqueda
let rec newton_Raphson N K b =
    let f = b ** (K + 1.0) + b * (1.0 - N) - 1.0
    let fp = (K + 1.0) * b ** K
    let bp = b - f / fp
    if abs(bp - b) < 0.00001 then bp else newton_Raphson N K bp

//Ejecucion de las estrategias con la entrada proporcionada
let t0 = System.DateTime.UtcNow
//match capitulo3.busquedaArbol DFS.estrategia (Sudoku.problema (leer 0 0 [])) with
match capitulo3.busquedaArbol DFS.estrategia (Sudoku.problema casoPrueba1) with
    | Some n -> 
        let sol = capitulo3.acciones n
        printfn "Solucion: %A" n.estado
    | None -> printfn "Solucion no encontrada :("
let delta = System.DateTime.UtcNow - t0
printfn "Tiempo transcurrido: %A " delta
