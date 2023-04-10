namespace Busqueda

module Sudoku =
    //Acciones permitidas
    type accion =
        | Poner
    //Estructura del estado
    type estado = List<List<int>>
    //Costo por accion
    let costo _ _ _ = 1.0
    //Encontrar el primer 0 de arriba a abajo y de derecha a izquierda
    let cero estado = List.findIndex( fun x -> List.exists(fun y -> y = 0) x) estado, List.findIndex (fun z -> z = 0) (List.item (List.findIndex( fun x -> List.exists(fun y -> y = 0) x) estado) estado)
    //Verificacion de que haya 0 en el estado actuarl para evaluar si nos encontramos en un estado meta
    let meta estado = not (List.exists(fun x -> List.exists (fun y -> y = 0) x) estado)
    //Funcion que genera los estados sucesores del nodo actual
    let sucesores estado =
        //Unica accion posible
        let accion = Poner
        //Encontrar un 0
        let (i, j) = cero estado
        //Posibles valores de cada casilla
        let posibles = [1; 2; 3; 4; 5; 6; 7; 8; 9]
        //Obtencion el valor de la casilla
        let casilla = estado.[i].[j]
        //Obtencion de la fila de la casilla actual
        let fila = List.item i estado
        //Obtencion de la columna de la casilla actial
        let columna = List.item j (List.transpose estado)
        //Obtencion de la cuadricula de la casilla actual
        let cuadrilla = 
            let iIndex = (i / 3) * 3
            let jIndex = (j / 3) * 3
            List.concat (List.map (fun x -> List.take 3 (List.skip jIndex x)) (List.take 3 (List.skip iIndex estado)))
        //Generacion de los valores permitidos de acuerdo a las restricciones de la casilla
        let opciones =  List.filter (fun x -> (not (List.contains x fila) &&  not (List.contains x columna) && not (List.contains x cuadrilla))) posibles
        //Genaracion de la lista de sucesores
        match accion with
        |Poner -> 
            if casilla = 0 && opciones.Length <> 0 then
                seq {
                    for pos in opciones do
                        yield (accion, List.updateAt i (List.updateAt j pos (estado.[i])) estado)
                } |> Seq.toList
            else []
    //Definicion del problema
    let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }