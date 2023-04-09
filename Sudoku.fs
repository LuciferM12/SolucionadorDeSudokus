namespace Busqueda

module Sudoku =
    type accion =
        | Poner

    type estado = List<List<int>>

    let costo _ _ _ = 1.0
    
    let cero estado = List.findIndex( fun x -> List.exists(fun y -> y = 0) x) estado, List.findIndex (fun z -> z = 0) (List.item (List.findIndex( fun x -> List.exists(fun y -> y = 0) x) estado) estado)

    let meta estado = not (List.exists(fun x -> List.exists (fun y -> y = 0) x) estado)

    let sucesores estado =
        let accion = Poner
        let (i, j) = cero estado
        let posibles = [1; 2; 3; 4; 5; 6; 7; 8; 9]
        let casilla = estado.[i].[j]
        let fila = List.item i estado
        let columna = List.item j (List.transpose estado)
        let cuadrilla = 
            let iIndex = (i / 3) * 3
            let jIndex = (j / 3) * 3
            List.concat (List.map (fun x -> List.take 3 (List.skip jIndex x)) (List.take 3 (List.skip iIndex estado)))
        let opciones =  List.filter (fun x -> (not (List.contains x fila) &&  not (List.contains x columna) && not (List.contains x cuadrilla))) posibles
        match accion with
        |Poner -> 
            if casilla = 0 && opciones.Length <> 0 then
                seq {
                    for pos in opciones do
                        yield (accion, List.updateAt i (List.updateAt j pos (estado.[i])) estado)
                } |> Seq.toList
            else []
    let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }