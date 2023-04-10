namespace Busqueda

type problema<'s, 'a> = {
    inicio      : 's
    sucesores   : 's -> list<'a * 's>
    meta        : 's -> bool
    costo       : 's -> 'a -> 's -> float
}
type nodo<'s, 'a> = {
    depth           : int
    costo_ruta      : float
    estado          : 's
    accion          : option<'a>
    padre           : option<nodo<'s, 'a>>
}
type estrategia<'s, 'a, 'd> = {
    vacia       : 'd
    insertar    : 'd -> nodo<'s, 'a> -> 'd
    remover     : 'd -> option<nodo<'s, 'a> * 'd>
}
module capitulo3 =
    let mutable nodos = 0
    let nodoInicial estado = {
            estado = estado
            depth = 0
            costo_ruta = 0.0
            accion = None
            padre = None
        }
    let rec map f xs =
        match xs with
        | [] -> []
        | h :: t -> f h :: map f t
    let expandir problema padre =
        nodos <- nodos + 1
        problema.sucesores padre.estado
        |> List.map (fun (a,s) -> {
            depth = padre.depth + 1
            estado = s
            accion = Some a
            padre = Some padre
            costo_ruta = padre.costo_ruta + problema.costo padre.estado a s    
        })

    let busquedaArbol estrategia problema = 
        let raiz = nodoInicial problema.inicio
        let bolsa = estrategia.insertar
                        estrategia.vacia
                        raiz
        let rec loop bolsa = 
            match estrategia.remover bolsa with
            | Some (n, bolsa') -> 
                if problema.meta n.estado then Some n else
                                                    expandir problema n
                                                    |> List.fold estrategia.insertar bolsa'
                                                    |> loop
            | None -> None
        loop bolsa

    let busquedaGrafo key estrategia problema = 
        let raiz = nodoInicial problema.inicio
        let bolsa = estrategia.insertar
                        estrategia.vacia
                        raiz
        let rec loop (bolsa, procesado) = 
            match estrategia.remover bolsa with
            | Some (n, bolsa') -> 
                if problema.meta n.estado then Some n else
                                                        if Set.contains (key n) procesado
                                                        then loop (bolsa', procesado)
                                                        else 
                                                            expandir problema n
                                                            |> List.fold estrategia.insertar bolsa'
                                                            |> (fun bolsa -> loop (bolsa, Set.add (key n) procesado))
            | None -> None
        loop (bolsa, Set.empty)
    
    let rec acciones n =
        match n.accion, n.padre with
        |Some a, Some p -> acciones p @ [a]
        |_ -> []

module capitulo4 =
    open capitulo3
    open System
    let acensionColina h problema =
        let actual = nodoInicial problema.inicio
        let rec loop actual =
            let sucesores = expandir problema actual
            let vecino = List.minBy h sucesores
            if h vecino >= h actual then
                actual
            else
                loop vecino
        loop actual
    
    let temperatura T0 lamda t =
        let T = T0 * Math.Exp(-lamda * t)
        if T < 1E-6 then 
            0.0
        else
            T

    let recocidoSimulado seed h temperatura problema =
        let rnd = System.Random(seed)
        let actual = nodoInicial problema.inicio
        let rec loop (t, atual) =
            let T = temperatura t
            if T = 0.0 then
                actual
            else
                let sucesores = expandir problema actual
                let i = rnd.Next(List.length sucesores)
                let next = List.item i sucesores
                let delta = h next - h actual
                if delta < 0.0 || rnd.NextDouble() <= Math.Exp (delta / T) then 
                    loop (t + 1.0, next)
                else
                    loop (t + 1.0, actual)
        loop (1.0, actual)

