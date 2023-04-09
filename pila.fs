namespace Busqueda

type pila<'a> = list<'a>

module Pila = 
    let empty = []
    let push pila x = x :: pila //Concatenar al final
    let pop pila = 
        match pila with
        | h :: t -> Some(h, t)
        | [] -> None
    