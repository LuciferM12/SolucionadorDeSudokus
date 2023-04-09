namespace Busqueda

type cola<'a> = list<'a>

module Cola = 
    let empty = []
    let enqueue cola x = cola @ [x] //Concatenar al final
    let dequeue cola = 
        match cola with
        | h :: t -> Some(h, t)
        | [] -> None
    