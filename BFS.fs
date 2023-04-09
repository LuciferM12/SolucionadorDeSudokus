namespace Busqueda

module BFS =
    
    let estrategia = {
        vacia = Cola.empty
        insertar = Cola.enqueue
        remover = Cola.dequeue
    }

    let key n = n.estado