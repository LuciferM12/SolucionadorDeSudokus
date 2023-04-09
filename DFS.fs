namespace Busqueda

module DFS =
    
    let estrategia = {
        vacia = Pila.empty
        insertar = Pila.push
        remover = Pila.pop
    }

    let key n = n.estado