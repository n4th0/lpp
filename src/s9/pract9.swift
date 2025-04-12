

// ej 2

func prefijos(prefijo: String, palabras: [String]) -> [Bool] {
    if palabras.isEmpty {
        return []
    }else {
        return [palabras[0].hasPrefix(prefijo)] + 
        prefijos(prefijo: prefijo, palabras: Array(palabras.dropFirst()))
    }
}


// print(prefijos(prefijo: "ho", palabras: arr))


func parejaMayorParImpar(numeros: [Int]) -> (Int, Int){
    if numeros.isEmpty {
        return (0, 0)
    }else{
        let n = numeros[0]
            let tup = parejaMayorParImpar(numeros: Array(numeros.dropFirst()))
            if n % 2 == 0{
                return  (max(n, tup.0), (tup.1))
            }else{
                return  (tup.0, max(n, tup.1))
            }
    }
}

// let numeros = [10, 201, 12, 103, 204, 2]
// print("\n******\n2b) Función parejaMayorParImpar(numeros:)\n******")
// print(parejaMayorParImpar(numeros: numeros))
// // Imprime: (201, 204)


// ej 3

func compruebaParejas(_ ar:[Int], funcion: (Int) -> Int ) -> [(Int, Int)] {
    
    if ar.isEmpty || ar.dropFirst().isEmpty{
        return []
        }

    let result = funcion(ar[0])

    if result == ar[1]{
        return [(ar[0], result)] + compruebaParejas(Array(ar.dropFirst()),funcion: funcion)
        }

    return compruebaParejas(Array(ar.dropFirst()), funcion: funcion)
}

func cuadrado(x: Int) -> Int {
   return x * x
}
// print(compruebaParejas([2, 4, 16, 5, 10, 100, 105], funcion: cuadrado))
// Imprime [(2,4), (4,16), (10,100)]

func coinciden(parejas: [(Int, Int)], funcion: (Int) -> Int) -> [Bool] {

if parejas.isEmpty{
        return []
    }
    return [parejas[0].1 == funcion(parejas[0].0)] + coinciden(parejas:Array(parejas.dropFirst()), funcion:funcion) 
}

let array = [(2,4), (4,14), (4,16), (5,25), (10,100)]
// print(coinciden(parejas: array, funcion: cuadrado))
// Imprime: [true, false, true, true, true]

enum Movimiento {
    case deposito (Double)
    case cargoRecibo (String, Double)
    case cajero (Double)
    }

func aplica(movimientos: [Movimiento]) -> (Double, [String]) {
    if movimientos.isEmpty{
            return (0, [])
        }

    let tup = aplica(movimientos: Array(movimientos.dropFirst()))

    switch movimientos[0]{
        case let .cajero(x):
        return (tup.0 - x, tup.1)
        case let .deposito(x):
        return (tup.0 + x, tup.1)
        case let .cargoRecibo(x, y):
        return (tup.0 - y, [x] + tup.1)
        }
}

let movimientos: [Movimiento] = [.deposito(830.0), .cargoRecibo("Gimnasio", 45.0), .deposito(400.0), .cajero(100.0), .cargoRecibo("Fnac", 38.70)]

// print(aplica(movimientos: movimientos))
//Imprime (1046.3, ["Gimnasio", "Fnac"])


// ej 5
indirect enum ArbolBinario{
        case nodo(Int, ArbolBinario, ArbolBinario)
        case vacio
    }


func suma(arbolb: ArbolBinario) -> Int {
    switch arbolb{
            case let .nodo(elem, iz, der):
            return elem + suma(arbolb: iz) + suma(arbolb: der)
            case .vacio:
            return 0
        }
}

let arbol: ArbolBinario = .nodo(8, 
                                .nodo(2, .vacio, .vacio), 
                                .nodo(12, .vacio, .vacio))

// print(suma(arbolb: arbol))
// Imprime: 22


// ej 6

indirect enum Arbol{
        case nodo(Int, [Arbol])
    }

func suma(arbol: Arbol, cumplen: (Int) -> Bool) -> Int {
    switch arbol{
        case let .nodo(elem, bosque):
            let cumple =  cumplen(elem)

            if cumple && bosque.isEmpty {
                return elem
            }
            if cumple {
                return elem + suma_bosque(bosque: bosque, cumplen:cumplen)
            }

            if bosque.isEmpty{
                return 0
            }else{
                return  suma_bosque(bosque: bosque, cumplen:cumplen)
            }
    }
}

func suma_bosque(bosque: [Arbol], cumplen: (Int) -> Bool) -> Int {
    if bosque.isEmpty {
        return 0
    }
    return suma(arbol: bosque[0], cumplen: cumplen) + 
    suma_bosque(bosque: Array(bosque.dropFirst()), cumplen: cumplen) 
}

/*
Definimos el árbol

    10
   / | \
  3  5  8
  |
  1

*/

let arbol1 = Arbol.nodo(1, [])
let arbol3 = Arbol.nodo(3, [arbol1])
let arbol5 = Arbol.nodo(5, [])
let arbol8 = Arbol.nodo(8, [])
let arbol10 = Arbol.nodo(10, [arbol3, arbol5, arbol8])
func esPar(x: Int) -> Bool {
    return x % 2 == 0
}

// print("La suma del árbol es: \(suma(arbol: arbol10, cumplen: esPar))")
// Imprime: La suma del árbol genérico es: 18
