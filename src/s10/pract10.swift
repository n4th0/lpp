import Foundation


// ej 1
func maxOpt(_ x: Int?, _ y: Int? ) -> Int? {
    if x == nil && y == nil{
            return nil
        }
    if x == nil{
            return y
        }
    if y == nil{
            return x
        }

    return max(x!, y!)
}

let res1 = maxOpt(nil, nil) 
let res2 = maxOpt(10, nil)
let res3 = maxOpt(-10, 30)

// print("res1 = \(String(describing: res1))")
// print("res2 = \(String(describing: res2))")
// print("res3 = \(String(describing: res3))")

// Imprime:
// res1 = nil
// res2 = Optional(10)
// res3 = Optional(30)

func parejaMayorParImpar2(numeros: [Int]) -> (Int?, Int?){
    if numeros.isEmpty {
        return (nil, nil)
    }else{
        let n = numeros[0]
            let tup = parejaMayorParImpar2(numeros: Array(numeros.dropFirst()))
            if n % 2 == 0{
                if tup.0 == nil{
                    return  (n, (tup.1))

                    }
                return  (max(n, tup.0!), (tup.1))
            }else{
                if tup.1 == nil{
                    return  (tup.0, n)
                    }
                return  (tup.0, max(n, tup.1!))
            }
    }
}
let numeros = [-10, 202, 12, 100, 204, 2]
// print("\n******\n1b1) Función parejaMayorParImpar2(numeros:)\n******")
// print(parejaMayorParImpar2(numeros: numeros))
// Imprime:
// parejaMayorParImpar2(numeros: [-10, 202, 12, 100, 204, 2])
// (nil, Optional(204))



func sumaMaxParesImpares(numeros: [Int]) -> Int {
    let tup = parejaMayorParImpar2(numeros: numeros)
    if tup.0 == nil && tup.1 == nil{
            return 0
        }

    if tup.0 == nil{
            return tup.1!
        }
    if tup.1 == nil{
            return tup.0!
        }
    return tup.0! + tup.1!
}
// print("sumaMaxParesImpares(numeros: \(numeros))")
// print(sumaMaxParesImpares(numeros: numeros))
// Imprime:
// sumaMaxParesImpares(numeros: [-10, 202, 12, 100, 204, 2])
// 204


// // ej 2
// let nums = [1,2,3,4,5,6,7,8,9,10]
// print(nums.filter{$0 % 3 == 0}.count) // cantidad de divisores de 3


// let nums2 = [1,2,3,4,5,6,7,8,9,10]
// print(nums2.map{$0+100}.filter{$0 % 5 == 0}.reduce(0,+)) // 105+ 110 = 215

// let cadenas = ["En", "un", "lugar", "de", "La", "Mancha"]
// print(cadenas.sorted{$0.count < $1.count}.map{$0.count}) // 2 2 2 2 5 6

// let cadenas2 = ["En", "un", "lugar", "de", "La", "Mancha"]
// print(cadenas2.reduce([]) {
//     (res: [(String, Int)], c: String) -> [(String, Int)] in
//         res + [(c, c.count)]}.sorted(by: {$0.1 < $1.1})) // [(2, "En"),(2, "un"), (2, "de"), (2,"La") ...] 


func f(nums: [Int], n: Int) -> Int { // numero de veces que aparece el elemento
    return nums.filter{$0 == n}.count
}

func g(nums: [Int]) -> [Int] { // devuelve el array sin elementos repetidos
    return nums.reduce([], {
        (res: [Int], n: Int) -> [Int] in
            if !res.contains(n) {
                return res + [n]
            } else {
                return res
            }
    })
}

func h(nums: [Int], n: Int) -> ([Int], [Int]) { // clasifica los numeros del array
                                                // dependiendo si son mayores o menores
   return nums.reduce(([],[]), {                // al numero pasado 
                                                // ([menores],[mayores||igual])
       (res: ([Int],[Int]), num: Int ) -> ([Int],[Int]) in
           if (num >= n) {
               return (res.0, res.1 + [num])
           } else {
               return ((res.0 + [num], res.1))
           }
   })
}


func suma(palabras: [String], contienen: Character) -> Int{

    palabras.filter{ $0.contains( contienen) }.map{ $0.count }.reduce(0, +)

    }
func sumaMenoresMayores(nums: [Int], pivote: Int) -> (Int, Int){
    nums.reduce((0, 0), {
            if $1 < pivote {
            return ($0.0 + $1, $0.1)
            }else{
            return ($0.0 , $0.1 + $1)
            }
            })
    }

// print(sumaMenoresMayores(nums: [1,2,3,4,5,6,7,8,9], pivote: 5))

func bar(f: (Int) -> Int) {
  print(f(4))
}

func foo() -> (Int) -> Int {
  var x = 3
  return {
    x += $0 + 2
    return x
  }
}

var x = 5
let g = foo()
// bar(f: g)   // => 9
// bar(f: g)   // => 15


indirect enum Arbol<T>{
    case nodo(T, [Arbol])

    }

func toArray<T>(arb: Arbol<T>) -> [T]{
    switch arb{
        case let .nodo(x, y):
            if y.isEmpty {
                return [x]
            }
            return [x] + toArray(bosque: y)
    }
}

func toArray<T>(bosque: [Arbol<T>]) -> [T]{
    if bosque.isEmpty {
        return []
    }
    return toArray(arb: bosque[0]) + toArray(bosque: Array(bosque.dropFirst()))
}
func toArrayFOS<T>(arb: Arbol<T>) -> [T]{
    switch arb{
            case let .nodo(x, y):
            return [x] + y.map(toArrayFOS).reduce([], +)
        }
}

let arbolInt: Arbol = .nodo(53, 
                            [.nodo(13, []), 
                             .nodo(32, []), 
                             .nodo(41, 
                                   [.nodo(36, []), 
                                    .nodo(39, [])
                                   ])
                            ])
let arbolString: Arbol = .nodo("Zamora", 
                               [.nodo("Buendía", 
                                      [.nodo("Albeza", []), 
                                       .nodo("Berenguer", []), 
                                       .nodo("Bolardo", [])
                                      ]), 
                                .nodo("Galván", [])
                               ])


// print(toArray(arb: arbolInt))
// print(toArrayFOS(arb: arbolString))


func calculateMedia(_ p1: Double,_ p2: Double,_ p3:Double) -> Double{
        return 0.35*p1 + 0.3*p2 + 0.35*p3
    }

func imprimirListadoAlumnos(_ alumnos: [(String, Double, Double, Double, Int)]) {
    print("Alumno   Parcial1   Parcial2   Parcial3  Años")
    for alu in alumnos {
        alu.0.withCString {
            print(String(format:"%-10s %5.2f      %5.2f    %5.2f  %3d", $0, alu.1,alu.2,alu.3,alu.4))
        }
    }
}
func imprimirListadosNotas(_ alumnos: [(String, Double, Double, Double, Int)]) -> Void {
    imprimirListadoAlumnos(alumnos.sorted{ $0.0 < $1.0 })
    imprimirListadoAlumnos(alumnos.sorted{ $0.1 > $1.1 })
    imprimirListadoAlumnos(alumnos.sorted{ $0.2 < $1.2 })
    imprimirListadoAlumnos(alumnos.sorted{ $0.3 < $1.3 && $0.4 < $1.4 })
    imprimirListadoAlumnos(alumnos.sorted{ calculateMedia($0.1, $0.2, $0.3) < calculateMedia($1.1, $1.2, $1.3) })
}

let listaAlumnos = [("Pepe", 8.45, 3.75, 6.05, 1), 
                    ("Maria", 9.1, 7.5, 8.18, 1), 
                    ("Jose", 8.0, 6.65, 7.96, 1),
                    ("Carmen", 6.25, 1.2, 5.41, 2), 
                    ("Felipe", 5.65, 0.25, 3.16, 3), 
                    ("Carla", 6.25, 1.25, 4.23, 2), 
                    ("Luis", 6.75, 0.25, 4.63, 2), 
                    ("Loli", 3.0, 1.25, 2.19, 3)]
// imprimirListadosNotas(listaAlumnos)


// print(listaAlumnos.filter{ $0.1 >= 5 && $0.2 < 5}.count)
// Resultado: 5

// print(listaAlumnos.filter { calculateMedia($0.1, $0.2, $0.3) >= 5 }.map { $0.0 } )
// Resultado: ["Pepe", "Maria", "Jose"]


// var tupla = listaAlumnos .reduce( (0, 0, 0), { ($0.0 + $1.1, $0.1 + $1.2, $0.2 + $1.3) } ) 
// tupla = (tupla.0 / Double(listaAlumnos.count), tupla.1 / Double(listaAlumnos.count), tupla.2 / Double(listaAlumnos.count))
// print(tupla)
// Resultado: (6.6812499999999995, 2.7624999999999997, 5.22625)



 func construye(operador: Character) -> (Int, Int) -> Int{
    switch operador{
            case "+":
            return {
                    $0 + $1
                }
            case "-":
            return {
                    $0 - $1
                }
            case "*":
            return {
                    $0 * $1
                }
            default:
            return {
                    return $0 / $1
                }
        }
    }

var f = construye(operador: "+")
// print(f(2,3))
// Imprime 5
f = construye(operador: "-")
// print(f(2,3))
// Imprime -1
