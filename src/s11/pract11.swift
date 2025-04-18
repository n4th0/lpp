import Foundation
// struct MiEstructura {
//     var x = 0
// }
//
// class MiClase {
//     var x = 0
// }
//
// func foo(_ c: MiClase, _ x: Int) -> MiClase {
//     let n = MiClase()
//     n.x = x
//     return n
// }
//
// var s1 = MiEstructura()
// var s2 = s1
//
// var c1 = MiClase()
// var c2 = c1
//
// s1.x = 10
// c1.x = 10
// // print ("s2.x: \(s2.x), c2.x: \(c2.x)")
//
// c1 = foo(c1, 20)
// // print("c1.x, después de llamar a la función: \(c1.x)")
//
//
// struct Coord {
//     var x: Double
//     var y: Double
//
//     func movida(incX: Double, incY: Double) -> Coord {
//         return Coord(x: x+incX, y: y+incY)
//     }
//
//     mutating func mueve(incX: Double, incY: Double) {
//         x = x + incX
//         y = y + incY
//     }
// }
//
// struct Cuadrado {
//     var esquina = Coord(x: 0.0, y: 0.0)
//     var lado: Double
//
//     func movido1(_ cuadrado: Cuadrado, _ x: Double, _ y: Double) -> Cuadrado {
//         return Cuadrado(esquina: cuadrado.esquina.movida(incX: x, incY: y), lado: cuadrado.lado)
//     }
//
//     func movido2(_ cuadrado: Cuadrado, _ x: Double, _ y: Double) -> Cuadrado {
//         var esq = cuadrado.esquina
//         esq.mueve(incX: x, incY: y)
//         return Cuadrado(esquina: esq, lado: cuadrado.lado)
//     }
//
//     mutating func mueve(_ x: Double, _ y: Double){
//         esquina.mueve(incX: x, incY: y)
//     }
// }
//
//
//
//
//
// func foo(palabra: String, longitud: Int) -> Bool {
//     return palabra.count >= longitud 
// }
//
// class MisPalabras {
//     var guardadas: [String] = []
//     func guarda(_ palabra: String) {
//         guardadas.append(palabra)
//     }
//     var x : [Bool] {
//         get {
//             return guardadas.map {foo(palabra: $0,longitud: 4)}
//         }
//     } 
// }
//
// let palabras = MisPalabras()
// palabras.guarda("Ana")
// palabras.guarda("Pascual")
// palabras.guarda("María")
// // print(palabras.x)
//
// var x = 10  {
//    didSet {
//       if (x > 100) {
//           x = oldValue
//       }
//    }
// }
//
// var y: Int {
//     get {
//         return x / 3
//     }
//     set {
//         x = 3 * newValue
//     }
// }
//
// var z : Int {
//    get {
//       return x + y
//    }
//    set {
//       x = newValue / 2
//       y = newValue / 2
//    }
// }

// z = 60
// print("y: \(y)") // 90
// print("x: \(x)") // 30
// z = 600
// print("y: \(y)") // 90
// print("x: \(x)") // 30

// struct Valor {
//     var valor: Int = 0 {
//         willSet {
//             Valor.z += newValue
//         }        
//         didSet {
//             if valor > 10 {
//                 valor = 10
//             }
//         }
//     }
//     static var z = 0
// }
// var c1 = Valor()
// var c2 = Valor()
// c1.valor = 20 // valor = 10 z = 20
// c2.valor = 8 // valor 8  z =  28
// // print(c1.valor + c2.valor + Valor.z) // 10 + 8 + 28 = 46




struct Equipo{
    private let name: String
    private var points: Int

    mutating func wins(points: Int){
        self.points =  points + self.points
    }

    init(_ name: String){
        self.name = name
        points = 0
    }

    func getName() -> String {
        return name
    }
    func getPoints() -> Int {
        return points
    }
    func toString() -> String {
        return name + ": " + String(points) + " puntos"
    }

}



struct Partido{
    private let e1: Equipo
        private let e2: Equipo

        func getE1() -> Equipo {
            return e1
        }
    func getE2() -> Equipo {
        return e2
    }

    private let g1: Int
        private let g2: Int

        init(e1:Equipo, e2:Equipo, g1:Int, g2:Int){
            self.e1 = e1
                self.e2 = e2
                self.g1 = g1
                self.g2 = g2
        }

        func getResult() -> (Equipo, Equipo?) {

            if g1 < g2 {
                return (e2, nil)
            }
            if g1 > g2 {
                return (e1, nil)
            }
            return (e1, e2)
        }

    func toString() -> String {
        return e1.getName() + " " + String(g1) + " - " + e2.getName() + " " + String(g2)
    }
}



struct Liga{
        private var teams: [Equipo]
        private var matches: [Partido]

        mutating func addMatches(matches: [Partido]) {
            for match in matches {
                let t = match.getResult()
                if t.1 == nil{
                    for i in 0...(teams.count-1) { // it probably dont work
                        if t.0.getName() == teams[i].getName() { 
                            teams[i].wins(points: 3)
                        }
                    }
                }else{
                    for i in 0...(teams.count-1) { // it probably dont work
                        if t.0.getName() == teams[i].getName() || t.1!.getName() == teams[i].getName() { 
                            teams[i].wins(points: 1)
                        }
                    }
                    }

            }
            self.matches = self.matches + matches
        }
        
        init(teams: [Equipo]){
            self.teams = teams
            matches = []
            }

        func printTeams(){
                for i in teams{
                        print(i.toString())
                    }
            }
    }



let equipos:[Equipo] = [ 
Equipo("Real Madrid"),
Equipo("Barcelona"),
Equipo("Atlético Madrid"),
Equipo("Valencia"),
Equipo("Atlétic Bilbao"),
Equipo("Sevilla")]

let results:[Partido] = [ 
Partido(e1: equipos[0], e2: equipos[1], g1: 0, g2: 3), 
Partido(e1: equipos[5], e2: equipos[4], g1: 1, g2: 1), 
Partido(e1: equipos[3], e2: equipos[2], g1: 2, g2: 1)]

var santander = Liga(teams: equipos)

// print("------------------------------")
// santander.printTeams()
// print("------------------------------")
// santander.addMatches(matches: results)
// santander.printTeams()
// print("------------------------------")



class Animal{
    let name: String
    var paws: Int
    var age: Int
    static let animal = true
 
    init(){
            name = "pepe"
            age = 10
            paws = 4
    }
 
    init(name:String, paws:Int, age:Int){
        self.name = name
        self.paws = paws
        self.age = age
    }
 
    func toString() -> String {
        return "Animal: " + name + " with " + String(paws) + " paws and " + String(age) + " age"
    }

}

class Bird : Animal{

    override var age :Int {
        didSet {
            // self.age = oldValue
            print("seteo el valor de age")
        }
    }

    override init(){
        super.init()
        self.paws = 2
        super.age = 10
    }

    override func toString() -> String {
        super.toString() + " y es un pájaro"
    }
}

var a = Animal()

// var b = Bird()

// b.age = 30

// print(a.toString())
// print(b.toString())
// print(Bird.animal)




enum MarcaCoche: Int {
    case Mercedes=0, Ferrari, RedBull, McLaren

    static func random() -> MarcaCoche {
        let maxValue = McLaren.rawValue

        let r = Int.random(in: 0...maxValue)
        return MarcaCoche(rawValue: r)!
    }

}


enum TipoCambio: Int {
    case Automatico=0, Manual

    static func random() -> TipoCambio {
        let maxValue = Manual.rawValue

        let r = Int.random(in: 0...maxValue)
        return TipoCambio(rawValue: r)!
    }
}


class Coche{
    var velocidadActual: Double
    var distanciaRecorrido: Double
    var marcha: Int
    var marca: MarcaCoche = MarcaCoche.random()
    var descripcion: String 

    static let velocidadMaxima: Double = 150.0
    static let marchaMaxima: Int = 6

    init(){
        descripcion = "\(marca)"
        marcha = 1
        distanciaRecorrido = 0
        velocidadActual = 1
    }
    func go(){
    }
}



class CocheAutomatico : Coche{
    override var velocidadActual: Double {
        didSet {
            distanciaRecorrido += velocidadActual

            print(self.descripcion+" he rrecorrido \(distanciaRecorrido)")
            marcha = min(Int(velocidadActual / 25.0) + 1, Coche.marchaMaxima)
        }
    }

    override init(){
        super.init()
        self.descripcion = super.descripcion + " Automático"
    }
    override func go() {
        velocidadActual = velocidadActual + Double.random(in: 20...40)
    }
}


class CocheManual : Coche{
    override var marcha: Int{
        willSet{
            distanciaRecorrido += velocidadActual
            print(self.descripcion+" he rrecorrido \(distanciaRecorrido)")
            velocidadActual = min(25.0 * Double(newValue), Coche.velocidadMaxima)
        }
    }

    override init(){
        super.init()
        self.descripcion = super.descripcion + " Manual"
    }
    override func go() {
        if Bool.random() {
            marcha += 1
        }else {
            marcha = 0
        }
    }
}

class Carrera{
    var coches: [Coche]
    var horas: Int
    init(numCoches: Int, horas:Int){
        self.horas = horas
        coches = []
        for _ in 0...numCoches-1 {
            var c:Coche
            if Int.random(in: 0...1) == 0{
                c = CocheAutomatico()
            }else{
                c = CocheManual()
            }

            print(c.descripcion)
            coches += [c]
        }
    }
    func descripcion() {
        print("\(coches.count) coches con una duracion de \(horas) horas")
    }

    func empezar() {
        for i in 1...horas {
            print("hora " + String(i))
            for car in coches {
                car.go()
            }
        }
    }

    func clasificacion() {
        coches.sort { $0.distanciaRecorrido > $1.distanciaRecorrido }
        for (i, car) in coches.enumerated() {
            print("\(i+1). "+car.descripcion)
        }
    }

}

// let carrera = Carrera(numCoches: 2, horas: 3)
// print("\nDescripción de la carrera:")
// carrera.descripcion()
// print("\n!!! Comienza la carrera !!!")
// carrera.empezar()
// print("\n!!! Clasificación !!!")
// carrera.clasificacion()





struct Punto {
    var x = 0.0, y = 0.0
}
struct Tamaño {
    var ancho = 0.0, alto = 0.0
}


class Figura {
    var punto:Punto
    var tamaño:Tamaño

    var area: Double {
        get{
            return tamaño.ancho * tamaño.alto
        }
    }

    var centro: Punto {
        get{
            var p = Punto()
            p.x = punto.x + tamaño.ancho/2
            p.y = punto.y + tamaño.alto/2
            return p
        }

        set{
            punto.x = newValue.x - tamaño.ancho/2
            punto.y = newValue.y - tamaño.alto/2
        }
    }

    init(origen: Punto, tamaño: Tamaño){
        punto = origen
        self.tamaño = tamaño
    }
}

class Cuadrilatero: Figura{
    var p1:Punto
    var p2:Punto
    var p3:Punto
    var p4:Punto

    override var centro: Punto {
        get{
            var p = Punto()
            p.x = punto.x + tamaño.ancho/2
            p.y = punto.y + tamaño.alto/2
            return p
        }

        set{
            var v1 = Punto() // deberían ser vectores xd
            var v2 = Punto()
            var v3 = Punto()
            var v4 = Punto()

            v1.x = p1.x - punto.x // se calculan los vectores
            v1.y = p1.y - punto.y // normalizados desde p
            v2.x = p2.x - punto.x
            v2.y = p2.y - punto.y
            v3.x = p3.x - punto.x
            v3.y = p3.y - punto.y
            v4.x = p4.x - punto.x
            v4.y = p4.y - punto.y

            punto.x = newValue.x - tamaño.ancho/2
            punto.y = newValue.y - tamaño.alto/2

            p1.x = v1.x+punto.x
            p1.y = v1.y+punto.y
            p2.x = v2.x+punto.x
            p2.y = v2.y+punto.y
            p3.x = v3.x+punto.x
            p3.y = v3.y+punto.y
            p4.x = v4.x+punto.x
            p4.y = v4.y+punto.y
        }
    }

    override var area: Double {
        get{
            return Cuadrilatero.areaTriangulo(p1, p2, p4) + Cuadrilatero.areaTriangulo(p1, p3, p4)
        }
    }

    init(p1:Punto, p2:Punto, p3:Punto, p4:Punto){
        var p = Punto()
        p.x = min(p1.x, p2.x, p3.x, p4.x)
        p.y = min(p1.y, p2.y, p3.y, p4.y)
        var t = Tamaño()

        t.ancho = max(p1.x, p2.x, p3.x, p4.x) - min(p1.x, p2.x, p3.x, p4.x)
        t.alto = max(p1.y, p2.y, p3.y, p4.y) - min(p1.y, p2.y, p3.y, p4.y)

        self.p1 = p1
        self.p2 = p2
        self.p3 = p3
        self.p4 = p4
        super.init(origen: p, tamaño: t)
    }

    static func areaTriangulo(_ p1: Punto, _ p2: Punto, _ p3: Punto) -> Double {
        let det = p1.x * (p2.y - p3.y) + p2.x * (p3.y - p1.y) + p3.x * (p1.y - p2.y)
        return abs(det)/2
    }

}




class Circulo : Figura{
    var radio:Double

    override var area: Double {
        get{
            return radio*radio*Double.pi
        }
    }

    init(centro: Punto, radio:Double){
        self.radio = radio
        var p = Punto()

        p.x = centro.x - radio
        p.y = centro.y - radio

        var t = Tamaño()
        t.ancho = radio*2
        t.alto = radio*2
        super.init(origen: p, tamaño: t)
    }
}

struct AlmacenFiguras{
    var figuras: [Figura]
    init(){
        figuras = []
    }

    var numFiguras:Int {
        get{
            return figuras.count
        }
    }

    var areaTotal:Double{
        get{
            var total = 0.0
            for i in figuras {
                total += i.area
            }

            return total
        }
    }

    mutating func añade(figura: Figura) {
        figuras = figuras + [figura]
    }

    func desplaza(incX:Double, incY:Double) {
        for i in figuras {
            var p = i.centro
            p.x += incX
            p.y += incY
            i.centro = p
        }
    }
}


// TODO test the classes
