import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import common.parallel
import org.scalameter.*
import scala.util.Random
import Benchmark.*

val random = new Random()

//METODO DE COMPARACION DE ALGORITMOS

//METODO PARA GENERAR CADENAS

def generarSecuencia(n: Int): Seq[Char] = {
  val longitud = math.pow(2, n).toInt
  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
}

def generarSecuenciaCortas(n:Int) : Seq[Char] = {
  Seq.fill(n)(alfabeto(Random.nextInt(alfabeto.length)))
}

//METODO PARA GENERAR LA PRUEBA

//def generarPrueba(MIN:Int,MAX:Int,nPrueba: Int, algorSec:(Int,Oraculo) => Seq[Char],
//                  algorPar:Int=>(Int,Oraculo) => Seq[Char], umbral:Int ) : Seq[String] = {
//  val reportes = for {
//      l <- MIN to MAX
//      n <- 1 to nPrueba
//      secuencia = generarSecuencia(l)
//      o = crearOraculo(1)(secuencia)
//      resultado = compararAlgoritmosPar(
//        algorSec,
//        algorPar
//      )(umbral)(secuencia.length, o)
//    } yield s"Cadenas ${secuencia.length} | Prueba $n | Resultado: $resultado"
//
//    reportes
//}

/*
* Generar Prueba recibe 6 parametros:
* 1. longitud minima de los caracteres que se van a probar
* 2. longitud Maxima de los caracteres que se van a probar
* 3. numero de pruebas maximas que se van a hacer, de 1 hasta el valor ingresado
* 4. algoritmo secuencial a comparar
* 5. algoritmo paralelo a comparar
* 6. umbral del algoritmo secuencial
* */

//val generarPruebas:Seq[String] = generarPrueba(9,10,1,reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar,0)
//
//
//generarPruebas.foreach(println)

//def generarCasoPruebaSec(MIN:Int,MAX:Int, algorSec:(Int,Oraculo) => Seq[Char]) : Seq[String] = {
//  val reportes = for {
//    l <- MIN to MAX
//    secuencia = generarSecuencia(l)
//    o = crearOraculo(0)(secuencia)
//    cadenaGenerada = algorSec(secuencia.length,o)
//    resultado = cadenaGenerada == secuencia
//    tiempo = measure(algorSec(secuencia.length,o))
//  } yield s"Cadenas ${secuencia.length} | Resultado: $resultado | Tiempo: $tiempo"
//
//  reportes
//}
//
//val generarCasosPuebaSecMejorado = generarCasoPruebaSec(1,10,reconstruirCadenaMejorado)
//generarCasosPuebaSecMejorado.foreach(println)
//
//val generarCasosPuebaSecTurbo = generarCasoPruebaSec(1,10,reconstruirCadenaTurbo)
//generarCasosPuebaSecTurbo.foreach(println)
//
//val generarCasosPuebaSecTurboMejorada = generarCasoPruebaSec(1,12,reconstruirCadenaTurboMejorada)
//generarCasosPuebaSecTurboMejorada.foreach(println)
//
//val generarCasosPuebaSecTurboAcelerada = generarCasoPruebaSec(1,12,reconstruirCadenaTurboAcelerada)
//generarCasosPuebaSecTurboAcelerada.foreach(println)

def generarCasoPruebaSecCortas(long: List[Int], algorSec:(Int,Oraculo) => Seq[Char]) : Seq[String] = {
  val reportes = for {
    l <- long
    secuencia = generarSecuenciaCortas(l)
    o = crearOraculo(0)(secuencia)
    cadenaGenerada = algorSec(secuencia.length,o)
    resultado = cadenaGenerada == secuencia
    tiempo = measure(algorSec(secuencia.length,o))
  } yield s"Cadena: ${secuencia} | Resultado: $cadenaGenerada | Tiempo: $tiempo | Estado: $resultado"

  reportes
}

def generarCasoPruebaSec(MIN:Int,MAX:Int, algorPar:Int => (Int,Oraculo) => Seq[Char],umbral : Int) : Seq[String] = {
  val reportes = for {
    l <- MIN to MAX
    secuencia = generarSecuencia(l)
    o = crearOraculo(0)(secuencia)
    cadenaGenerada = algorPar(umbral)(secuencia.length,o)
    resultado = cadenaGenerada == secuencia
    tiempo = measure(algorPar(umbral)(secuencia.length,o))
  } yield s"Cadenas ${secuencia.length} | Resultado: $resultado | Tiempo: $tiempo"

  reportes
}

def generarCasoPruebaParCorta(long: List[Int], algorPar:Int => (Int,Oraculo) => Seq[Char],umbral : Int) : Seq[String] = {
  val reportes = for {
    l <- long
    secuencia = generarSecuenciaCortas(l)
    o = crearOraculo(1)(secuencia)
    cadenaGenerada = algorPar(umbral)(secuencia.length,o)
    resultado = cadenaGenerada == secuencia
    tiempo = measure(algorPar(umbral)(secuencia.length,o))
  } yield s"Cadena: ${secuencia} | Resultado: $cadenaGenerada | Tiempo: $tiempo | Estado: $resultado"

  reportes
}

//val generarCasosPuebaParMejorado = generarCasoPruebaSec(1,10,reconstruirCadenaMejoradoPar,0)
//generarCasosPuebaParMejorado.foreach(println)
//
//val generarCasosPuebaParTurbo = generarCasoPruebaSec(10,10,reconstruirCadenaTurboPar,4)
//generarCasosPuebaParTurbo.foreach(println)

//val generarCasosPuebaParTurboMejorada = generarCasoPruebaSec(12,12,reconstruirCadenaTurboMejoradaPar,2)
//generarCasosPuebaParTurboMejorada.foreach(println)
//
//val generarCasosPuebaParTurboAcelerada = generarCasoPruebaSec(12,12,reconstruirCadenaTurboAceleradaPar,2)
//generarCasosPuebaParTurboAcelerada.foreach(println)

//val generarCasosPruebaIngenuo =  generarCasoPruebaSecCortas(List(13,14,16),reconstruirCadenaIngenuo)
//generarCasosPruebaIngenuo.foreach(println)
//
//val generarCasosPruebaIngenuoPar =  generarCasoPruebaParCorta(List(2,4,6,8,10,12),reconstruirCadenaIngenuoPar,0)
//generarCasosPruebaIngenuoPar.foreach(println)


val a = generarSecuencia(5)
val o = crearOraculo(0)(a)

reconstruirCadenaTurboPar(1000)(a.length,o)
