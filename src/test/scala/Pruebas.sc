import Benchmark.compararAlgoritmosPar
import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import common.parallel
import org.scalameter.*

import scala.util.Random
val random = new Random()


/******************************************************************
 * *
 * SECCIÓN DE COMPARACIÓN DE ALGORITMOS: PARALELO VS. SEC      *
 * *
 ******************************************************************/


def generarSecuencia(n: Int): Seq[Char] = {
  val longitud = math.pow(2, n).toInt
  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
}

def generarSecuenciaCortas(n:Int) : Seq[Char] = {
  Seq.fill(n)(alfabeto(Random.nextInt(alfabeto.length)))
}


def generarPrueba(MIN:Int,MAX:Int,nPrueba: Int, algorSec:(Int,Oraculo) => Seq[Char],
                  algorPar:Int=>(Int,Oraculo) => Seq[Char], umbral:Int ) : Seq[String] = {
  val reportes = for {
      l <- MIN to MAX
      n <- 1 to nPrueba
      secuencia = generarSecuencia(l)
      o = crearOraculo(1)(secuencia)
      resultado = compararAlgoritmosPar(
        algorSec,
        algorPar
      )(umbral)(secuencia.length, o)
    } yield s"Cadenas ${secuencia.length} | Prueba $n | Resultado: $resultado"

    reportes
}

/*
* Generar Prueba recibe 6 parametros:
* 1. longitud minima de los caracteres que se van a probar
* 2. longitud Maxima de los caracteres que se van a probar
* 3. numero de pruebas maximas que se van a hacer, de 1 hasta el valor ingresado
* 4. algoritmo secuencial a comparar
* 5. algoritmo paralelo a comparar
* 6. umbral del algoritmo secuencial
* */


def generarPruebaIngenuo(sec: List[Int],nPrueba: Int, algorSec:(Int,Oraculo) => Seq[Char],
                         algorPar:Int=>(Int,Oraculo) => Seq[Char], umbral:Int ) : Seq[String] = {
  val reportes = for {
    longN <- sec
    n <- 1 to nPrueba
    secuencia = generarSecuenciaCortas(longN)
    o = crearOraculo(1)(secuencia)
    resultado = compararAlgoritmosPar(
      algorSec,
      algorPar
    )(umbral)(secuencia.length, o)
  } yield s"Cadena ${secuencia} | Prueba $n | Resultado: $resultado"

  reportes
}


/*******************************************
 * *
 * SECCIÓN DE PRUEBA DE ALGORITMOS      *
 * *
 *******************************************/


def generarCasoPruebaSec(MIN:Int,MAX:Int, algorSec:(Int,Oraculo) => Seq[Char]) : Seq[String] = {
  val reportes = for {
    l <- MIN to MAX
    secuencia = generarSecuencia(l)
    o = crearOraculo(0)(secuencia)
    cadenaGenerada = algorSec(secuencia.length,o)
    resultado = cadenaGenerada == secuencia
    tiempo = measure(algorSec(secuencia.length,o))
  } yield s"Cadenas ${secuencia.length} | Resultado: $resultado | Tiempo: $tiempo"

  reportes
}


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

def generarCasoPruebaPar(MIN:Int,MAX:Int, algorPar:Int => (Int,Oraculo) => Seq[Char],umbral : Int) : Seq[String] = {
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
    o = crearOraculo(0)(secuencia)
    cadenaGenerada = algorPar(umbral)(secuencia.length,o)
    resultado = cadenaGenerada == secuencia
    tiempo = measure(algorPar(umbral)(secuencia.length,o))
  } yield s"Cadena: ${secuencia} | Resultado: $cadenaGenerada | Tiempo: $tiempo | Estado: $resultado"

  reportes
}

/***********************************************************
 * *
 * DEFINICIÓN DE CONSTANTES PARA LAS PRUEBAS            *
 * *
 ***********************************************************/

//// Rango de longitudes para las pruebas de cadenas largas
//val MIN_LONG = 6
//val MAX_LONG = 9
//
//// Lista de longitudes para las pruebas de cadenas cortas
//val longitudesCortas = (1 to 5).toList
//val umbralParalelo = 0
//
//println("Iniciando conjunto de pruebas...")
//
///***********************************************************
// * *
// * PRUEBAS PARA reconstruirCadenaIngenuo                 *
// * *
// ***********************************************************/
//println("\n--- Pruebas para Algoritmo Ingenuo (cadenas cortas) ---")
//val pruebaIngenuoSec = generarCasoPruebaSecCortas(longitudesCortas, reconstruirCadenaIngenuo)
//pruebaIngenuoSec.foreach(println)
//
//println("\n--- Pruebas para Algoritmo Ingenuo Paralelo (cadenas cortas) ---")
//val pruebaIngenuoPar = generarCasoPruebaParCorta(longitudesCortas, reconstruirCadenaIngenuoPar, umbralParalelo)
//pruebaIngenuoPar.foreach(println)
//
//
///***********************************************************
// * *
// * PRUEBAS PARA reconstruirCadenaMejorado                *
// * *
// ***********************************************************/
//println("\n--- Pruebas para Algoritmo Mejorado ---")
//val pruebaMejoradoSec = generarCasoPruebaSec(MIN_LONG, MAX_LONG, reconstruirCadenaMejorado)
//pruebaMejoradoSec.foreach(println)
//
//println("\n--- Pruebas para Algoritmo Mejorado Paralelo ---")
//val pruebaMejoradoPar = generarCasoPruebaPar(MIN_LONG, MAX_LONG, reconstruirCadenaMejoradoPar, umbralParalelo)
//pruebaMejoradoPar.foreach(println)
//
//
///***********************************************************
// * *
// * PRUEBAS PARA reconstruirCadenaTurbo                   *
// * *
// ***********************************************************/
//println("\n--- Pruebas para Algoritmo Turbo ---")
//val pruebaTurboSec = generarCasoPruebaSec(MIN_LONG, MAX_LONG, reconstruirCadenaTurbo)
//pruebaTurboSec.foreach(println)
//
//println("\n--- Pruebas para Algoritmo Turbo Paralelo ---")
//val pruebaTurboPar = generarCasoPruebaPar(MIN_LONG, MAX_LONG, reconstruirCadenaTurboPar, umbralParalelo)
//pruebaTurboPar.foreach(println)
//
//
///***********************************************************
// * *
// * PRUEBAS PARA reconstruirCadenaTurboMejorada           *
// * *
// ***********************************************************/
//println("\n--- Pruebas para Algoritmo Turbo Mejorada ---")
//val pruebaTurboMejoradaSec = generarCasoPruebaSec(MIN_LONG, MAX_LONG, reconstruirCadenaTurboMejorada)
//pruebaTurboMejoradaSec.foreach(println)
//
//println("\n--- Pruebas para Algoritmo Turbo Mejorada Paralelo ---")
//val pruebaTurboMejoradaPar = generarCasoPruebaPar(MIN_LONG, MAX_LONG, reconstruirCadenaTurboMejoradaPar, umbralParalelo)
//pruebaTurboMejoradaPar.foreach(println)
//
//
///***********************************************************
// * *
// * PRUEBAS PARA reconstruirCadenaTurboAcelerada         *
// * *
// ***********************************************************/
//println("\n--- Pruebas para Algoritmo Turbo Acelerada ---")
//val pruebaTurboAceleradaSec = generarCasoPruebaSec(MIN_LONG, MAX_LONG, reconstruirCadenaTurboAcelerada)
//pruebaTurboAceleradaSec.foreach(println)
//
//println("\n--- Pruebas para Algoritmo Turbo Acelerada Paralelo ---")
//val pruebaTurboAceleradaPar = generarCasoPruebaPar(MIN_LONG, MAX_LONG, reconstruirCadenaTurboAceleradaPar, umbralParalelo)
//pruebaTurboAceleradaPar.foreach(println)




/*****************************************************
 * *
 * SECCIÓN DE PRUEBAS DE ÁRBOL DE SUFIJOS         *
 * *
 *****************************************************/



//val t1 = Nodo('_', false, Nil)
//val t2 = adicionar("cat".toList, t1)
//val t3 = adicionar("car".toList, t2)
//val t4 = adicionar("cac".toList, t3)
//val t5 = adicionar("ca".toList,t4)
//val t40 = adicionar("m".toList,t5)
//
//
//println(pertenece("cat".toList, t4))
//println(pertenece("car".toList, t4))
//println(pertenece("cac".toList, t4))
//println(pertenece("ca".toList, t4))
//
//println(pertenece("ca".toList, t5))
//
//
//val m =
//  Nodo('_',false,List(
//    Nodo('c',false,List(
//      Nodo('a',false,List(
//        Hoja('t',true), Hoja('r',true), Hoja('c',true))
//      ))
//    ))
//  )
//
//val prueba1 = Seq('a','c','a','c')
//val prueba2 = Seq('c','a','c','t')
//val prueba = Seq('t','a')
//
//
//val arbol = arbolDeSufijos(Seq(prueba1,prueba2,prueba))
//
//pertenecer(Seq('t','a'),arbol)
//
//
//val tprueba: Trie = Nodo(' ', false, List(
//  Nodo('a', false, List(
//    Nodo('c', true, List(
//      Nodo('a', false, List(
//        Hoja('c', true)
//      )),
//      Hoja('t', true)
//    ))
//  )),
//  Nodo('c', true, List(
//    Nodo('a', false, List(
//      Nodo('c', true, List(
//        Hoja('t', true)
//      ))
//    )),
//    Hoja('t', true)
//  )),
//  Hoja('t', true)
//))






/*
 * =======================================================
 * SECCIÓN DE PRUEBA PROPORCIONADA POR EL PROFESOR
 * =======================================================
 */

type AlgoritCadena = (Int, Oraculo) => Seq[Char]

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  //Crea una secuencia de long caracteres del alfabeto,
  // escogidos de forma aleatoria, terminando en s
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}

val costoOraculo = 0

val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a')
val sec3=secAlAzar(10,Seq())

val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)

reconstruirCadenaIngenuo(sec1.length, or_1)
reconstruirCadenaIngenuo(sec2.length, or_2)
reconstruirCadenaIngenuo(sec3.length, or_3)



def secsCortasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(i,Seq())
} yield s

def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s

def pruebasIngenuo (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s,reconstruirCadenaIngenuo(s.length,o))



def pruebasMejorado (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaMejorado(s.length,o))



def pruebasTurbo (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaTurbo(s.length,o))

def pruebasTurboMejorada (ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaTurboMejorada(s.length,o))



def pruebasTurboAcelerada(ss:Seq[Seq[Char]]) = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s,reconstruirCadenaTurboAcelerada(s.length,o))


// Secuencias para pruebas
val ss1_10=secsCortasParaPruebas(10)
val ss1_16=secsCortasParaPruebas(16)
val ss2_1024 = secsLargasParaPruebas(10)
val ss2_2048 = secsLargasParaPruebas(11)
val ss2_4096 = secsLargasParaPruebas(12)
val s1_8 = ss1_10(7)
val s2_8 = ss1_16(7)
val s1_10 = ss1_10.reverse(0)
val s1_11=ss1_16(10)
val s1_12 = ss1_16(11)
val s1_13 = ss1_16(12)
val s1_14 = ss1_16(13)
val s1_15 = ss1_16(14)
val s1_16 = ss1_16(15)
val s1_32 = ss2_1024(4)
val s2_32 = ss2_2048(4)
val s3_32 = ss2_4096(4)
val s1_64 = ss2_1024(5)
val s2_64 = ss2_2048(5)
val s3_64 = ss2_4096(5)
val s1_128 = ss2_1024(6)
val s2_128 = ss2_2048(6)
val s3_128 = ss2_4096(6)
val s1_256 = ss2_1024(7)
val s2_256 = ss2_2048(7)
val s3_256 = ss2_4096(7)
val s1_512 = ss2_1024(8)
val s2_512 = ss2_2048(8)
val s3_512 = ss2_4096(8)
val s1_1024 = ss2_1024(9)
val s2_1024 = ss2_2048(9)
val s3_1024 = ss2_4096(9)
val s1_2048 = ss2_2048(10)
val s2_2048 = ss2_4096(10)
val s1_4096 = ss2_4096(11)


// Pruebas funcionales

// secuencias de longitud 8
reconstruirCadenaIngenuo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaIngenuo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaMejorado(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaMejorado(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurbo(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurbo(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurboMejorada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboMejorada(s2_8.length, crearOraculo(costoOraculo)(s2_8))
reconstruirCadenaTurboAcelerada(s1_8.length, crearOraculo(costoOraculo)(s1_8))
reconstruirCadenaTurboAcelerada(s2_8.length, crearOraculo(costoOraculo)(s2_8))
// secuencias de longitud 16
reconstruirCadenaMejorado(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurbo(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboMejorada(s1_16.length, crearOraculo(costoOraculo)(s1_16))
reconstruirCadenaTurboAcelerada(s1_16.length, crearOraculo(costoOraculo)(s1_16))

// secuencias de longitud 32
reconstruirCadenaMejorado(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaMejorado(s2_32.length, crearOraculo(costoOraculo)(s2_32))
reconstruirCadenaMejorado(s3_32.length, crearOraculo(costoOraculo)(s3_32))
reconstruirCadenaTurbo(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurbo(s2_32.length, crearOraculo(costoOraculo)(s2_32))
reconstruirCadenaTurbo(s3_32.length, crearOraculo(costoOraculo)(s3_32))
reconstruirCadenaTurboMejorada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboMejorada(s2_32.length, crearOraculo(costoOraculo)(s2_32))
reconstruirCadenaTurboMejorada(s3_32.length, crearOraculo(costoOraculo)(s3_32))
reconstruirCadenaTurboAcelerada(s1_32.length, crearOraculo(costoOraculo)(s1_32))
reconstruirCadenaTurboAcelerada(s2_32.length, crearOraculo(costoOraculo)(s2_32))
reconstruirCadenaTurboAcelerada(s3_32.length, crearOraculo(costoOraculo)(s3_32))


// secuencias de longitud 64
reconstruirCadenaMejorado(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaMejorado(s2_64.length, crearOraculo(costoOraculo)(s2_64))
reconstruirCadenaMejorado(s3_64.length, crearOraculo(costoOraculo)(s3_64))
reconstruirCadenaTurbo(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurbo(s2_64.length, crearOraculo(costoOraculo)(s2_64))
reconstruirCadenaTurbo(s3_64.length, crearOraculo(costoOraculo)(s3_64))
reconstruirCadenaTurboMejorada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboMejorada(s2_64.length, crearOraculo(costoOraculo)(s2_64))
reconstruirCadenaTurboMejorada(s3_64.length, crearOraculo(costoOraculo)(s3_64))
reconstruirCadenaTurboAcelerada(s1_64.length, crearOraculo(costoOraculo)(s1_64))
reconstruirCadenaTurboAcelerada(s2_64.length, crearOraculo(costoOraculo)(s2_64))
reconstruirCadenaTurboAcelerada(s3_64.length, crearOraculo(costoOraculo)(s3_64))

// secuencias de longitud 128
reconstruirCadenaMejorado(s1_128.length, crearOraculo(costoOraculo)(s1_128))
reconstruirCadenaMejorado(s2_128.length, crearOraculo(costoOraculo)(s2_128))
reconstruirCadenaMejorado(s3_128.length, crearOraculo(costoOraculo)(s3_128))
reconstruirCadenaTurbo(s1_128.length, crearOraculo(costoOraculo)(s1_128))
reconstruirCadenaTurbo(s2_128.length, crearOraculo(costoOraculo)(s2_128))
reconstruirCadenaTurbo(s3_128.length, crearOraculo(costoOraculo)(s3_128))
reconstruirCadenaTurboMejorada(s1_128.length, crearOraculo(costoOraculo)(s1_128))
reconstruirCadenaTurboMejorada(s2_128.length, crearOraculo(costoOraculo)(s2_128))
reconstruirCadenaTurboMejorada(s3_128.length, crearOraculo(costoOraculo)(s3_128))
reconstruirCadenaTurboAcelerada(s1_128.length, crearOraculo(costoOraculo)(s1_128))
reconstruirCadenaTurboAcelerada(s2_128.length, crearOraculo(costoOraculo)(s2_128))
reconstruirCadenaTurboAcelerada(s3_128.length, crearOraculo(costoOraculo)(s3_128))

// secuencias de longitud 256
reconstruirCadenaMejorado(s1_256.length, crearOraculo(costoOraculo)(s1_256))
reconstruirCadenaMejorado(s2_256.length, crearOraculo(costoOraculo)(s2_256))
reconstruirCadenaMejorado(s3_256.length, crearOraculo(costoOraculo)(s3_256))
reconstruirCadenaTurbo(s1_256.length, crearOraculo(costoOraculo)(s1_256))
reconstruirCadenaTurbo(s2_256.length, crearOraculo(costoOraculo)(s2_256))
reconstruirCadenaTurbo(s3_256.length, crearOraculo(costoOraculo)(s3_256))
reconstruirCadenaTurboMejorada(s1_256.length, crearOraculo(costoOraculo)(s1_256))
reconstruirCadenaTurboMejorada(s2_256.length, crearOraculo(costoOraculo)(s2_256))
reconstruirCadenaTurboMejorada(s3_256.length, crearOraculo(costoOraculo)(s3_256))
reconstruirCadenaTurboAcelerada(s1_256.length, crearOraculo(costoOraculo)(s1_256))
reconstruirCadenaTurboAcelerada(s2_256.length, crearOraculo(costoOraculo)(s2_256))
reconstruirCadenaTurboAcelerada(s3_256.length, crearOraculo(costoOraculo)(s3_256))

// secuencias de longitud 512
reconstruirCadenaMejorado(s1_512.length, crearOraculo(costoOraculo)(s1_512))
reconstruirCadenaMejorado(s2_512.length, crearOraculo(costoOraculo)(s2_512))
reconstruirCadenaMejorado(s3_512.length, crearOraculo(costoOraculo)(s3_512))
reconstruirCadenaTurbo(s1_512.length, crearOraculo(costoOraculo)(s1_512))
reconstruirCadenaTurbo(s2_512.length, crearOraculo(costoOraculo)(s2_512))
reconstruirCadenaTurbo(s3_512.length, crearOraculo(costoOraculo)(s3_512))
reconstruirCadenaTurboMejorada(s1_512.length, crearOraculo(costoOraculo)(s1_512))
reconstruirCadenaTurboMejorada(s2_512.length, crearOraculo(costoOraculo)(s2_512))
reconstruirCadenaTurboMejorada(s3_512.length, crearOraculo(costoOraculo)(s3_512))
reconstruirCadenaTurboAcelerada(s1_512.length, crearOraculo(costoOraculo)(s1_512))
reconstruirCadenaTurboAcelerada(s2_512.length, crearOraculo(costoOraculo)(s2_512))
reconstruirCadenaTurboAcelerada(s3_512.length, crearOraculo(costoOraculo)(s3_512))


// secuencias de longitud 1024
reconstruirCadenaMejorado(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
reconstruirCadenaMejorado(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
reconstruirCadenaMejorado(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
reconstruirCadenaTurbo(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
reconstruirCadenaTurbo(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
reconstruirCadenaTurbo(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
reconstruirCadenaTurboMejorada(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
reconstruirCadenaTurboMejorada(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
reconstruirCadenaTurboMejorada(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))
reconstruirCadenaTurboAcelerada(s1_1024.length, crearOraculo(costoOraculo)(s1_1024))
reconstruirCadenaTurboAcelerada(s2_1024.length, crearOraculo(costoOraculo)(s2_1024))
reconstruirCadenaTurboAcelerada(s3_1024.length, crearOraculo(costoOraculo)(s3_1024))

// Pruebas por lotes

pruebasIngenuo(ss1_10)
pruebasIngenuo(ss1_16.slice(0,10))
pruebasIngenuo(ss1_16.slice(0,11))
pruebasIngenuo(ss1_16.slice(0,12))
pruebasIngenuo(ss1_16.slice(0,13))
pruebasIngenuo(ss1_16.slice(0,14))
pruebasIngenuo(ss1_16.slice(0,15))
pruebasIngenuo(ss1_16)

// con n=13 casi no puede, con n= 14, no pudo


pruebasMejorado(ss1_10)
pruebasMejorado(ss1_16.slice(0,10))
pruebasMejorado(ss1_16.slice(0,11))
pruebasMejorado(ss1_16.slice(0,12))
pruebasMejorado(ss1_16.slice(0,13))
pruebasMejorado(ss1_16.slice(0,14))
pruebasMejorado(ss1_16.slice(0,15))
pruebasMejorado(ss1_16)
pruebasMejorado(ss2_1024)
pruebasMejorado(ss2_2048)
pruebasMejorado(ss2_4096)
// con n=2^11  pudo, con n= 2^12, no pudo

pruebasTurbo(ss1_10)
pruebasTurbo(ss1_16.slice(0,10))
pruebasTurbo(ss1_16.slice(0,11))
pruebasTurbo(ss1_16.slice(0,12))
pruebasTurbo(ss1_16.slice(0,13))
pruebasTurbo(ss1_16.slice(0,14))
pruebasTurbo(ss1_16.slice(0,15))
pruebasTurbo(ss1_16)
pruebasTurbo(ss2_1024)
pruebasTurbo(ss2_4096)

pruebasTurboMejorada(ss1_10)
pruebasTurboMejorada(ss1_16.slice(0,10))
pruebasTurboMejorada(ss1_16.slice(0,11))
pruebasTurboMejorada(ss1_16.slice(0,12))
pruebasTurboMejorada(ss1_16.slice(0,13))
pruebasTurboMejorada(ss1_16.slice(0,14))
pruebasTurboMejorada(ss1_16.slice(0,15))
pruebasTurboMejorada(ss1_16)
pruebasTurboMejorada(ss2_1024)
pruebasTurboMejorada(ss2_2048)
pruebasTurboMejorada(ss2_4096)

pruebasTurboAcelerada(ss1_10)
pruebasTurboAcelerada(ss1_16.slice(0,10))
pruebasTurboAcelerada(ss1_16.slice(0,11))
pruebasTurboAcelerada(ss1_16.slice(0,12))
pruebasTurboAcelerada(ss1_16.slice(0,13))
pruebasTurboAcelerada(ss1_16.slice(0,14))
pruebasTurboAcelerada(ss1_16.slice(0,15))
pruebasTurboAcelerada(ss1_16)
pruebasTurboAcelerada(ss2_1024)
pruebasTurboAcelerada(ss2_2048)
pruebasTurboAcelerada(ss2_4096)



