import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import common.parallel
import org.scalameter.*
import scala.util.Random
import Benchmark.*


val random = new Random()

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}


def secsLargasParaPruebas(n:Int):Seq[Seq[Char]] = for {
  i <- 1 to n
  s = secAlAzar(math.pow(2,i).toInt,Seq())
} yield s

val time10 = 10
val time100 = 100
val time1000 = 1000
val sec3 = secsLargasParaPruebas(3)
val sec6 = secsLargasParaPruebas(6)
val sec10 = secsLargasParaPruebas(10)
val sec12 = secsLargasParaPruebas(12)

val umbral = 2


//for{
//  secuencia <- sec3
//  o = crearOraculo(time10)(secuencia)
//}yield ("Longitud: " + secuencia.length ,compararAlgoritmosPar(
//  reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar
//)(umbral)(secuencia.length,o))

for{
  secuencia <- sec3
  o = crearOraculo(time10)(secuencia)
}yield ("Longitud: " + secuencia.length ,compararAlgoritmosSec(
  reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada
)(secuencia.length,o))



//METODO DE COMPARACION DOS

//def generarSecuencia(n: Int): Seq[Char] = {
//  val longitud = math.pow(2, n).toInt
//  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
//}
//
//val umbral = 2
//
//val secuenciasPrueba = generarSecuencia(2) // <- el numero va variando
//val times = List(1,10,100)
//
//for{
//  n <- 1 to 3
//  time <- times
//  o = crearOraculo(time)(secuenciasPrueba)
//}yield("Prueba: " + n,"Delay: " + time, compararAlgoritmosPar(
//  reconstruirCadenaIngenuo,
//  reconstruirCadenaIngenuoPar
//)(umbral)(secuenciasPrueba.length,o))



