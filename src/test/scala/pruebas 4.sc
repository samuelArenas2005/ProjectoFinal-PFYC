import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar._
import common.parallel
import org.scalameter.*
import scala.util.Random
import Benchmark.*


def generarSecuenciaCortas(n:Int) : Seq[Char] = {
  Seq.fill(n)(alfabeto(Random.nextInt(alfabeto.length)))
}

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


val a = generarSecuencia(5)
val o = crearOraculo(0)(a)

reconstruirCadenaTurboPar(100)(a.length,o)

//val prueba = generarPruebaIngenuo(List(11),1,reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar,0)
//
//prueba.foreach(println)

reconstruirCadenaTurbo2()

