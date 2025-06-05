import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._


package object ReconstCadenasPar {

  def reconstruirCadenaIngenuoPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    def crearCadenas(chars: LazyList[Char], longitud: Int): LazyList[Seq[Char]] = {
      if (longitud == 1) chars.map(Seq(_))
      else for {
        char <- chars
        subCad <- crearCadenas(chars, longitud - 1)
      } yield char +: subCad
    }

    crearCadenas(alfabeto.to(LazyList), n).find(o).getOrElse(Seq())
  }


  def reconstruirCadenaMejoradoPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // Generar todas las combinaciones de longitud 2 del alfabeto,
    // y filtrar aquellas que el oráculo acepta como válidas.
    val cadenasIniciales: Seq[Seq[Char]] = for {
      c1 <- alfabeto
      c2 <- alfabeto
      if o(Seq(c1, c2))
    } yield Seq(c1, c2)


    def expandirCadenas(cadenas: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) cadenas.head
      else {
        val pares = for {
          a <- cadenas
          b <- cadenas
        } yield (a, b)

        val nuevasCadenas = if (pares.size > umbral) {
          // Paralelizamos solo si el número de combinaciones excede el umbral
          pares.par
            .map { case (a, b) => a ++ b }
            .filter(o)
            .seq
        } else {
          pares
            .map { case (a, b) => a ++ b }
            .filter(o)
        }
        expandirCadenas(nuevasCadenas, k * 2)
      }
    }
    expandirCadenas(cadenasIniciales, 2)
  }


  def reconstruirCadenaTurboMejoradaPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

  def reconstruirCadenaTurboAceleradaPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

}
