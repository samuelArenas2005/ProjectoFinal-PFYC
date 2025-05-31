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

  def reconstruirCadenaTurboPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

  def reconstruirCadenaTurboMejoradaPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

  def reconstruirCadenaTurboAceleradaPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

}
