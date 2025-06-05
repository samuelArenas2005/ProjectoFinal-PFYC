import common.*
import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*

package object ReconstCadenasPar {

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def crearCadenas(chars: LazyList[Char], longitud: Int): LazyList[Seq[Char]] = {
      if (longitud == 1) chars.map(Seq(_))
      else for {
        char <- chars
        subCadena <- crearCadenas(alfabeto.to(LazyList), longitud - 1)
      } yield char +: subCadena
    }

    val cadenas1 = task(crearCadenas(alfabeto.slice(0, 1).to(LazyList), n))
    val cadenas2 = task(crearCadenas(alfabeto.slice(1, 2).to(LazyList), n))
    val cadenas3 = task(crearCadenas(alfabeto.slice(2, 3).to(LazyList), n))
    val cadenas4 = task(crearCadenas(alfabeto.slice(3, 4).to(LazyList), n))

    val cadenasCompletas = cadenas1.join() ++ cadenas2.join() ++ cadenas3.join() ++ cadenas4.join()
    cadenasCompletas.par.find(o).getOrElse(Seq())
  }


  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    val vacia: Seq[Char] = Seq.empty

    def generarSubcadenasValidasPar(subcadenasActuales: Set[Seq[Char]]): Set[Seq[Char]] = {
      val parConjunto = subcadenasActuales.par
      val parNuevas = for {
        subcadena <- parConjunto
        letra <- alfabeto.par
        nueva = subcadena :+ letra
        if o(nueva)
      } yield nueva
      parNuevas.seq.toSet
    }


    def construirSubcadena(cadenasActuales: Set[Seq[Char]], k: Int): Seq[Char] = {
      val siguientes: Set[Seq[Char]] = generarSubcadenasValidasPar(cadenasActuales)
      siguientes.find(_.length == n) match {
        case Some(sol) => sol
        case None =>
          construirSubcadena(siguientes, k + 1)
      }
    }

    construirSubcadena(Set(vacia), 1)
  }


  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }


  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(sc: Set[String], k: Int): Set[String] = {
      val parSc = sc.par
      val parNuevas = for {
        s1 <- sc.par
        s2 <- sc.par
        s = s1 + s2
        if s.sliding(k).forall(sc.contains)
      } yield s
      parNuevas.seq.toSet
    }


    // Función recursiva que construye la cadena en paralelo
    def construir(sc: Set[String], k: Int): String = {
      if (k >= n) {
        sc.find(w => w.length == n && o(w.toSeq)).getOrElse("")
      } else {
        val nextK = 2 * k
        val filtered = filtrar(sc, k)
        val candidates = filtered.par
        val valid = candidates.filter(w => o(w.toSeq))
        valid.find(_.length == n).getOrElse(construir(valid.seq.toSet, nextK))
      }
    }

    val sc1 = alfabeto.flatMap(a => {
      val s = a.toString
      if (o(s.toSeq)) Some(s) else None
    }).toSet

    construir(sc1, 1).toList
  }


  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

}
