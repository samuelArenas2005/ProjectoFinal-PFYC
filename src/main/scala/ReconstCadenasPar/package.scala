import common.*

import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*

import scala.annotation.tailrec


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

    @tailrec
    def construirSubcadenaValida(cadenasActuales: Set[Seq[Char]]): Seq[Char] = {
      val siguientesSubcadenas = generarSubcadenasValidasPar(cadenasActuales)
      if (siguientesSubcadenas.head.length == n) siguientesSubcadenas.head
      else construirSubcadenaValida(siguientesSubcadenas)
    }

    construirSubcadenaValida(Set(vacia))
  }


  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    @tailrec
    def expandirCadenas(cadenas: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) cadenas.head
      else {
        val pares = for {
          a <- cadenas
          b <- cadenas
        } yield (a, b)
        val nuevasCadenas = if (pares.size > umbral) {
          pares.par.map { case (a, b) => a ++ b }.filter(o).seq
        } else {
          pares.map { case (a, b) => a ++ b }.filter(o)
        }
        expandirCadenas(nuevasCadenas, k * 2)
      }
    }

    val cadenasIniciales = alfabeto.map(c => Seq(c))
    expandirCadenas(cadenasIniciales, 1)
  }



  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def filtrar(sc: Set[String], k: Int): Set[String] = {
      val parNuevas = for {
        s1 <- sc.par
        s2 <- sc.par
        s = s1 + s2
        if s.sliding(k).forall(sc.contains)
      } yield s
      parNuevas.seq.toSet
    }

    @tailrec
    def construir(sc: Set[String], k: Int): String = {
      if (k >= n) {
        sc.find(w => w.length == n && o(w.toSeq)).getOrElse("")
      } else {
        val filtered = filtrar(sc, k)
        val candidates = filtered.par
        val valid = candidates.filter(w => o(w.toSeq))
        construir(valid.seq.toSet, 2 * k)
      }
    }

    val sc1 = alfabeto.map(_.toString).toSet

    construir(sc1, 1).toList
  }


  def reconstruirCadenaTurboAceleradaPar(umbral:Int)(n: Int, o: Oraculo): Seq[Char] = {
    def filtrarTrie(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val scTrie = arbolDeSufijos(sc)
      (for {
        s1 <- sc.par
        s2 <- sc.par
        s = s1 ++ s2
        if s.sliding(k).forall(sub => pertenece(sub, scTrie))
      } yield s).seq
    }

    @tailrec
    def verificarCadenas(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) {
        sc.find(w => w.length == n && o(w.toSeq)).getOrElse("")
      } else {
        val filtered = filtrarTrie(sc, k)
        val valid = filtered.par.filter(o).toList
        verificarCadenas(valid, 2*k)
      }
    }

    val cadenasIniciales = alfabeto.map(c => Seq(c))
    verificarCadenas(cadenasIniciales,1)
  }

}