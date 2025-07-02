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
    
    def construirSubcadenaValida(cadenasActuales: Set[Seq[Char]]): Seq[Char] = {
      val siguientesSubcadenas = generarSubcadenasValidasPar(cadenasActuales)
      if (siguientesSubcadenas.head.length == n) siguientesSubcadenas.head
      else construirSubcadenaValida(siguientesSubcadenas)
    }
    
    construirSubcadenaValida(Set(vacia))
  }
  

//  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
//
//    def expandirCadenas(cadenas: Seq[Seq[Char]], k: Int): Seq[Char] = {
//      if (k >= n) cadenas.head
//      else {
//        val pares = for {
//          a <- cadenas
//          b <- cadenas
//        } yield (a, b)
//        val nuevasCadenas = if (pares.size > umbral) {
//          // Paralelizamos solo si el número de combinaciones excede el umbral
//          pares.par.map { case (a, b) => a ++ b }.filter(o).seq
//        } else {
//          pares.map { case (a, b) => a ++ b }.filter(o)
//        }
//        expandirCadenas(nuevasCadenas, k * 2)
//      }
//    }
//
//    val cadenasIniciales = alfabeto.map(c => Seq(c))
//    expandirCadenas(cadenasIniciales, 1)
//  }


  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // 1. Generar las combinaciones de todas las parejas (a, b)
    def generarCombinaciones(cadenas: Seq[Seq[Char]]): Seq[(Seq[Char], Seq[Char])] = {
      for {
        a <- cadenas
        b <- cadenas
      } yield (a, b)
    }

    // 2. Combinar y filtrar usando el oráculo, con paralelismo si se supera el umbral
    def combinarYFiltrar(pares: Seq[(Seq[Char], Seq[Char])], usarParalelo: Boolean): Seq[Seq[Char]] = {
      if (usarParalelo) {
        pares.par.map { case (a, b) => a ++ b }.filter(o).seq
      } else {
        pares.map { case (a, b) => a ++ b }.filter(o)
      }
    }

    // 3. Expansión recursiva de cadenas hasta alcanzar longitud n
    def expandirCadenas(cadenas: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) cadenas.head
      else {
        val pares = generarCombinaciones(cadenas)
        val usarParalelo = pares.size > umbral
        val nuevasCadenas = combinarYFiltrar(pares, usarParalelo)
        expandirCadenas(nuevasCadenas, k * 2)
      }
    }

    // 4. Inicialización de cadenas a partir del alfabeto
    val cadenasIniciales: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))

    // 5. Llamada principal
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

    // Función recursiva que construye la cadena en paralelo
    @tailrec
    def construir(sc: Set[String], k: Int): String = {
      if (k >= n) {
        sc.find(w => o(w.toSeq)).getOrElse("")
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
        if s.sliding(k).forall(sub => pertenecer(sub, scTrie))
      } yield s).seq
    }
    // el metodo funciona como una recursion que verifica en bloques de potencias de 2 las cadenas candidatas
    @tailrec
    def verificarCadenas(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) {
        if (k >= umbral) sc.par.find(w => o(w)).getOrElse(Seq.empty) else sc.find(w => o(w)).getOrElse(Seq.empty)
      } else {
        // Filtrar combinaciones válidas según el árbol de sufijos
        val filtered = filtrarTrie(sc, k)
        // Evaluar con el oráculo, usando paralelismo si hay suficientes elementos
        val valid = if (filtered.size >= umbral) filtered.par.filter(o).toList else filtered.filter(o)
        verificarCadenas(valid, 2*k)
      }
    }

    val cadenasIniciales = alfabeto.map(c => Seq(c))
    verificarCadenas(cadenasIniciales,1)
  }

}
