import ArbolSufijos.*
import Oraculo.*

import scala.annotation.tailrec

package object ReconstCadenas {
  
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def crearCadenas(chars: LazyList[Char], longitud: Int): LazyList[Seq[Char]] = {
      if (longitud == 1) chars.map(Seq(_)) 
      else for {
        char <- chars
        subCadena <- crearCadenas(chars,longitud - 1) 
      }yield char +: subCadena 
    }
    crearCadenas(alfabeto.to(LazyList), n).find(o).getOrElse(Seq())
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val vacia: Seq[Char] = Seq()

    def generarSubcadenasValidas(subcadenasActuales: Set[Seq[Char]]): Set[Seq[Char]] =
      for {
        subcadena <- subcadenasActuales
        letra <- alfabeto
        nueva = subcadena :+ letra
        if o(nueva)
      } yield nueva

    @tailrec
    def construirSubcadenaValida(cadenasActuales: Set[Seq[Char]]): Seq[Char] = {
      val siguientesSubcadenas = generarSubcadenasValidas(cadenasActuales)
      if (siguientesSubcadenas.head.length == n) siguientesSubcadenas.head
      else construirSubcadenaValida(siguientesSubcadenas)
    }

    construirSubcadenaValida(Set(vacia))
  }

  
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    
    @tailrec
    def verificarCadenas(SC: Seq[Seq[Char]], k: Int): Seq[Char] = {
        if (k < n) {
          val cadenasCandidatas: Seq[Seq[Char]] = for {
            chain <- SC
            subChain <- SC
            if o(chain ++ subChain)
          } yield chain ++ subChain
          verificarCadenas(cadenasCandidatas, k*2)
        } else SC.head
        
    }

    val cadenasIniciales = alfabeto.map(c => Seq(c))
    verificarCadenas(cadenasIniciales, 1)

  }
  

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    def filtrar(sc: Set[String], k: Int): Set[String] = {
      for {
        s1 <- sc
        s2 <- sc
        s = s1 + s2
        if s.sliding(k).forall(sc.contains)
      } yield s
    }

    @tailrec
    def construir(sc: Set[String], k: Int): String = {
      if (k >= n) {
        sc.find(w => w.length == n && o(w.toSeq)).getOrElse("")
      } else {
        val filtered = filtrar(sc, k)
        val candidates = filtered
        val valid = candidates.filter(w => o(w.toSeq))
        construir(valid, 2 * k)
      }
    }

    val sc1 = alfabeto.map(_.toString).toSet

    construir(sc1, 1).toList
  }




  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    def filtrarTrie(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val scTrie = arbolDeSufijos(sc)
      for {
        s1 <- sc
        s2 <- sc
        s: Seq[Char] = s1 ++ s2
        if s.sliding(k).forall(sub => pertenece(sub, scTrie))
      } yield s
    }
    
    @tailrec
    def verificarCadenas(sc: Seq[Seq[Char]], k: Int): Seq[Char] = {
      if (k >= n) {
        sc.find(w => o(w)).getOrElse(Seq.empty)
      } else {
        val filtered: Seq[Seq[Char]] = filtrarTrie(sc, k)
        val valid = filtered.filter(o)
        verificarCadenas(valid, 2*k)
      }
    }

    val cadenasIniciales = alfabeto.map(c => Seq(c))
    verificarCadenas(cadenasIniciales,1)

  }


}
