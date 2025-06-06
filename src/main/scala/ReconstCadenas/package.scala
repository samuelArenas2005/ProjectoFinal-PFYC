import ArbolSufijos.*
import Oraculo.*

import scala.annotation.tailrec


package object ReconstCadenas {


  // Este método consiste de manera sencilla generar todas las posibles combinaciones de secuencia de caracteres
  //con un tamaño n, para luego a partir de ellas encontrar la deseada por el oraculo
  //Se utilizaron estructura de datos perezosas para que no se calcule todas las cadenas, si no todas las cadenas hasta
  // encontrar la cadena objetivo

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    // Se llama de manera recursiva de cola cada combinación posible de caracteres
    def crearCadenas(chars: LazyList[Char], longitud: Int): LazyList[Seq[Char]] = {
      if (longitud == 1) chars.map(Seq(_)) // Caso base que genera las Seq con un solo caracter del alfabeto
      else for {
        char <- chars
        subCadena <- crearCadenas(chars,longitud - 1)  // <-- LLamado recursivo que crea todas las subCadenas de longitudes menor
        //Para ser posteriormente 'Concatenados' con cada caracter del alfabeto.
      }yield char +: subCadena // se añade a la lista los caracteres de los otros llamados recursivos.
    }

    // se llama a crear cadena a partir del alfabeto y un tamaño n, para luego
    // llamar al método find que se encargará de encontrar el elemento que cumpla con la condición
    //Una vez encontrado retorna un elemento de tipo Option, por lo que es necesario utilizar getOrElse
    //Para retonar el valor solicitado Seq[Char]
    crearCadenas(alfabeto.to(LazyList), n).find(o).getOrElse(Seq())

  }


  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    val vacia: Seq[Char] = Seq()

    /** genera el conjunto de nuevas subcanedas de longitud k, enxtendiendo cada subcadena valida actual con cada caracter del alfabeto.
     * nota: solo se conservan las subcadenas que el oraculo acepte. */
    def generarSubcadenasValidas(subcadenasActuales: Set[Seq[Char]]): Set[Seq[Char]] =
      for{
        subcadena <- subcadenasActuales
        letra <- alfabeto
        nuevaSubcadena = subcadena :+ letra
        if o(nuevaSubcadena)
      } yield nuevaSubcadena

    /** Construye recursivamente el conjunto de subcadenass de longitud k validas*/
    def construirSubcadenaValida(cadenasActuales:Set[Seq[Char]], k:Int):Seq[Char] ={
        val siguientesSubcadenas = generarSubcadenasValidas(cadenasActuales)
        siguientesSubcadenas.find(_.length == n) match{
          case Some(sol) => sol
          case None => construirSubcadenaValida(siguientesSubcadenas , k+1)
        }
    }
    /** se  inicia la construccion desde la cadena vacia, con subcadenas de longitud 0 */
    construirSubcadenaValida(Set(vacia),1)
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    
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

    val sc1 = alfabeto.flatMap(a => {
      val s = a.toString
      if (o(s.toSeq)) Some(s) else None
    }).toSet

    construir(sc1, 1).toList
  }




  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    def filtrarTrie(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val scTrie = arbolDeSufijos(sc)
      for {
        s1 <- sc
        s2 <- sc
        s: Seq[Char] = s1 ++ s2
        if s.sliding(k).forall(sub => pertenecer(sub, scTrie))
      } yield s
    }

    // el metodo funciona como una recursion que verifica en bloques de potencias de 2 las cadenas candidatas
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
