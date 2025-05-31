import ArbolSufijos.*
import Oraculo.*


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
    Seq('c')
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    
    val cadenasIniciales = for {
      char <- alfabeto
      subChar <- alfabeto
      if o(Seq(char, subChar))
    } yield Seq(char, subChar)

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

    verificarCadenas(cadenasIniciales, 2)

  }


//  def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
//    def filtroRec(cadena: Seq[Char], c: Int): Boolean = {
//      if (k / 2 - c != 0) {
//        if (SC.contains(cadena.slice(k - k / 2 - c, k + k / 2 - c)) &&
//          SC.contains(cadena.slice(k - k / 2 + c, k + k / 2 + c)))
//          filtroRec(cadena, c + 1) else false
//      } else {
//        true
//      }
//    }
//
//    val cadenasCandidatas = for {
//      chain <- SC
//      subChain <- SC
//      cadenaAFiltrar = chain ++ subChain
//      if filtroRec(cadenaAFiltrar, 0)
//    } yield cadenaAFiltrar
//
//    cadenasCandidatas
//  }
//
//  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
//
//
//    val cadenasIniciales = for {
//      char <- alfabeto
//      subChar <- alfabeto
//      if o(Seq(char, subChar))
//    } yield Seq(char, subChar)
//
//    def verificarCadenas(SC:Seq[Seq[Char]],k: Int):Seq[Char] ={
//        if(k >= n){
//          val cadenaFinal = for{
//            cadena <- SC
//            if o(cadena)
//          }yield cadena
//
//          cadenaFinal.head
//
//        }else{
//          val cadenasCandidatas: Seq[Seq[Char]] = for {
//            chain <- SC
//            subChain <- SC
//            if o(chain ++ subChain)
//          } yield chain ++ subChain
//          if (k*2 < n){
//            val cadenaFiltrada = filtrar(cadenasCandidatas, k * 2)
//            verificarCadenas(cadenaFiltrada, k*4)
//          } else cadenasCandidatas.head
//      }
//
//    }
//
//    if(n>2) verificarCadenas(filtrar(cadenasIniciales,2),4) else cadenasIniciales.head
//
//
//  }


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
        val nextK = 2 * k
        val filtered = filtrar(sc, k)
        val candidates = filtered
        val valid = candidates.filter(w => o(w.toSeq))
        valid.find(_.length == n).getOrElse(construir(valid, nextK))
      }
    }

    val sc1 = alfabeto.flatMap(a => {
      val s = a.toString
      if (o(s.toSeq)) Some(s) else None
    }).toSet

    construir(sc1, 1).toList
  }



  //La función filtrarTrie es un filtro que utiliza el metodo reconstruirCadenaTurboAcelerado
  // este genera nuevas cadenas de longitud 2k, donde k es la longitud de la cadena que recibe
  //estas nuevas cadenas pasan a traves de un filtro que evita la redundancia, es decir elimina
  // cadenas que con el conjunto SC anterior se sabe que no son candidatas validas
  //SC <- Secuencia Candidata
  // Este algoritmo utiliza el arbol de sufijos para ser creado

  def filtrarTrie(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
    val SCtrie = arbolDeSufijos(SC) // Crea el arbol de sufijos

    //Recibe una cadena candidata nueva y va a determinar si las subcadenas son validas
    // de no serlo retorna false
    // De si serlo recorrera todas las otras posibles subcadenas
    def filtroRec(cadena: Seq[Char], c: Int): Boolean = {
      if (k / 2 - c != 0) {
        if (pertenecer(cadena.slice(k - k / 2 - c, k + k / 2 - c), SCtrie) &&
          pertenecer(cadena.slice(k - k / 2 + c, k + k / 2 + c), SCtrie))
          filtroRec(cadena, c + 1) else false
      } else {
        true
      }
    }

    //Genera todas las nuevas cadenas candidatas que va a filtrar
    val cadenasCandidatas = for {
      chain <- SC
      subChain <- SC
      cadenaAFiltrar = chain ++ subChain
      if filtroRec(cadenaAFiltrar, 0) // Acá se aplica el filtro
    } yield cadenaAFiltrar

    cadenasCandidatas
  }

  //Este método genera las cadenas en bloques de 2 y va verificando si son pertenecen al oraculo, y asi ir descartando opciones
  // que al final se sobreentiende como no validas, adicionalmente utiliza el filtro para evitar redundancias.
  // SC <-- Secuencia Candidata

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    // Aca se generan las cadenas iniciales de longitud 2 que sean subcadenas de la cadena Final
    val cadenasIniciales = for {
      char <- alfabeto
      subChar <- alfabeto
      if o(Seq(char, subChar))
    } yield Seq(char, subChar)

    // el metodo funciona como una recursion que verifica en bloques de potencias de 2 las cadenas candidatas
    def verificarCadenas(SC: Seq[Seq[Char]], k: Int): Seq[Char] = {
      //Entra a este caso cuando el filtro genero cadenas de la misma longitud de la cadena final, por lo que solo basta con encontrar
      // la cadena final.
        if (k >= n) {
          val cadenaFinal = for {
            cadena <- SC
            if o(cadena)
          } yield cadena

          cadenaFinal.head

        } else { // en caso de que el filtro no genero las cadenas candidatas de longitud n, entonces generá nuevas
          //cadenas
          val cadenasCandidatas: Seq[Seq[Char]] = for {
            chain <- SC
            subChain <- SC
            if o(chain ++ subChain)
          } yield chain ++ subChain
          if (k * 2 < n) { // Una vez generadas verifica si la estas cadenas son de una longitud menor a la buscada
            // en caso de que si pasará por el filtro que generá cadenas del doble de longitud y se llamará nuevamente
            // a la función de forma recursiva.
            val cadenaFiltrada = filtrarTrie(cadenasCandidatas, k * 2)
            verificarCadenas(cadenaFiltrada, k * 4)
          } else cadenasCandidatas.head // en el caso de que la cadena generada ya sea el tamaño buscado entonces ya hemos
          // encontrado nuestra cadena
        }
      }

    // Caso necesario para cadenas de longitud 2 debido a que estas se generan en las cadenas iniciales y por lo tanto
    // el filtro ya deberá haberla encontrado.
    if(n>2) verificarCadenas(filtrarTrie(cadenasIniciales,2),4) else cadenasIniciales.head


  }


}
