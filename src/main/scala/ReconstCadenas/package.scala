import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  
  def reconstruirCadenaIngenuo(n:Int,o:Oraculo):Seq[Char] = {
    Seq('c')
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
    Seq('c')
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    Seq('c')
  }


}
