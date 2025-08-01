import scala.annotation.tailrec
package object ArbolSufijos {
  abstract class Trie
  case class Nodo (car:Char, marcada:Boolean,hijos:List[Trie]) extends Trie

  case class Hoja(car:Char, marcada:Boolean) extends Trie

  def raiz(t:Trie): Char = {
    t match{
      case Nodo(c,_,_) => c
      case Hoja(c,_) => c
    }
  }

  def cabezas(t:Trie): Seq[Char] = {
    t match{
      case Nodo(_,_,lt) => lt.map(t=>raiz(t))
      case Hoja(c,_) => Seq[Char](c)
    }
  }


  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    s match{
      case Nil => t match {
        case Nodo(_,m,_) => m
        case Hoja(_,m) => m
      }
      case x::xs => t match {
        case Nodo(_,_,hijos) =>
          hijos.find(raiz(_) == x) match {
            case Some(hijo) => pertenece(xs, hijo)
            case None => false
          }
        case _ => false
      }
    }
  }
  

  def adicionar(s: Seq[Char], t: Trie): Trie = {

    def adicionarRec(s: Seq[Char], hijos: List[Trie]): List[Trie] = {
      s match {
        case Nil => hijos
        case x :: xs =>
          hijos.span {
            case Nodo(c, _, _) => c != x
            case Hoja(c, _) => c != x
          } match {
            case (before, Nodo(c, fin, hs) :: after) =>
              val nuevosHijos = adicionarRec(xs, hs)
              val nuevoNodo = Nodo(c, fin || xs.isEmpty, nuevosHijos)
              before ++ (nuevoNodo :: after)

            case (before, Hoja(c, fin) :: after) =>
              val nuevoNodo =
                if (xs.isEmpty) Hoja(c, true)
                else Nodo(c, fin, adicionarRec(xs, Nil))
              before ++ (nuevoNodo :: after)

            case (before, Nil) =>
              val nuevoNodo =
                if (xs.isEmpty) Hoja(x, true)
                else Nodo(x, false, adicionarRec(xs, Nil))
              before :+ nuevoNodo
            case _ => hijos
          }
      }
    }

    t match {
      case Nodo(c, fin, hijos) =>
        val nuevosHijos = adicionarRec(s, hijos)
        Nodo(c, fin, nuevosHijos)
      case Hoja(_, _) =>
        Nodo('_', false, adicionarRec(s, Nil))
    }
  }
  
  def arbolDeSufijos(ss:Seq[Seq[Char]]):Trie = {

    @tailrec
    def arbolDeSufijosRec(ss:Seq[Seq[Char]], t:Trie):Trie = {

      @tailrec
      def arbolDeSufijosRec2(s:Seq[Char], t:Trie):Trie = {
        s match{
          case Nil => t
          case _ => arbolDeSufijosRec2(s.tail,adicionar(s,t))
        }
      }

      ss match{
        case Nil => t
        case x::xs => arbolDeSufijosRec(xs,arbolDeSufijosRec2(x,t))
      }
    }
    arbolDeSufijosRec(ss,Nodo('_',false,Nil))
  }
  


}
