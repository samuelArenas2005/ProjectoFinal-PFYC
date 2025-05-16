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
  
  def pertencer(s:Seq[Char],t:Trie): Boolean = {
    true
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    val v = Nodo('c',true,List())
    v
  }
  
  def arbolDeSufijos(ss:Seq[Seq[Char]]):Trie = {
    val v = Nodo('c', true, List())
    v
  }


}
