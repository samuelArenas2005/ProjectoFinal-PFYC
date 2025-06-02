import ArbolSufijos.*
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.*
/*
val t: Trie = Nodo('_', false, List(
  Nodo('a', false, List(
    Nodo('c', true, List(
      Nodo('a', false, List(
        Hoja('c', true)
      )),
      Hoja('t', true)
    ))
  )),
  Nodo('c', true, List(
    Nodo('a', false, List(
      Nodo('c', true, List(
        Hoja('t', true)
      ))
    )),
    Hoja('t', true)
  )),
  Hoja('t', true)
))

val a = Seq('a')

pertenecer(a,t)

val t1 = Nodo('_', false, Nil)
val t2 = adicionar("cat".toList, t1)
val t3 = adicionar("car".toList, t2)
val t4 = adicionar("cac".toList, t3)
val t5 = adicionar("ca".toList,t4)

println(pertenecer("cat".toList, t4))
println(pertenecer("car".toList, t4))
println(pertenecer("cac".toList, t4))
println(pertenecer("ca".toList, t4))

println(pertenecer("ca".toList, t5))


val m =
  Nodo('_',false,List(
    Nodo('c',false,List(
      Nodo('a',false,List(
        Hoja('t',true), Hoja('r',true), Hoja('c',true))
      ))
    ))
  )

val prueba1 = Seq('a','c','a','c')
val prueba2 = Seq('c','a','c','t')

arbolDeSufijos(Seq(prueba1,prueba2))

val tprueba: Trie = Nodo(' ', false, List(
  Nodo('a', false, List(
    Nodo('c', true, List(
      Nodo('a', false, List(
        Hoja('c', true)
      )),
      Hoja('t', true)
    ))
  )),
  Nodo('c', true, List(
    Nodo('a', false, List(
      Nodo('c', true, List(
        Hoja('t', true)
      ))
    )),
    Hoja('t', true)
  )),
  Hoja('t', true)
))

val palabras: Seq[Seq[Char]] = Seq(
  "casa".toList,
  "cama".toList,
)
*/

val seq = Seq('g','t','a','c')
val o = crearOraculo(1)(seq)
val n = seq.length

//reconstruirCadenaTurbo(n,o)
reconstruirCadenaTurboAcelerada(n,o)
//reconstruirCadenaTurboMejorada(n,o)

/*
def filtrar(SC:Seq[Seq[Char]],k: Int):Seq[Seq[Char]] = {
  val cadenasCandidatas: Seq[Seq[Char]] = for {
    chain <- SC
    subChain <- SC
    cadenaAFiltrar = chain ++ subChain
    if SC.containsSlice(cadenaAFiltrar)
  } yield cadenaAFiltrar
  cadenasCandidatas
}

def verificarCadenas(SC:Seq[Seq[Char]],k: Int):Seq[Char] ={

  if (k/2 == 1){
    val cadenasCandidatas: Seq[Seq[Char]] = for {
      char <- alfabeto
      subChar <- alfabeto
      if o(Seq(char, subChar))
    } yield Seq(char, subChar)
    verificarCadenas(filtrar(cadenasCandidatas,k),k*2)
  }
  else{
    val cadenasCandidatas: Seq[Seq[Char]] = for {
      chain <- SC
      subChain <- SC
      if o(chain ++ subChain)
    } yield chain ++ subChain

    if(k < n) verificarCadenas(filtrar(cadenasCandidatas,k),k*2) else cadenasCandidatas.head
  }

}
verificarCadenas(Nil,1)*/

val a = List(Seq('a','c'),Seq('a','t'),Seq('t','a'))
val k = 2



