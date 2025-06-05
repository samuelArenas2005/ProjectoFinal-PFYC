import ArbolSufijos.*
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.*
import ReconstCadenasPar.*
import common.*
import Benchmark.*

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

//val seq = Seq('a','g','c','a','a','t','t','a','a','g','c','a','a','t','t','a','a','g','c','a','a','t','t','a','a','g','c','a','a','t','t','a')
//val seq2 = Seq('a','a','a','a','a','a','a','a','a','a','a','a','a','t','a','a')
//val o = crearOraculo(1)(seq2)
//val n = seq2.length
//val umbral = 2

//reconstruirCadenaIngenuoPar2(umbral)(n,o)

//reconstruirCadenaIngenuo(n,o)
//reconstruirCadenaIngenuoPar(umbral)(n,o)
//reconstruirCadenaIngenuoPar2(umbral)(n,o)
//reconstruirCadenaTurboMejorada(n,o)
//
//compararAlgoritmosPar(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar)(umbral)(n,o)

//compararAlgoritmosPar(reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar)(umbral)(n,o)

//def crearCadenas(chars: List[Char], longitud: Int): List[Seq[Char]] = {
//  if (longitud == 1) chars.map(Seq(_))
//  else for {
//    char <- chars
//    subCadena <- crearCadenas(alfabeto.to(List), longitud - 1)
//  } yield char +: subCadena
//}
//
//
//
//val cadenas1 = task(crearCadenas(alfabeto.slice(0,1).to(List),4))
//val cadenas2 = task(crearCadenas(alfabeto.slice(1,2).to(List),4))
//val cadenas3 = task(crearCadenas(alfabeto.slice(2,3).to(List),4))
//val cadenas4 = task(crearCadenas(alfabeto.slice(3,4).to(List),4))
//
//cadenas1.join()
//cadenas2.join()
//cadenas3.join()
//cadenas4.join()


//val cadenaencontrada = cadenas1.join() ++ cadenas2.join() ++ cadenas3.join() ++ cadenas4.join()
//val o2 = crearOraculo(1)(Seq('a','a','c','a'))
//
//cadenaencontrada.find(o2)
//
//cadenaencontrada


//def crearCadenas(chars: List[Char], longitud: Int): List[Seq[Char]] = {
//  if (longitud == 1) chars.map(Seq(_))
//  else for {
//    char <- chars
//    subCadena <- crearCadenas(alfabeto.to(List), longitud - 1)
//  } yield char +: subCadena
//}
//
//crearCadenas(List('a','c'),2)
//crearCadenas(List('g','t'),2)
//
//alfabeto.slice(0,2)
//alfabeto.slice(2,4)
//
//val task1 = task(crearCadenas(List('a','c'),2))


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

//val b = List(Seq('a','c'),Seq('a','t'),Seq('t','a'))
//val k = 2



