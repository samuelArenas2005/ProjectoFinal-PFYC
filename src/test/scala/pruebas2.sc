import ArbolSufijos.*
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.*
import ReconstCadenasPar.*
import common.*
import Benchmark.*

import scala.util.Random


//val t1 = Nodo('_', false, Nil)
//val t2 = adicionar("cat".toList, t1)
//val t3 = adicionar("car".toList, t2)
//val t4 = adicionar("cac".toList, t3)
//val t5 = adicionar("ca".toList,t4)
//val t40 = adicionar("m".toList,t5)
//
//val t6 = adicionar2("cat".toList, t1)
//val t7 = adicionar2("car".toList, t6)
//val t8 = adicionar2("cac".toList, t7)
//val t9 = adicionar2("ca".toList,t8)
//
//println(pertenecer("cat".toList, t4))
//println(pertenecer("car".toList, t4))
//println(pertenecer("cac".toList, t4))
//println(pertenecer("ca".toList, t4))
//
//println(pertenecer("ca".toList, t5))
//
//
//val m =
//  Nodo('_',false,List(
//    Nodo('c',false,List(
//      Nodo('a',false,List(
//        Hoja('t',true), Hoja('r',true), Hoja('c',true))
//      ))
//    ))
//  )
//
//val prueba1 = Seq('a','c','a','c')
//val prueba2 = Seq('c','a','c','t')
//val prueba = Seq('t','a')
//
//
//val arbol = arbolDeSufijos(Seq(prueba1,prueba2,prueba))
//
//pertenecer(Seq('t','a'),arbol)
//
//
//val tprueba: Trie = Nodo(' ', false, List(
//  Nodo('a', false, List(
//    Nodo('c', true, List(
//      Nodo('a', false, List(
//        Hoja('c', true)
//      )),
//      Hoja('t', true)
//    ))
//  )),
//  Nodo('c', true, List(
//    Nodo('a', false, List(
//      Nodo('c', true, List(
//        Hoja('t', true)
//      ))
//    )),
//    Hoja('t', true)
//  )),
//  Hoja('t', true)
//))

//cabezas(tprueba).contains('a')

def generarSecuencia(n: Int): Seq[Char] = {
  val longitud = math.pow(2, n).toInt
  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
}

val a = generarSecuencia(6)

val n = a.length
val o = crearOraculo(1)(a)


//
//val a0 = reconstruirCadenaIngenuo(n,o)
//
//val a01 = reconstruirCadenaIngenuoPar(2)(n,o)

//val a1 = reconstruirCadenaMejorado(n,o)
//
//val a2 = reconstruirCadenaMejoradoPar(2)(n,o)
//
//val a3 = reconstruirCadenaTurbo(n,o)
//
//val a4 = reconstruirCadenaTurboPar(2)(n,o)

//val a5 = reconstruirCadenaTurboMejorada(n,o)
//
//val a6 = reconstruirCadenaTurboMejoradaPar(2)(n,o)
//
//val a7 = reconstruirCadenaTurboAcelerada(n,o)
//
//val a8 = reconstruirCadenaTurboAceleradaPar(2)(n,o)

//a0 == a
//a01 == a


//a1 == a
//a2 == a
//a3 == a
//a4 == a
//a5 == a
//a6 == a
//a7 == a
//a8 == a

//compararAlgoritmosPar(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar)(0)(n,o)
//
//compararAlgoritmosPar(reconstruirCadenaMejorado,reconstruirCadenaMejoradoPar)(0)(n,o)
//
//compararAlgoritmosPar(reconstruirCadenaTurbo,reconstruirCadenaTurboPar)(2)(n,o)
//
//compararAlgoritmosPar(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaPar)(0)(n,o)
//
//compararAlgoritmosPar(reconstruirCadenaTurboAcelerada,reconstruirCadenaTurboAceleradaPar)(2)(n,o)
//
////
//
//compararAlgoritmosSec(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada)(n,o)
//
//compararAlgoritmosSec(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada)(n,o)
//
//compararAlgoritmosSec(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada)(n,o)
//
//compararAlgoritmosSec(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada)(n,o)
//
////
//compararAlgoritmosSec(reconstruirCadenaMejorado,reconstruirCadenaTurboMejorada)(n,o)
//
//compararAlgoritmosSec(reconstruirCadenaMejorado,reconstruirCadenaTurboAcelerada)(n,o)
//
//compararAlgoritmosSec(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboAcelerada)(n,o)




