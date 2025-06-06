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
//
//arbolDeSufijos(Seq(prueba1,prueba2))
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
//

def generarSecuencia(n: Int): Seq[Char] = {
  val longitud = math.pow(2, n).toInt
  Seq.fill(longitud)(alfabeto(Random.nextInt(alfabeto.length)))
}

val a = generarSecuencia(0)
val n = a.length
val o = crearOraculo(1)(a)

reconstruirCadenaIngenuo(n,o)




