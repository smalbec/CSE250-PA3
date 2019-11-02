/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:smalbec
 * Person#:50280232
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3


import cse250.objects.{StreetGraph, TaxEntry}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.{NodeSeq, XML}

object   MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] ={

    val xml = XML.loadFile(filename)

    return  (xml \ "node").map(node => node \@ "id").to(mutable.Set)

  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {

    val xml = XML.loadFile(filename)

    var returnMap = mutable.Map("" -> ArrayBuffer(""))

    val ids =  (xml \ "node").map(node => node \@ "id").to(mutable.Set)

    val way = (xml \\ "way")

    val nd = way.map(way => way \\ "nd")

    val tag = way.map(way => way \\ "tag")

    var ref = ArrayBuffer(ArrayBuffer(""))

    var base = mutable.Map("" -> ArrayBuffer(""))

    var name = ArrayBuffer.empty[String]

    var iter: Seq[String] = Seq()

    var index = 0

    var namescount = 0

    for(i <- tag) {
      iter = i.map(tag => tag \@ "k")
      index = 0
      for (j <- iter) {
        if (j == "tiger:name_base") {
          name += i.map(tag => tag \@ "v")(index)
          namescount += 1
        }
        index += 1
      }
    }

    for(i<-nd){
      ref += i.map(i => i \@ "ref").to(ArrayBuffer)
    }

    ref.remove(0)

    var repeatedNames = ArrayBuffer("")



    for(i <- 0 until namescount){
      if(base.contains(name(i))){
        repeatedNames = base(name(i)) ++ ref(i)
        base(name(i)) = repeatedNames
      }
      else{base.addOne((name(i) -> ref(i)))}

    }

    var intersections = ArrayBuffer("")

    //i loops through all node id
    for(i<- ids) {
      intersections = ArrayBuffer("")
      // k,v loops through all names and ArrayBuffer of id ref
      returnMap += (i -> ArrayBuffer(""))
      for ((k, v) <- base) {
        if (v.contains(i)) {
          //j loops through all id ref
          for(j<-v){
            if(j == i ){
              intersections += k
            }

          }
          returnMap(i) = intersections
        }
      }
    }




    return null

  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {
    val streetGraph = new StreetGraph
    streetGraph
  }

  def computeFewestTurns(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Int = {
    -1
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    List()
  }
}

