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
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}
import scala.xml.{NodeSeq, XML}

object   MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] ={

    val xml = XML.loadFile(filename)

    return  (xml \ "node").map(node => node \@ "id").to(mutable.Set)

  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {

    val xml = XML.loadFile(filename)

    var returnMap = mutable.Map("" -> mutable.Set(""))

    val way = (xml \\ "way")

    var name = ""

    var iter: Seq[String] = Seq()

    var nodes: mutable.HashSet[String] = mutable.HashSet()

    var index = 0

    var namescount = 0

    for(i <- way) {
      val nd = i.map(i => i \\ "nd")(0)
      val tag = i.map(i => i \\ "tag")(0)
      iter = tag.map(tag => tag \@ "k")
      nodes = nd.map(nd => nd \@ "ref").to(mutable.HashSet)
      index = 0
      if (iter.contains("tiger:name_base")) {
        for(j<- iter){
          if (j == "tiger:name_base") {
            name = tag.map(tag => tag \@ "v")(index)
          }
            index += 1
        }
        for(node <- nodes){
            if(returnMap.contains(node)){
              returnMap(node) = (returnMap(node) + name)
            }
            else{
              returnMap += (node -> mutable.Set(name))
            }
          }
        }
      }

    returnMap -= ""



    return returnMap

  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]])
                                          : StreetGraph = {
    val streetGraph = new StreetGraph

    //    var filteredd = nodeToStreetMapping filterKeys intersectionIDs
    //
    //    var filtered = nodeToStreetMapping.view.filterKeys(intersectionIDs)

    var keys = nodeToStreetMapping.keySet intersect intersectionIDs


    var tempvertices = scala.collection.Set("")

    for (intersections <- keys) {
      if (nodeToStreetMapping(intersections).size != 1) {
        for (vertices <- nodeToStreetMapping(intersections)) {
          tempvertices = nodeToStreetMapping(intersections).filter(_ != vertices)
          tempvertices.foreach(vertex => streetGraph.insertEdge(vertex.toUpperCase(), vertices.toUpperCase))
          //streetGraph.insertVertex(vertices)
        }
      }
    }

    //      for((k,v)<- streetGraph.vertices){
    //        tempvertices = streetGraph.vertices.keySet.filter(_ != k)
    //        for(i<-tempvertices){
    //          streetGraph.insertEdge(k,i)
    //        }
    //      }
    //    }

    streetGraph
  }

  def computeFewestTurns(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Int = {

    val cur = start.infoMap("STREET")
    val dest = end.infoMap("STREET")

    var pathCount = 0

    var queue: mutable.Queue[String] = mutable.Queue()
    var visit = Set(cur)
    queue.enqueue(cur)

    while(queue.nonEmpty){
      val current = queue.dequeue()
      if(current == dest){
        return pathCount
      }
      else{
        val neighbors = streetGraph.vertices(cur).edges
        for(edge<- neighbors){
            if(!visit.contains(edge.name)){
            queue.enqueue((edge.name))
            visit += edge.name
          }
        }
      }
    }
    pathCount
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    List()
  }
}