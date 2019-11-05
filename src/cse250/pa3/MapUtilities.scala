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

import scala.util.control.Breaks._
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
              returnMap(node) = (returnMap(node) + name.toUpperCase())
            }
            else{
              returnMap += (node -> mutable.Set(name.toUpperCase()))
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

    if(!streetGraph.vertices.contains(cur) || !streetGraph.vertices.contains(dest)){
      return -1
    }


    var pathCount = 0
    var prevSize = 1
    var curSize = 1
    var distance = 1
    var ogcurSize = curSize



    var map: mutable.Map[Int, mutable.Set[String]] = mutable.Map(0-> mutable.Set(cur))

    if(cur == dest){
      return pathCount
    }

    var size = 1000

    var queue: mutable.Queue[String] = mutable.Queue()
    var visit = Set(cur)
    queue.enqueue(cur)

    var set = mutable.Set.empty[String]

    while(queue.nonEmpty){
      val current = queue.dequeue()
      val neighbors = streetGraph.vertices(current).edges


      if(curSize == 0){
        map += (distance -> set)
        prevSize += -1
        curSize = neighbors.size
      }
      if(prevSize == 0){
        distance += 1
        prevSize = ogcurSize
        set = mutable.Set.empty
      }


      curSize = neighbors.size
      ogcurSize = neighbors.size

        for(edge<- neighbors){
          set += edge.name
          curSize += -1
          size += -1
            if(!visit.contains(edge.name)){
              if(edge.name == dest){
                return pathCount + 1
              }
            queue.enqueue((edge.name))
            visit += edge.name
          }
        }
        pathCount += 1
    }
    -1
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxEntry, end: TaxEntry): Seq[String] = {
    val cur = start.infoMap("STREET")
    val dest = end.infoMap("STREET")

    if(!streetGraph.vertices.contains(cur) || !streetGraph.vertices.contains(dest)){
      return Seq()
    }

    var vertexDistance =  mutable.Map(0 -> mutable.Set(cur))

    var index = 1

    var shortestPath = Seq(cur)




    if(cur == dest){
      return List(cur)
    }

    var queue: mutable.Queue[String] = mutable.Queue()
    var visit = Set(cur)
    queue.enqueue(cur)

    while(queue.nonEmpty){
      val current = queue.dequeue()
      val neighbors = streetGraph.vertices(current).edges
     (vertexDistance += (index -> mutable.Set()))

      for(edge<- neighbors){
        if(!visit.contains(edge.name)){
//          if(edge.name == dest){
//
//          }
          queue.enqueue((edge.name))
          visit += edge.name
          vertexDistance(index) = vertexDistance(index) + edge.name
        }
      }
      index +=1
    }

    var seq = vertexDistance.toSeq.reverse

    var reverseQueue: mutable.Queue[String] = mutable.Queue()
    reverseQueue.enqueue(dest)

    var path :Seq[String] = Seq(dest)

    var length = 2

    breakable {while(reverseQueue.nonEmpty) {
      val current = reverseQueue.dequeue()
      val neighbors = streetGraph.vertices(current).edges

      for(edge<- neighbors){
        if(seq(length)._2.contains(edge.name)){
          breakable {for(i<-seq(length)._2){
            if(i == edge.name){
              path = path :+ i
              reverseQueue.enqueue(edge.name)
              length += 1
            if(i == cur) {
              return path.reverse
            }
              break
            }
          }}
        }
      }


      //path += current

    }}
    //val neighbors = streetGraph.vertices(current).edges




    Seq()
  }
}