/**
 * Main.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxEntry}
import cse250.pa3.MapUtilities


object Main {
  def main(args: Array[String]): Unit = {
    val taxentryFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
    val entries = TaxEntry.loadEntries(taxentryFilename, 25)
    val mapXMLFile = "data/buffalo-map"
    val intersectionNodeXMLFile = "data/export.osm"
    val intersectionIDs = MapUtilities.loadIntersectionIDs(intersectionNodeXMLFile)
    val nodeToStreetMapping = MapUtilities.loadMapInfo(mapXMLFile)
    val streetGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)
    println(s"${entries(1)} to\n${entries(2)}")
    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(2)))
    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(2)))

//
//    println(s"${entries(3)} to\n${entries(24)}")
//    println(MapUtilities.computeFewestTurns(streetGraph, entries(3), entries(24)))
//    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(3), entries(24)))


//    val taxentryFilename = "data/2017-2018_Assessment_Roll-updated-smallo.csv"
//    val entries = TaxEntry.loadEntries(taxentryFilename, 5)
//    val mapXMLFile = "data/short.osm"
//    val intersectionNodeXMLFile = "data/export.osm"
//    val intersectionIDs = MapUtilities.loadIntersectionIDs(intersectionNodeXMLFile)
//    val nodeToStreetMapping = MapUtilities.loadMapInfo(mapXMLFile)
//    val streetGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)
//    println(s"${entries(1)} to\n${entries(2)}")
//    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(2)))
//    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(2)))

  }
}

//var base = mutable.Map("" -> "")
//
//var name = ArrayBuffer.empty[String]
//
//var iter: Seq[String] = Seq()
//
//var index = 0
//
//for(i <- tag) {
//  iter = i.map(tag => tag \@ "k")
//  index = 0
//  for (j <- iter) {
//  if (j == "tiger:name_base") {
//  name += i.map(tag => tag \@ "v")(index)
//}
//  index += 1
//}
//}