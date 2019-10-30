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
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxEntry}

import scala.collection.mutable
import scala.xml.XML

object MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] = ???

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = ???

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
