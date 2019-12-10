package com.crader.advent.year2019

import com.crader.advent.SingleInputSolution
import com.crader.advent.util.InputFile

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.util.Try

object Day6 extends SingleInputSolution[Map[String, String], Int, Int] {

  override protected def parse(inputPath: String): Try[Map[String, String]] = {
    InputFile.fromMultipleLines(inputPath) { line =>
      val split =  line.split(')')
      // orbiting objects are mapped to the objects that they orbit
      (split(1), split(0))
    }(Seq.toFactory(Iterator)).map(_.toMap)
  }

  // only one object may orbit another
  override def part1(orbits: Map[String, String]): Try[Int] = Try { countAllOrbits(orbits) }

  /**
   * I think the naive solution of crawling the orbit path for each object, without caching, has an approximately
   * quadratic time complexity. I wanted see if I could do better than that:
   *
   * This solution involves several passes (which could probably be reduced), but should still have a linear time
   * complexity. It works by starting at each leaf node (an object that is in orbit, but is not orbited itself) and
   * crawling down its indirect orbit path, keeping track of the entire path as it goes. After an entire path is
   * crawled, the path length (number of direct & indirect orbits) for each component object is cached. This enables
   * later path crawls to stop early - never crawling the same sub-path twice. When all paths are explored, the path
   * length cache can be used to total up the number of direct & indirect orbits.
   */
  private def countAllOrbits(orbits: Map[String, String]): Int = {
    // cache orbit counts so we don't have to crawl the same path multiple times
    val orbitCountsByObj = mutable.HashMap[String, Int]()

    @tailrec
    def followOrbitPath(obj: String, orbitPath: Seq[String]): Unit = {
      if (orbitCountsByObj.contains(obj)) {
        // the rest of this orbit path has already been crawled, no need to keep going
        val offset = orbitCountsByObj(obj) + 1
        orbitCountsByObj.addAll(orbitPath.zip(offset until (orbitPath.size + offset)))
      } else if (orbits.contains(obj)) {
        followOrbitPath(orbits(obj), obj +: orbitPath)
      } else {
        orbitCountsByObj.addAll(orbitPath.zip(1 to orbitPath.size))
      }
    }

    // Multiple passes, this can be improved
    val leafNodes = orbits.keySet -- orbits.valuesIterator.toSet
    leafNodes.foreach(followOrbitPath(_, Seq()))

    orbitCountsByObj.valuesIterator.sum
  }


  override protected def part2(orbits: Map[String, String]): Try[Int] = Try {
    val startObj = orbits.getOrElse("YOU", throw new IllegalArgumentException("Could not find 'YOU' in orbit input"))
    val endObj = orbits.getOrElse("SAN", throw new IllegalArgumentException("Could not find 'SAN' in orbit input"))
    shortestPath(startObj, endObj, orbits)
  }

  /**
   * We already have:
   *  - The "Universal Orbit Map" as fully-connected tree (single forward reference/orbit)
   *  - Both the start and end positions
   *
   * Therefore, there exists only a single path (without back-tracking) between YOU and SAN. Knowing this, we can crawl
   * each forward orbit from both YOU and SAN until we've found a common object; this is a specialized version of a
   * Breadth-First Search. It's necessary to crawl from both YOU and SAN because either (or both) node could be on a
   * separate orbit branch, and therefore inaccessible by crawling forward orbits from the either node.
   */
  private def shortestPath(startObj: String, endObj: String, orbits: Map[String, String]): Int = {
    // Using Options here to handle to case of one path ending before the other or before finding the answer
    @tailrec
    def crawl(objFromStart: Option[String],
              objFromEnd: Option[String],
              distance: Int,
              distancesFromStart: Map[String, Int],
              distancesFromEnd: Map[String, Int]): Int = {
      if (objFromStart.exists(distancesFromEnd.contains)) {
        distance + distancesFromEnd(objFromStart.get)
      } else if (objFromEnd.exists(distancesFromStart.contains)) {
        distancesFromStart(objFromEnd.get) + distance
      } else if (objFromStart.nonEmpty || objFromEnd.nonEmpty) {
        crawl(
          objFromStart.flatMap(orbits.get),
          objFromEnd.flatMap(orbits.get),
          distance + 1,
          objFromStart.fold(distancesFromStart)(o => distancesFromStart + (o -> distance)),
          objFromEnd.fold(distancesFromEnd)(o => distancesFromEnd + (o -> distance)),
        )
      } else {
        throw new IllegalStateException(
          "The Universal Orbit Map does not form a fully-connected tree! Has the map been corrupted?"
        )
      }
    }

    crawl(
      Some(startObj),
      Some(endObj),
      0,
      // avoid using Map1 by explicitly instantiating a HashMap
      immutable.HashMap(startObj -> 0),
      immutable.HashMap(endObj -> 0)
    )
  }
}
