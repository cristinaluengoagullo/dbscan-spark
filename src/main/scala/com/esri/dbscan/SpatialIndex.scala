package com.esri.dbscan

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

/**
  * Spatial index to quickly location neighbors of a point.
  * The implementation is based on a grid, where all the indexed points are grouped together based on the cell in the grid that they fall into.
  *
  * @param eps the cell size.
  */
case class SpatialIndex(eps: Double) {

  type SIKey = List[Int]
  type SIVal = mutable.ArrayBuffer[DBSCANPoint]

  val grid = mutable.Map[SIKey, SIVal]()

  /**
    * Index supplied point.
    *
    * @param point the point to index.
    * @return this spatial index.
    */
  def +(point: DBSCANPoint) = {
    var indexes = List[Int]()
    for (i <- 0 to point.coords.size-1) {
	val index = (point.coords(i) / eps).floor.toInt
	indexes = indexes :+ index
    }

    grid.getOrElseUpdate(indexes, ArrayBuffer[DBSCANPoint]()) += point
    this
  }

  /**
    * Filter all points within a distance <= eps of the given point
    * @param origPoint the point to search around.
    * @param origIndexes indexes of the cell to which the point is mapped
    * @param accumIndexes indexes to 
    * @return a sequence of points that are within a distance <= eps of the given point. 
    */
  def filterNeighbors(origPoint: DBSCANPoint, origIndexes: List[Int], accumIndexes: List[Int]) : Seq[DBSCANPoint] = {
    var points = Seq[DBSCANPoint]()
    if (origIndexes.isEmpty) {
        points = grid.getOrElse(accumIndexes, Seq.empty)
                     .filter(point => origPoint.distance(point) <= eps)
    } else {
        val index = origIndexes(0)
        var indexes = List[Int]()
        if (origIndexes.size > 1) {
            indexes = origIndexes.slice(1,origIndexes.size)
        } 
        points = (index - 1 to index + 1).flatMap{i => filterNeighbors(origPoint,indexes,accumIndexes:+i)}
    }
    points
  }

  /**
    * Find all the neighbors of the specified point.
    * @param point the point to search around.
    * @return a sequence of points that are within a distance <= eps of the given point.
    */
  def findNeighbors(point: DBSCANPoint) = {
    var indexes = List[Int]()
    for (i <- 0 to point.coords.size-1) {
        val index = (point.coords(i) / eps).floor.toInt
        indexes = indexes :+ index
    }
    filterNeighbors(point,indexes,List[Int]())
  }
}
