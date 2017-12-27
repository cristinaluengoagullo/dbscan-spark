package com.esri.dbscan

import scala.collection.mutable.ArrayBuffer
import math._

/**
  * Location in 2D space.
  *
  * @param id the point identifier.
  * @param x  the horizontal 2D placement.
  * @param y  the vertical 2D placement.
  */
class Point(val id: Long, val coords: List[Double]) extends Serializable {

  /**
    * Check if supplied point is the same as this point. This is a naive implementation as it checks only the point identifier.
    *
    * @param other the other point.
    * @return if the other point id is the same as this point id.
    */
  override def equals(other: Any): Boolean = other match {
    case that: Point => id == that.id
    case _ => false
  }

  /**
    * Hash representation of this point
    *
    * @return the hash of the point id.
    */
  override def hashCode(): Int = {
    Smear.smear(id)
  }

  
  def distance(other: Point) = {
    sqrt((coords zip other.coords).map { case (x,y) => pow(y - x, 2) }.sum)
  }


  /**
    * Convert this point to a sequence of <code>Cell</code> instances based on its location in its parent cell.
    * The parent cell is the cell whose envelope wholly contains this point.
    * If the point is within <code>eps</code> distance of the edge of the cell, the neighboring cell is added to the aforementioned sequence.
    *
    * @param cellSize the parent cell size
    * @param eps      the neighborhood distance.
    * @return the parent cell and all the neighboring cells if the point is close to the edge.
    */
  def toCells(cellSize: Double, eps: Double) = {
    val xfac = (coords(0) / cellSize).floor
    val yfac = (coords(1) / cellSize).floor
    val cx = xfac * cellSize
    val cy = yfac * cellSize
    val xmin = cx + eps
    val ymin = cy + eps
    val xmax = cx + cellSize - eps
    val ymax = cy + cellSize - eps
    val row = yfac.toInt
    val col = xfac.toInt
    val cellArr = new ArrayBuffer[Cell](4)
    cellArr += Cell(row, col)
    if (coords(0) < xmin) {
      cellArr += Cell(row, col - 1)
      if (coords(1) < ymin) {
        cellArr += Cell(row - 1, col - 1)
        cellArr += Cell(row - 1, col)
      } else if (coords(1) > ymax) {
        cellArr += Cell(row + 1, col - 1)
        cellArr += Cell(row + 1, col)
      }
    } else if (coords(0) > xmax) {
      cellArr += Cell(row, col + 1)
      if (coords(1) < ymin) {
        cellArr += Cell(row - 1, col + 1)
        cellArr += Cell(row - 1, col)
      } else if (coords(1) > ymax) {
        cellArr += Cell(row + 1, col + 1)
        cellArr += Cell(row + 1, col)
      }
    } else if (coords(1) < ymin) {
      cellArr += Cell(row - 1, col)
    } else if (coords(1) > ymax) {
      cellArr += Cell(row + 1, col)
    }
    cellArr
  }

  /**
    * @return text representation of this instance.
    */
  override def toString = s"Point($id,$coords)"

}

/**
  * Companion object to build a point
  */
object Point extends Serializable {
  /**
    * Instantiate a point given an id, x and y value.
    *
    * @param id the point identifier
    * @param x  the horizontal placement
    * @param y  the vertical placement
    * @return a new <code>Point</code> instance.
    */
  def apply(id: Long, coords: List[Double]) = {
    new Point(id, coords)
  }
}
