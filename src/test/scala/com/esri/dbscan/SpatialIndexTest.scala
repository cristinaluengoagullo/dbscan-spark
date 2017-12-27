package com.esri.dbscan

import org.scalatest.{FlatSpec, Matchers}

/**
  */
class SpatialIndexTest extends FlatSpec with Matchers {

  // 2 dimensions
  "Grid2D" should "find 2 points" in {

    val points = List(
      DBSCANPoint(0, List(4, 4)),
      DBSCANPoint(1, List(5, 5)),
      DBSCANPoint(2, List(6, 6)),
      DBSCANPoint(3, List(1, 1)),
      DBSCANPoint(4, List(9, 9))
    )
    val grid = points.foldLeft(SpatialIndex(2)) {
      _ + _
    }

    val result = grid.findNeighbors(DBSCANPoint(0, List(5.5, 5.5)))

    result.length shouldBe 2
    result should contain allOf(points(1), points(2))
  }

  "Grid2D" should "not find points" in {

    val points = List(
      DBSCANPoint(3, List(1, 1)),
      DBSCANPoint(4, List(9, 9))
    )
    val grid = points.foldLeft(SpatialIndex(2)) {
      _ + _
    }

    val result = grid.findNeighbors(DBSCANPoint(0, List(5.5, 5.5)))

    result shouldBe empty
  }

  // 3 dimensions
  "Grid3D" should "find 3 points" in {

    val points = List(
      DBSCANPoint(0, List(4, 4, 4)),
      DBSCANPoint(1, List(5, 5, 5)),
      DBSCANPoint(2, List(5, 5, 5)),
      DBSCANPoint(3, List(6, 6, 6)),
      DBSCANPoint(4, List(1, 1, 1)),
      DBSCANPoint(5, List(9, 9, 9))
    )
    val grid = points.foldLeft(SpatialIndex(2)) {
      _ + _
    }

    val result = grid.findNeighbors(DBSCANPoint(0, List(5.5, 5.5, 5.5)))

    result.length shouldBe 3
    result should contain allOf(points(1), points(2), points(3))
  }

  // 4 dimensions
  "Grid4D" should "find 4 points" in {

    val points = List(
      DBSCANPoint(0, List(4, 4, 4, 4)),
      DBSCANPoint(1, List(5, 5, 5, 5)),
      DBSCANPoint(2, List(1, 1, 1, 1)),
      DBSCANPoint(3, List(8, 8, 8, 8)),
      DBSCANPoint(4, List(9, 9, 9, 9)),
      DBSCANPoint(5, List(5.5, 5.5, 5.5, 5.5)),
      DBSCANPoint(6, List(6, 6, 6, 6)),
      DBSCANPoint(7, List(6.4, 6.4, 6.4, 6.4))
    )
    val grid = points.foldLeft(SpatialIndex(2)) {
      _ + _
    }

    val result = grid.findNeighbors(DBSCANPoint(0, List(5.5, 5.5, 5.5, 5.5)))

    result.length shouldBe 4
    result should contain allOf(points(1), points(5), points(6), points(7))
  }
}
