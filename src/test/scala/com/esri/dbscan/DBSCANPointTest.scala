package com.esri.dbscan

import org.scalatest.{FlatSpec, Matchers}

/**
  */
class DBSCANPointTest extends FlatSpec with Matchers {

  // 2 dimensions
  "Dist2D" should " be 1.36" in {
   
    val point1 = DBSCANPoint(0,List(1.2,3.4))
    val point2 = DBSCANPoint(1,List(2.0,4.5))
    var dist = point1.distance(point2)
    dist = "%.2f".format(dist).replace(",",".").toDouble

    dist shouldBe 1.36
  }

  // 4 dimensions
  "Dist4D" should " be 3.04" in {
   
    val point1 = DBSCANPoint(0,List(1.2,3.4,5.6,8.3))
    val point2 = DBSCANPoint(1,List(2.0,4.5,7.8,9.9))
    var dist = point1.distance(point2)
    dist = "%.2f".format(dist).replace(",",".").toDouble

    dist shouldBe 3.04
  }

  // 6 dimensions
  "Dist6D" should " be 4.57" in {
   
    val point1 = DBSCANPoint(0,List(1.2,3.4,5.6,8.3,1.0,2.0))
    val point2 = DBSCANPoint(1,List(2.0,4.5,7.8,9.9,3.2,4.6))
    var dist = point1.distance(point2)
    dist = "%.2f".format(dist).replace(",",".").toDouble

    dist shouldBe 4.57
  }

}
