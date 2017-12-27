package com.esri.dbscan

import org.scalatest._

import scala.io.Source

class DBSCANTest extends FlatSpec with Matchers {

  // 2 dimensions
  "DBSCAN2D" should "cluster" in {
    val points = Array(
      DBSCANPoint(0, List(9, 9)),
      DBSCANPoint(1, List(11, 9))
    )

    val clusters = DBSCAN(3, 2).cluster(points).toList

    clusters.length shouldBe 1
  }

  "DBSCAN2D" should "find one cluster" in {
    val points = Array(
      DBSCANPoint(0, List(0, 0)),
      DBSCANPoint(1, List(0, 2)),
      DBSCANPoint(2, List(0, 4)),
      DBSCANPoint(3, List(0, 6)),
      DBSCANPoint(4, List(0, 8)),
      DBSCANPoint(5, List(3, 0))
    )
    val clusters = DBSCAN(2.5, 2).cluster(points).toList

    clusters.length shouldBe 1
    clusters(0) should contain only(points(0), points(1), points(2), points(3), points(4))
  }

  /**
    * http://people.cs.nctu.edu.tw/~rsliang/dbscan/testdatagen.html
    */
  "DBSCAN2D" should "have 6 clusters and 20 outliers" in {

    val points = Source.fromURL(getClass.getResource("/dat_4_6_6_20.txt")).getLines().map(line => {
      val splits = line.split(' ')
      DBSCANPoint(splits(0).toInt, List(splits(1).toDouble, splits(2).toDouble))
    }).toArray

    val results = Source.fromURL(getClass.getResource("/res_4_6_6_20.txt")).getLines().map(line => {
      val splits = line.split(',')
      splits.tail.map(_.toInt)
    }).toArray

    val clusters = DBSCAN(4, 6).cluster(points).toList

    clusters.length shouldBe 6

    clusters.zipWithIndex.foreach {
      case (cluster, index) => {
        val result = results(index)
        cluster.foreach(point => {
          result should contain(point.id)
        })
        val ids = cluster.map(_.id)
        result.foreach(id => {
          ids should contain(id)
        })
      }
    }
  }

  "DBSCAN2D" should "have 20 clusters and 20 outliers" in {

    val points = Source.fromURL(getClass.getResource("/dat_4_10_20_20.txt")).getLines().map(line => {
      val splits = line.split(' ')
      DBSCANPoint(splits(0).toInt, List(splits(1).toDouble, splits(2).toDouble))
    }).toArray

    val results = Source.fromURL(getClass.getResource("/res_4_10_20_20.txt")).getLines().map(line => {
      val splits = line.split(',')
      splits.tail.map(_.toInt)
    }).toArray

    val clusters = DBSCAN(4, 10).cluster(points).toList

    clusters.length shouldBe 20

    clusters.zipWithIndex.foreach {
      case (cluster, index) => {
        val result = results(index)
        cluster.foreach(point => {
          result should contain(point.id)
        })
        val ids = cluster.map(_.id)
        result.foreach(id => {
          ids should contain(id)
        })
      }
    }
  }

  // 3 dimensions
  "DBSCAN3D" should "find one cluster" in {
    val points = Array(
      DBSCANPoint(0, List(0, 0, 1)),
      DBSCANPoint(1, List(0, 2, 1)),
      DBSCANPoint(2, List(0, 4, 1)),
      DBSCANPoint(3, List(0, 6, 1)),
      DBSCANPoint(4, List(0, 8, 1)),
      DBSCANPoint(5, List(3, 0, 0))
    )
    val clusters = DBSCAN(2.5, 2).cluster(points).toList

    clusters.length shouldBe 1
    clusters(0) should contain only(points(0), points(1), points(2), points(3), points(4))
  }

  "DBSCAN3D" should "have 2 clusters" in {

    val points = Source.fromURL(getClass.getResource("/dat_circles.txt")).getLines().map(line => {
      val splits = line.split(' ')
      DBSCANPoint(splits(0).toInt, List(splits(1).toDouble, splits(2).toDouble, splits(3).toDouble))
    }).toArray

    val results = Source.fromURL(getClass.getResource("/res_circles.txt")).getLines().map(line => {
      val splits = line.split(',')
      splits.tail.map(_.toInt)
    }).toArray

    val clusters = DBSCAN(0.1,5).cluster(points).toList

    clusters.length shouldBe 2

    clusters.zipWithIndex.foreach {
      case (cluster, index) => {
        val result = results(index)
        cluster.foreach(point => {
          result should contain(point.id)
        })
        val ids = cluster.map(_.id)
        result.foreach(id => {
          ids should contain(id)
        })
      }
    }
  }

  // 4 dimensions
  "DBSCAN4D" should "find one cluster" in {
    val points = Array(
      DBSCANPoint(0, List(0, 0, 1, 0)),
      DBSCANPoint(1, List(0, 2, 1, 0)),
      DBSCANPoint(2, List(0, 4, 1, 0)),
      DBSCANPoint(3, List(0, 6, 1, 0)),
      DBSCANPoint(4, List(0, 8, 1, 0)),
      DBSCANPoint(5, List(3, 0, 0, 0))
    )
    val clusters = DBSCAN(2.5, 2).cluster(points).toList

    clusters.length shouldBe 1
    clusters(0) should contain only(points(0), points(1), points(2), points(3), points(4))
  }
}
