package com.ora

package object scalabeyondbasics {
   implicit val tuple2ToList: ((Int, String)) => List[String] = (t: (Int, String)) => List.fill(t._1)(t._2)
}
