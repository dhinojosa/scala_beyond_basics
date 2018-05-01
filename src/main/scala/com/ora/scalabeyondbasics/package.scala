package com.ora

package object scalabeyondbasics {
   implicit val tuple2ToList: Tuple2[Int, String] => List[String]
           = t => List.fill(t._1)(t._2)
   //implicit val tuple2ToList: (Int, String) => List[String] =
   //      t => List.fill(t._1)(t._2)
}
