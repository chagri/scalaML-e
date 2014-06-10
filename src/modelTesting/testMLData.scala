package modelTesting

import scala.io.Source._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.Vector

object testMLData {
  
  def mean(xs: List[Int]): Double = xs match {
  case Nil => 0.0
  case ys => ys.reduceLeft(_ + _) / ys.size.toDouble
  }
 
  def stddev(xs: List[Int], avg: Double): Double = xs match {
	  case Nil => 0.0
	  case ys => math.sqrt((0.0 /: ys) {
	   (a,e) => a + math.pow(e - avg, 2.0)
	  } / xs.size)
  }
  
  def corr(a:Vector[Double] , b:Vector[Double]): Double = {

    val n = a.size
    var amean = 0.0
    var bmean = 0.0
    var avar = 0.0
    var bvar = 0.0
    
    for (i <- 0 to n-1) {
      amean += a(i)
      bmean += b(i)
    }
    
    amean = amean / n
    bmean = bmean / n
    
    for (i <- 0 to n-1){
      avar += math.pow((a(i) - amean),2)
      bvar += math.pow((b(i)- bmean), 2)
    }
    
    avar = avar/n
    bvar = bvar/n
    
    val astdev = math.sqrt(avar)
    val bstdev = math.sqrt(bvar)
    var Sum = 0.0   
    
    for (i <- 0 to n-1){
      Sum += (a(i) - amean)*(b(i) - bmean)/(astdev*bstdev)
    }
    
    (1.0 * Sum) / (n-1) 
    
  }
   
  def main(Args: Array[String]){
    
   var data = scala.io.Source.fromFile("mlData/bbowa_out", "utf-8")
   var lines = data.getLines
   var x = new ArrayBuffer[Double]
   var y = new ArrayBuffer[Double]
   
   for (line <- lines){
     var l = line.split('|').mkString
     x.append(l(0).toDouble)
     y.append(l(3).toDouble)
   }
   
   var a = x.toVector
   var b = y.toVector
   
   print(a.size,b.size)
   val ans = corr(x.toVector,y.toVector)
   print(ans)
   data.close()

  }
 
  
  
}