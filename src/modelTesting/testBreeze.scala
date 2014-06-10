package modelTesting

import scala.io.Source._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import breeze.linalg._
import scala.Vector
import breeze.linalg.DenseVector

object testBreeze {

  def corr(a:Vector[Double] , b:Vector[Double]): Double = {
    
    val n = a.size
    val(amean, avar) = meanAndVariance(a)
    val(bmean,bvar) = meanAndVariance(b)
    val astddev = math.sqrt(avar)
    val bstddev = math.sqrt(bvar)
    
    var Sum = 0.0
    
    for (i <- 0 to n-1){
      Sum += (a(i) - amean)* (b(i) - bmean)
    }
    
    (1.0 * Sum) / ((n - 1.0)* astddev*bstddev) 
    
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
