import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._
import scala.util.Random

import scala.io.Source

object test
{
def main(args: Array[String]): Unit = 
    {var db = new ArrayBuffer[ArrayBuffer[Set[Int]]]()
var newtrans =  new ArrayBuffer[Set[Int]]()


    	var transaction:String = ""
    	for(line <- Source.fromFile("in.txt").getLines()) { 
    		//transaction=""
        	transaction = line.trim()
        	if(transaction=="")
        	{//newtrans+=set2;
        		println(newtrans)
        		db += newtrans
        		newtrans.clear

        		}
        		else{
        	var set2: Set[Int] = Set()
        	for(item <- transaction.split(",")){
        		set2 += item.toInt 
        	}
        	newtrans+= set2;
        	//set2.clear
        	//println(set2)
      	}
      	//println(newtrans)
      }
println(newtrans)
//println(database)




    }
}