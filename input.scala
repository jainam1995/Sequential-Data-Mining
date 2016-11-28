import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._
import scala.util.Random

import scala.io.Source

object test
{
	var database = ArrayBuffer[ArrayBuffer[Set[Int]]]()
	var threshold = 1
	var maxgap = 100
	var max_events = 3
	def isSubSeq(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean = 
	{
		var YesOrNo = true
		var len = secondList.length
		var j = 0
		var f1 = -1
		var flag=0
		for( itemSet <- firstList) 
		{
			flag=0
			

			j = f1 + 1
			while(j < len)
			{
				if(itemSet.subsetOf(secondList(j)))
				{
					
					if(flag==0)
					{
						f1 = j
						flag=1

					}
				}

				j += 1 
			}
		
			

			if(flag == 0)
			{
				return false
			}

		}
		

		return YesOrNo
	}


	
	def isSubSeqmaxgaputil(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean =
	{
		if(firstList.size == 0)
		{
			return true
		}

		var firstEle = firstList(0)

		var length = secondList.size

		var min = length

		if(maxgap < length)
		{
			min = maxgap
		}

		for(ind <- 0 to min-1)
		{
			if(firstEle.subsetOf(secondList(ind)))
			{
				if(isSubSeqmaxgaputil(firstList.slice(1,firstList.size),secondList.slice(ind+1, secondList.size)) )
				{
					return true
				}
			}
		}

		return false
	}

	def isSubSeqmaxgap(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean = 
	{
		var firstEle = firstList(0)
		var secIndex = -1
		for(itemSet <- secondList)
		{	
			secIndex += 1
			if(firstEle.subsetOf(itemSet) )
			{
				//print(itemSet)
				if(isSubSeqmaxgaputil(firstList.slice(1,firstList.size),secondList.slice(secIndex+1, secondList.size)) )
				{
					return true
				}
			}

		}	

		return false
	}

	


	//calculates support for a given sequence in the given database. 
	def supportCount(sequence: ArrayBuffer[Set[Int]]): Int = 
	{
		
		var support=0
		for( seq <- database) 
		{
			if(isSubSeqmaxgap(sequence,seq)){
				support += 1
			}
		}
		
		
		return support
	}

	def samek2ornot(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean = 
	{
		if(firstList==secondList)
		{
			return true
		}
			return false
	}



	def candidatepruneutil( candidate_subset:ArrayBuffer[Set[Int]], k1frequent:ArrayBuffer[ArrayBuffer[Set[Int]]]) : Boolean = 
	{
		for(element <- k1frequent)
		{
			if(samek2ornot(element,candidate_subset))
			{
				return true
			}
		}
		return false
	}

	def candidateprune( candidate:ArrayBuffer[Set[Int]], k1frequent:ArrayBuffer[ArrayBuffer[Set[Int]]]) : Boolean = 
	{
		
		var k = 0
		for(i <- 0 to candidate.size-1)
		{
			for(item <- candidate(i))
			{
				//println(candidate(i).size)
				if( candidate(i).size > 1)
				{
					candidate(i)-=item
					if(candidate(i).size == 0)
					{
						k = 1
						candidate.remove(i)
					}
					if( !candidatepruneutil(candidate,k1frequent))
					{
						return false
					}
					if( k==1)
					{
						var temp = Set(item)
						for(k <- i to candidate.size-1)
						{
							var temp2 = temp
							temp = candidate(i)
							candidate(i) = temp2
						}
						candidate+=temp
						k=0
						
					}
					else
					{
						candidate(i)+=item
					}
				}
				else
				{
					if( i == 0 || i == candidate.size -1)
					{
						candidate(i)-=item
						if(candidate(i).size == 0)
						{
							k = 1
							candidate.remove(i)
						}
						if( !candidatepruneutil(candidate,k1frequent))
						{
							return false
						}
						if( k==1)
						{
							var temp = Set(item)
							for(k <- i to candidate.size-1)
							{
								var temp2 = temp
								temp = candidate(i)
								candidate(i) = temp2
							}
							candidate+=temp
							k=0
						}
						
					}
					else
					{
						candidate(i)+=item
					}

				} 
			}
		}
		return true
	}

	//check support count before adding
	def base1level( alphabets:ArrayBuffer[Int] ) : ArrayBuffer[ArrayBuffer[Set[Int]]]=
	{
		var listoflist11 = new ArrayBuffer[ArrayBuffer[Set[Int]]]()
		for( alphabet  <- alphabets)
		{
			var set1: Set[Int] = Set()
			set1+= alphabet
			var List11= ArrayBuffer[Set[Int]]()
			List11+=set1

			if(supportCount(List11) >= threshold)
			{
				listoflist11 += List11
			}
		}
		
		return listoflist11	

	}

	//will do more support counting if some label is not present
	// will create a list of ints from base1level and then take intersection
	def base2level( alphabets:ArrayBuffer[Int] ) : ArrayBuffer[ArrayBuffer[Set[Int]]]=
	{
		

		var listoflist11 = new ArrayBuffer[ArrayBuffer[Set[Int]]]()
		for( alphabet  <- alphabets)
		{
			for(alphabet2 <- alphabets)
			{
				if(alphabet < alphabet2)
				{
					var set1: Set[Int] = Set()
					set1+= alphabet
					set1+= alphabet2
					var List11= ArrayBuffer[Set[Int]]()
					List11+=set1
					if(supportCount(List11) >= threshold)
					{
						listoflist11 += List11
					}
				}
			}
		}
		for( alphabet  <- alphabets)
		{
			for(alphabet2 <- alphabets)
			{
				if(alphabet != alphabet2)
				{
					var set1: Set[Int] = Set()
					var set2: Set[Int] = Set()
					set1+= alphabet
					set2+= alphabet2
					var List11= ArrayBuffer[Set[Int]]()
					List11+=set1
					List11+=set2
					if(supportCount(List11) >= threshold)
					{
						listoflist11 += List11
					}
				}
				
			}
		}
		for( alphabet  <- alphabets)
		{
			for(alphabet2 <- alphabets)
			{
				if(alphabet == alphabet2)
				{
					var set1: Set[Int] = Set()
					var set2: Set[Int] = Set()
					set1+= alphabet
					set2+= alphabet2
					var List11= ArrayBuffer[Set[Int]]()
					List11+=set1
					List11+=set2
					if(supportCount(List11) >= threshold)
					{
						listoflist11 += List11
					}
				}
				
			}
		}
		
		return listoflist11	
	}

	def candgenutil( first:ArrayBuffer[Set[Int]], second:ArrayBuffer[Set[Int]] ):ArrayBuffer[Set[Int]] = 
	{	

		var tempfirst: Set[Int] = first(0)
		var templast: Set[Int] = second(second.length-1)
		


		var answer = new ArrayBuffer[Set[Int]]()
		
		var first_buff= new ArrayBuffer[Set[Int]]()
		first_buff = first.clone
		first_buff.remove(0)
		
		

		var second_buff= new ArrayBuffer[Set[Int]]()
		second_buff = second.clone
		second_buff.remove(second_buff.size-1)

		
	
		for( tempfirsts <- tempfirst) 
		{

			tempfirst-=tempfirsts
			if(tempfirst.size != 0)
			{
				first_buff = tempfirst +: first_buff

			}
	

			for(  templasts<- templast) 
			{
				templast-=templasts
				
				
				if(templast.size != 0)
				{
					second_buff = second_buff :+ templast
				}
		
				if(first_buff==second_buff)
				{
		
					if(templast.size==0)
					{

						var temp_buff= new ArrayBuffer[Set[Int]]()
						temp_buff = first.clone
						temp_buff += Set(templasts)
						//println(temp_buff)
						answer = temp_buff	
					}
					else
					{
						var temp_buff= new ArrayBuffer[Set[Int]]()
						//print("heya")	
						temp_buff = first.clone
						temp_buff(temp_buff.size-1)+=templasts
						//println(temp_buff)
						answer=temp_buff
					}	
		

				}
				

				if(templast.size != 0)
					second_buff.remove(second_buff.size-1)

				templast+=templasts
			}
			if(tempfirst.size != 0)
				first_buff.remove(0)
			
			tempfirst+=tempfirsts
		}
		return answer
	}

	def candgenandsupport(k1frequent:ArrayBuffer[ArrayBuffer[Set[Int]]]):ArrayBuffer[ArrayBuffer[Set[Int]]]=
	{
		var kcndlist = new ArrayBuffer[ArrayBuffer[Set[Int]]]() 
		for(i <- 0 to k1frequent.size-1)
		{
			

			for(j <- 0 to k1frequent.size -1)
			{
				

				if( i != j)
				{
					var temp = candgenutil(k1frequent(i), k1frequent(j))
					
					if(temp.size > 0)
					{
						if(candidateprune(temp,k1frequent))
						{
						
							if(supportCount(temp) >= threshold)
							{
								if(!kcndlist.contains(temp))
								{
									kcndlist+=temp
								}
							}
						}
					}
				}

			}
		}
		return kcndlist
	}

	 def generate_all_sets12(): ArrayBuffer[Set[Int]]=
	{
		var all_sets= new ArrayBuffer[Set[Int]]();
       	Set(1,2,3,4,5,6,7,8,9,10,11,12).subsets foreach (all_sets+=_)
		all_sets.remove(0)

		return all_sets
	}
	
	def get_me_seq(r:scala.util.Random, all_sets:ArrayBuffer[Set[Int]]): ArrayBuffer[Set[Int]]=
	{

		var size = r.nextInt(100)
		var randoms = Seq.fill(size)(Random.nextInt(100))
		var sequence = new ArrayBuffer[Set[Int]]()

		for(i <- randoms)
		{
			sequence += all_sets(i)
		}

		return sequence
	}

	def create_database(): ArrayBuffer[ArrayBuffer[Set[Int]]]=
	{
		var all_sets = generate_all_sets12()
		//println("all sets done")
		var num_lists = 1
		var db = new ArrayBuffer[ArrayBuffer[Set[Int]]]()

		val r = new scala.util.Random(10)

		for(index <- 1 to num_lists)
		{
			var sequence = get_me_seq(r, all_sets)
			db += sequence
		}
		db= ArrayBuffer(ArrayBuffer(Set(5, 2), Set(5, 6, 2), Set(5, 1, 4), Set(5, 7), Set(9, 8), Set(12, 8), Set(1, 3), Set(5, 8), Set(6, 4), Set(9, 3), Set(5, 1, 11), Set(11, 4)))
		return db
	}
/*def readDataFromFile(fileName: String): Seq[Set[String]] = {
    val input = Source.fromFile(fileName)
    val inputScanner = new Scanner(input.bufferedReader())
    val result = new ArrayBuffer(ArrayBuffer(Set()))
    val reline=new ArrayBuffer(Set())
    while (inputScanner.hasNext) {
      val line = inputScanner.nextLine()

      val lineScanner = new Scanner(line)

      val transactionData = new mutable.HashSet[String]()

      while (lineScanner.hasNext) {
        transactionData += lineScanner.next("\\w+")
      }
      result += transactionData.toSet
    }
    result
  }
*/

	def create_database2(): ArrayBuffer[ArrayBuffer[Set[Int]]] = 
	{

	var db = new ArrayBuffer[ArrayBuffer[Set[Int]]]()
var newtrans =  ArrayBuffer[Set[Int]]()


    	var transaction:String = ""
    	for(line <- Source.fromFile("in.txt").getLines()) { 
    		//transaction=""
        	transaction = line.trim()
        	if(transaction=="")
        	{//newtrans+=set2;
        		//println(newtrans)
        		var seq = new ArrayBuffer[Set[Int]]()
        		 seq = newtrans.clone
        		db += seq

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
//println(newtrans)
var seq = new ArrayBuffer[Set[Int]]()
        		 seq = newtrans.clone
        		db += seq

        		newtrans.clear
println(db)


return db	


	}


	def main(args: Array[String]): Unit = 
    {var db = new ArrayBuffer[ArrayBuffer[Set[Int]]]()
var newtrans =  ArrayBuffer[Set[Int]]()


    	var transaction:String = ""
    	for(line <- Source.fromFile("in.txt").getLines()) { 
    		//transaction=""
        	transaction = line.trim()
        	if(transaction=="")
        	{//newtrans+=set2;
        		//println(newtrans)
        		var seq = new ArrayBuffer[Set[Int]]()
        		 seq = newtrans.clone
        		db += seq

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
//println(newtrans)
var seq = new ArrayBuffer[Set[Int]]()
        		 seq = newtrans.clone
        		db += seq

        		newtrans.clear
println(db)




    }

}
