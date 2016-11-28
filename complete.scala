import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._
import scala.util.Random
object test
{
	var database = ArrayBuffer[ArrayBuffer[Set[Int]]]()
	var threshold = 1
	var method = 1
	var mingap = 1
	var maxspan = 100
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

	def isSubSeqMinGap(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean = 
	{


		var YesOrNo = true
		var len = secondList.length
		var j = 0
		var f1 = 0
		var flag=0
		for( itemSet <- firstList) 
		{
			flag=0
			
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
		
			j = f1 + mingap

			if(flag == 0)
			{
				return false
			}

		}
		

		return YesOrNo
	}

	def isSubseqmaxspan(firstList: ArrayBuffer[Set[Int]] , secondList: ArrayBuffer[Set[Int]]) : Boolean = 
	{
		var YesOrNo = false
		var len = secondList.length
		var j = 0
		
		while(j < len)
		{
					
			if(firstList(0).subsetOf(secondList(j)))
			{
				var secondList_sp = secondList.slice(j,j+maxspan)
				
				if(isSubSeq(firstList,secondList_sp))
				{
					return true
				}
			}

			j += 1 

		}

		
		return YesOrNo
	}

	//calculates support for a given sequence in the given database. 
	def supportCount(sequence: ArrayBuffer[Set[Int]]): Int = 
	{
		var support=0
		for( seq <- database) 
		{
			if(method == 1){
				if(isSubSeq(sequence,seq)){
					support += 1
				}
			}
			else if(method == 2){
				
				if(isSubSeqMinGap(sequence,seq)){
					support += 1
				}

			}
			else{
				
				if(isSubseqmaxspan(sequence,seq)){

					support += 1
				}
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
		//Int siz = candidate.size
		var k = 0
		for(i <- 0 to candidate.size-1)
		{
			for(item <- candidate(i))
			{
				//println(candidate(i).size)
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
		
		var num_lists = 5
		var db = new ArrayBuffer[ArrayBuffer[Set[Int]]]()

		val r = new scala.util.Random(10)

		for(index <- 1 to num_lists)
		{
			var sequence = get_me_seq(r, all_sets)
			db += sequence
		}

		println("Database : ")
		println(db)
		println("#################################")
	
		return db
	}


	/*def init() = {
		if(args.length < 3)
			println("Insufficient Arguments. Setting Default values")
		else {
			max_events = args(0)
			mingap = args(1)
			maxspan= args(2)




		}
		
	}*/
	def main(args: Array[String]): Unit = 
    {




		database = create_database()

		

	if(args.length < 3)
			println("Insufficient Arguments. Setting Default values")
		else {
			max_events = args(0).toInt
			mingap = args(1).toInt
			maxspan= args(2).toInt




		}

		var frequent_all = new ArrayBuffer[ArrayBuffer[ArrayBuffer[Set[Int]]]]()

		var alpha = ArrayBuffer(1,2,3,4,5,6)
	
println("Number of levels "+args(0))
		//for method 1
		//init()
		println("Without any constraints")
		var size1_frequent = base1level(alpha)
		var size2_frequent = base2level(alpha)

		frequent_all += size1_frequent
		frequent_all += size2_frequent

		var sizek_frequent = ArrayBuffer[ArrayBuffer[Set[Int]]]()

	
		 println(size1_frequent.size)
		 println("################")
		 println(size2_frequent.size)
		 println("################")

		
		for( k <- 3 to max_events) {
			sizek_frequent = candgenandsupport( frequent_all(frequent_all.size - 1) )
			frequent_all += sizek_frequent
			
		
			 println(sizek_frequent.size)
			 println("################")
			
		}

		
		
		method = 2
		//mingap = 1
		println("MinGap "+args(1))
		size1_frequent = base1level(alpha)
		size2_frequent = base2level(alpha)

		frequent_all += size1_frequent
		frequent_all += size2_frequent

		sizek_frequent = ArrayBuffer[ArrayBuffer[Set[Int]]]()

		
		 println(size1_frequent.size)

		 println("################")
	
		 println(size2_frequent.size)

		 println("################")

		for( k <- 3 to max_events) {
			sizek_frequent = candgenandsupport( frequent_all(frequent_all.size - 1) )
			frequent_all += sizek_frequent
			
		
			 println(sizek_frequent.size)
			 println("################")
			
		}
		

		
		method = 3
		//maxspan = 100
		println("MaxSpan "+args(2))

		size1_frequent = base1level(alpha)
		size2_frequent = base2level(alpha)
		
		frequent_all += size1_frequent
		frequent_all += size2_frequent

		sizek_frequent = ArrayBuffer[ArrayBuffer[Set[Int]]]()

		
		 println(size1_frequent.size)

		 println("################")
		
		 println(size2_frequent.size)

		 println("################")

		
		for( k <- 3 to max_events) {
			sizek_frequent = candgenandsupport( frequent_all(frequent_all.size - 1) )
			frequent_all += sizek_frequent
			
			
			 println(sizek_frequent.size)
			 println("################")
			
		}	
		
    }

}
