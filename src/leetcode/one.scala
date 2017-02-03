package leetcode

object one extends App{
  //implement pow(x, n).
  def pow(x: Int, n :Int) : Int = n match{
    case i if i <  0    => pow( 1/x, -n)
    case i if i == 0    => 1
    case i if i %2 == 0 => pow(x * x, n/2)
    case _ => x * pow(x * x, n/2)    
  }
  
  
  /*Given an array of strings, group anagrams together.
  	For example, given: ["eat", "tea", "tan", "ate", "nat", "bat"], 
  	Return:
  	[
      ["ate", "eat","tea"],
      ["nat","tan"],
      ["bat"]
    ]
  * */
   def groupAnagrams(strs: List[String]) : List[List[String]]=  {
     strs.groupBy(e => e.sortWith(_<_))          
         .values
         .toList      
  }
  
  
     def rotate(arr: Array[Array[Int]]) : Array[Array[Int]] = {
        swap(arr.reverse)     
     }
     
     def antiRotate(arr: Array[Array[Int]]) : Array[Array[Int]] = {
        swap(arr.map(x=> x.reverse))     
     }
   
    
     def swap(arr: Array[Array[Int]]) :  Array[Array[Int]] = {
       for( x <- 0 to arr.length-1;
            y <- x + 1 to arr(x).length-1) 
        {
            val temp = arr(x)(y)
            arr(x)(y)  = arr(y)(x)
            arr(y)(x) = temp
                    
        }
        arr  
     }
  /*
   * Rotate Image
   * You are given an n x n 2D matrix representing an image.
		Rotate the image by 90 degrees (clockwise).
   * */
  
  
  
  //Two Sum
  /*
   * Given nums = [2, 7, 11, 15], target = 9,
		 Because nums[0] + nums[1] = 2 + 7 = 9,
	   return [0, 1].
   * 
   */
  
   def twoSum(nums: Array[Int], target: Int) : (Int, Int) = {
     val map = Map[Int, (Int, Int)]()
     var result = (-1, -1)
      
     breakable { 
       for(index <- 0 to nums.length -1 ) {
          val value = map.getOrElseUpdate(nums(index), (0, index))
          val find =  map get (target - nums(index))
          if(find != None) {
             result = (find.get._2, index)
             break
          }
       }
     }
     result
   }
  
   
   println(twoSum(Array(2,11,7,15), 9))
  
  
  
  //Tests
  println(pow(2,10))
  println(groupAnagrams(List("eat", "tea", "tan", "ate", "nat", "bat")));
  
}
