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
  
  
  /*
   * Rotate Image
   * You are given an n x n 2D matrix representing an image.
		Rotate the image by 90 degrees (clockwise).
   * */
  
  
  
 
  
  
  
  //Tests
  println(pow(2,10))
  println(groupAnagrams(List("eat", "tea", "tan", "ate", "nat", "bat")));
  
}
