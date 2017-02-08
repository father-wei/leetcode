package leetcode

import scala.collection.mutable.Map
import scala.Option
import scala.annotation.tailrec

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
  
  
  
 
  /*
   * Two Sum
   * Given nums = [2, 7, 11, 15], target = 9,
		 Because nums[0] + nums[1] = 2 + 7 = 9,
	   return [0, 1].
   * 
   */
  
   def twoSum(nums: Array[Int], target: Int) : (Int, Int) = {
     
     val map = Map[Int, Int]()
     
     def iterateArr(arr: Array[Int], index:Int): Option[(Int, Int)] = {
        val value = map getOrElseUpdate (nums(index), index)
        val find =  map get (target - nums(index))
        
        if(index == arr.length){
           None
        }
        
        if(find != None) {
           Some ((find.get, index))        
        }else {
          iterateArr(arr, index + 1 )
        }
     }
     
     iterateArr(nums, 0).get
    
   }
  
   
  
  /*
   * Add Two Numbers
   * Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
		Output: 7 -> 0 -> 8
   * */
  def addTwoNumbers( l1: List[Int], l2: List[Int]): List[Int] = {
     
    val l = l1.zip(l2)
      .map(t => t match {case (x, y) => x + y});
      
    l.foldLeft((List[Int](), 0))((acc, e) => e match {
      case e if ( e + acc._2  >= 10)  => ( (e + acc._2) % 10  :: acc._1, 1)
      case _                          => ( (e + acc._2) :: acc._1, 0)  
           
    })._1.reverse
      
  }
	

  //Longest Substring Without Repeating Characters	 
  def lengthOfLongestSubstring(s: String): Int = {
    val map = Map[Char, Int]();
    
    @tailrec
    def iterate(current: Int,  max: Int): Int = current match {
      case e if e == s.length() => max
      case _ => {
        val value = map get s(current)         
        map += (s(current) -> (current + 1))
        
        value match {
          case Some(e) => iterate(current + 1, 
                                    math.max((current - e + 1), max))
          case None    => iterate(current + 1, max + 1)
        }        
      }       
    }
    
    iterate(0, Integer.MIN_VALUE);
  }
  
	
	
	
  println (lengthOfLongestSubstring("abcabcbb"))
  println (lengthOfLongestSubstring("bbbbb"))
  println (lengthOfLongestSubstring("pwwkewaz"))
	
  
  //Median of Two Sorted Arrays
  def findMedianSortedArrays(a : Array[Int], b: Array[Int]) : Int = (a.length, b.length) match{
    case (sizeA, sizeB) if (sizeA + sizeB) % 2 == 0 => {
       println("here")
       val k1 = findKElement(a, 0, sizeA, b, 0, sizeB, (sizeA + sizeB)/2)
      
       val k2 = findKElement(a, 0, sizeA, b, 0, sizeB, (sizeA + sizeB)/2 + 1)
      println("here", k1, k2)
       (k1 + k2) / 2
    }
    case (sizeA, sizeB) if (sizeA + sizeB) % 2 != 0 => {
       val k1 = findKElement(a, 0, sizeA, b, 0, sizeB, (sizeA + sizeB + 1)/2)
       k1
    }
    
  }
  
  @tailrec
  def findKElement(arr1: Array[Int], startA: Int, endA: Int, arr2:Array[Int], startB: Int, endB: Int, k: Int) : Int =
    (endA - startA, endB - startB, k) match 
    {
      case (0, _, _)  =>  arr2(startB + k - 1)
      case (_, 0, _)  =>  arr1(startA + k - 1)
      case (_, _, 1)  => if (arr1(startA) < arr2(startB)) arr1(startA) else arr2(startB)   
   
      case (n, m, k) => {
          val midA = (startA + endA) /2
          val midB = (startB + endB) /2 
          
          if (arr1(midA) <= arr2(midB)) {
            if (n/2 + m/2 + 1 >= k) {
              findKElement(arr1, startA, endA, arr2, startB, midB, k)
            } else {
              findKElement(arr1, midA + 1, endA, arr2, startB, endB, k - n / 2 - 1)
            }
          }
          else {
            if (n/2 + m/2 + 1 >= k) {
              findKElement(arr1, startA, midA, arr2, startB, endB, k)
            } else {
              findKElement(arr1, startA, endA, arr2, midB + 1, endB, k - m / 2 - 1)
            } 
          }
      }
      
  }
  
  
  
  println(findMedianSortedArrays(Array(1,3,4), Array(2,5,6)))
  
  
  //Tests
  println(twoSum(Array(2,11,7,15), 17))
  println( addTwoNumbers(List(2,4,3), List(5,6,4)) )  
  println(pow(2,10))
  println(groupAnagrams(List("eat", "tea", "tan", "ate", "nat", "bat")));
  
}
