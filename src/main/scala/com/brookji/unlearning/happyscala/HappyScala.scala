package com.brookji.unlearning.happyscala

/**
  * Created by brook on 15-11-30.
  *
  * Brook Ji
  *
  * 申请加入 QQ Group "无水Scala"
  *
  */
class HappyScala {

  /**
    * checksum for http header
    * see https://en.wikipedia.org/wiki/IPv4_header_checksum
    * @param header
    * @return
    */
  def checkSum(header : String) : Option[String] = {
    val checkIndex = 5 // checksum index in header
    //toLowerCase and splits
    val headerArray = header.toLowerCase.split(" ")
    if (!checkHeader(headerArray)){
      None
    }else{
      val sum1 = hexSum(headerArray)
      //println(sum1.mkString(""))
      val tmpArray = Array[String]("000" + sum1(0), sum1.takeRight(4).mkString(""))
      val sum2 = hexSum(tmpArray).takeRight(4)
      //println(sum2.mkString(""))
      val checkSumValue = sum2.map{
        ch =>
          toHex(toBinary(ch).map(a => if (a.equals('0')) '1' else '0' ))
      }.mkString("")
      headerArray(checkIndex) = checkSumValue
      //println(checkSumValue)
      Some(headerArray.mkString(" "))

    }
  }

  /**
    * 4位二进制转化为16进制
    * @param array
    * @return
    */
  private def toHex(array : Array[Char]) : Char = {
    val t1 = if (array(0).equals('1')) 8 else 0
    val t2 = if (array(1).equals('1')) 4 else 0
    val t3 = if (array(2).equals('1')) 2 else 0
    val t4 = if (array(3).equals('1')) 1 else 0
    val t = t1 + t2 + t3 + t4
    if (t <= 9)
      ('0' + t ).toChar
    else
      ('a' + (t - 10) ).toChar
  }

  /**
    * 16进制转化为2进制
    * @param ch
    * @return
    */
  private def toBinary(ch : Char) : Array[Char] = {
    val t : Int = if (ch <= '9') ch - '0' else ch - 'a' + 10
    val t1 = if ( (t & 8) > 0 ) '1' else '0'
    val t2 = if ( (t & 4) > 0 ) '1' else '0'
    val t3 = if ( (t & 2) > 0 ) '1' else '0'
    val t4 = if ( (t & 1) > 0 ) '1' else '0'

    Array[Char](t1,t2,t3,t4)
  }

  /**
    * check Header Format
    * @param array header String
    * @return
    */
  private def checkHeader(array : Array[String]) : Boolean ={
    if (array == null || array.size != 10)
      false
    else{
      array.foreach{
        str =>
          if (str.size != 4)
            return false
          val notHexadecimal =str.toCharArray.map(checkHexadecimal(_)).filter(_ == false).size
          if (notHexadecimal > 0 )
            return false
      }
      true
    }
  }

  /**
    * check Hexadecimal
    * @param ch
    * @return
    */
  private def checkHexadecimal(ch : Char) : Boolean = {
    val ascii = ch.toInt
    if ( ascii >= '0' && ascii <= '9' )
      true
    else if (ascii >= 'a' && ascii <= 'e')
      true
    else
      false
  }

  /**
    *
    * @param array
    * @return
    */
  private def hexSum( array : Array[String]) : Array[Char] = {
    val result = "00000".toCharArray
    var tmp = '0'
    array.foreach{
      str =>
        tmp = '0'
        (str.reverse + "0").toCharArray.zipWithIndex.foreach{
          case (ch, index) =>
            val (f1, s1) = hexSumTuple(ch, result(index))
            val (f2, s2) = hexSumTuple(tmp, s1)
            tmp = if (f1.equals('1') || f2.equals('1')) '1' else '0'
            result(index) = s2
        }
    }
    result.reverse
  }

  /**
    *
    * @param a
    * @param b
    * @return
    */
  private def hexSumTuple( a : Char, b : Char) : (Char, Char) = {
    val ta = if (a <= '9' ) a - '0' else a - 'a' + 10
    val tb = if (b <= '9' ) b - '0' else b - 'a' + 10
    var tmp = ta + tb
    val first : Char = if ( tmp > 15 ) {tmp = tmp - 16; '1'} else '0'
    val second : Char = if ( tmp < 10) (tmp + '0').toChar else ( (tmp - 10) + 'a').toChar
    (first, second)
  }

}

