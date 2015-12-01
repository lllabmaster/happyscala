import com.brookji.unlearning.happyscala.HappyScala
import org.scalatest.FunSuite

/**
  * Created by brook on 15-12-1.
  */
class HappyScalaTest extends FunSuite{

  test("Test HappyScala.checkSum method"){
    val happyScala = new HappyScala()
    val str1 = "4500 0073 0000 4000 4011 0000 c0a8 0001 c0a8 00c7"
    val str2 = "4500 0073 0000 4000 4011 b861 c0a8 0001 c0a8 00c7"
    assert(happyScala.checkSum(str1).get.equals(str2))

    val str3 = "4500 0030 4422 4000 8006 0000 8c7c 19ac ae24 1e2b"
    val str4 = "4500 0030 4422 4000 8006 442e 8c7c 19ac ae24 1e2b"
    assert(happyScala.checkSum(str3).get.equals(str4))
  }



}
