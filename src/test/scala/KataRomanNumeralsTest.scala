import collection.mutable.Stack
import org.scalatest._

object Romans {
  def from( arabic : Int) : String = {
    var translation = Map(
        1 -> "I", 4   -> "IV",  5 -> "V",  9 -> "IX",  10 -> "X",
                  40  -> "XL", 50 -> "L", 90 -> "XC", 100 -> "C",
                  400 -> "CD",500 -> "D",900 -> "CM",1000 -> "M"
      )

      var roman = ""
      var keys = translation.keys.toArray.sortWith(_ > _)
      var remainder = arabic
      while(remainder > 0){
        var index = 0
        while( index < keys.length ){
          var current = keys(index)
          if (remainder == current) {
            remainder = 0
            roman +=  translation(current)
          }else if (remainder > current) {
            remainder = remainder - current
            roman += translation(current)
          }else{
            index += 1
          }
        }
      }

    return roman
  }
}

class StackSpec extends FlatSpec with ShouldMatchers {

  {
    var values = List(
      (1,"I"), (2,"II"), (3,"III"), (4,"IV"), (5,"V"), (6,"VI"), (7,"VII"), (8,"VIII"),
      (9,"IX"), (10,"X"), (11,"XI"), (12,"XII"), (13,"XIII"),
      (40,"XL"), (50,"L"), (51,"LI"), (62,"LXII"),
      (90,"XC"), (100,"C"), (111,"CXI"), (121,"CXXI"),
      (400,"CD"), (500,"D"), (555,"DLV"),
      (900,"CM"), (1000,"M"), (1111,"MCXI"),
      (1954,"MCMLIV"),
      (1990,"MCMXC"),
      (2008,"MMVIII"),
      (47,"XLVII")
      )
    values.foreach { case(a,r) => 
        it should ("convert %s to %s" format (a,r)) in {
          Romans.from(a) should equal (r)
        } 
      }

  }

   
}

