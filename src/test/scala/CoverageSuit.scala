import com.kitsonkit.coverages.{BruteForceSolution, Cov, DivideAndConquer}
import org.scalatest.FunSuite

class CoverageSuit extends FunSuite {

  val maxDays = 365

  test("ABba") {
    assert( Cov(List(Cov(2,5), Cov(3,4))) == Cov(2,5))
    assert( BruteForceSolution.longest(List(Cov(2,5), Cov(3,4))) == Cov(2,5))
  }

  test("AaBb") {
    assert( Cov(List(Cov(2,3), Cov(4,5))) == Cov(2,5))
    assert( BruteForceSolution.longest(List(Cov(2,3), Cov(4,5))) == Cov(2,5))
  }

  test("ABab") {
    assert( Cov(List(Cov(2,4), Cov(3,5))) == Cov(2,5))
    assert( BruteForceSolution.longest(List(Cov(2,4), Cov(3,5))) == Cov(2,5))
  }

  test("Only one elements must be return itself") {
    assert(Cov(List(Cov(10,10))) == Cov(10,10))
    assert(BruteForceSolution.longest(List(Cov(10,10))) == Cov(10,10))
    assert(BruteForceSolution.longest(List(Cov(1,maxDays))) == Cov(1,maxDays))
  }
  test("An empty List should return zero valued Coverage") {
    assert( Cov(List.empty[Cov]) == Cov(0,0) )
    assert( BruteForceSolution.longest(List.empty[Cov]) == Cov(0,0) )
  }

  test("Merge two  Cov(1, 20), Cov(21, 30) into Cov(1,30)") {
    assert(
      Cov(List(Cov(1, 20), Cov(21, 30))) == Cov(1,30)
    )
    assert(
      BruteForceSolution.longest(List(Cov(1, 20), Cov(21, 30))) == Cov(1,30)
    )
  }
  test("Merge Cov(1, 20),Cov(21, 30),Cov(15, 25),Cov(28, 40) into Cov(1,40)") {
    assert(
      Cov(List(Cov(1, 20),Cov(21, 30),Cov(15, 25),Cov(28, 40) )) == Cov(1,40)
    )
    assert(
      BruteForceSolution.longest(List(Cov(1, 20),Cov(21, 30),Cov(15, 25),Cov(28, 40) )) == Cov(1,40)
    )
  }

  test ("long long list" ) {
    val coverages = for {i <- 1 to maxDays; j <- i to maxDays} yield Cov(i,j)
    assert(Cov(coverages.toList) == Cov(1, maxDays))
    assert(BruteForceSolution.longest(coverages.toList) == Cov(1, maxDays))
  }

  test("As an input data") {
    val coverages = List(
      Cov(1, 20),
      Cov(21, 30),
      Cov(15, 25),
      Cov(28, 40),
      Cov(50, 60),
      Cov(61, 200)
    )
    assert(Cov(coverages) == Cov(50, 200))
    //assert(BruteForceSolution.longest(coverages) == Cov(50,200))
  }

}
