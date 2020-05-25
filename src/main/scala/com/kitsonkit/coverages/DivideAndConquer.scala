package com.kitsonkit.coverages

import scala.annotation.tailrec

/**
 * Case class contains coverage period
 *
 * @param eff  Start day
 * @param term End day
 */
case class Cov(eff: Int, term: Int)

/**
 * Object contains business logic
 */
object Cov {
  /**
   * Syntax sugar for easy call like "Cov(List(Cov(1,2)))" instead of chain Cov.longest( Cov.reducer( List(Cov(1,2),...) ) )
   *
   * @param coverages
   * @return
   */
  def apply(coverages: List[Cov]): Cov = longest(reducer(coverages))

  /**
   * Recursive tail recursion search pieces of coverages which we can merge in one bigger piece
   *
   * @param coverages List of coverages
   * @param maxSize   size of previous list, stop marker if no changes - means no optimization in list was on previous step
   * @return List of merged coverages
   */
  @tailrec
  final def reducer(coverages: List[Cov], maxSize: Int = Int.MaxValue): List[Cov] = {
    if (coverages.isEmpty) List(Cov(0,0))
    else {
      if (maxSize == coverages.size) coverages
      else {
        val first = coverages.head
        val (pair_me, as_is) = coverages.tail.span(canBeMerged(first) _)
        //put merged part as last element in updated list, for be sure that all elements can be first in reducer
        reducer(as_is ::: merge(first :: pair_me), coverages.size)
      }
    }
  }

  /**
   * Return longest coverage if it possible or empty coverage in other case
   *
   * @param coverages List of merged coverages
   * @return Cov(0,0) if no coverages or longest coverage from list
   */
  def longest(coverages: List[Cov]) = {
    //update rule of ordering Cov class by amount of covered days
    implicit val ordering = Ordering.by { coverage: Cov => coverage.term - coverage.eff }
    reducer(coverages).max
  }

  // intersect check _ABba_ || _BAab_
  def inside(one: Cov, another: Cov) = (one.eff <= another.eff && another.term <= one.term) || (another.eff <= one.eff && one.term <= another.term)

  //AaBb eq to Ab
  def near(one: Cov, another: Cov) = (one.term + 1 == another.eff) || (another.term + 1 == one.eff)

  //ABab
  def partial(one: Cov, another: Cov) = ((another.eff > one.eff && another.eff < one.term) || (another.term > one.eff && another.term < one.term)) ||
    ((one.eff > another.eff && one.eff < another.term) || (one.term > another.eff && one.term < another.term))

  /**
   * Check two Coverage object against intersection or zero gap distance
   * ()() form for esy use as one parameter function
   *
   * @param one     One coverage
   * @param another Another coverage
   * @return boolean value true if this coverages possible to merge
   */
  private def canBeMerged(one: Cov)(another: Cov) = inside(one, another) || near(one, another) || partial(one, another)

  /**
   * Merge all intersected / touched coverages in one piece
   *
   * @param coverages List of intersected / touched coverages
   * @return one Coverage object
   */
  private def merge(coverages: List[Cov]) = List(Cov(coverages.map(_.eff).min, coverages.map(_.term).max))
}

/**
 * Simpler but with restriction method
 * Works only for one year, 365 days
 * Pro - not need create merged objects
 * Cons - repeat fill array for each coverage in list
 */

object BruteForceSolution {
  val maxDays = 365;

  def longest(coverages: List[Cov]) = {
    if (coverages.isEmpty) {
      Cov(0, 0)
    } else {
      val days = Array.fill(maxDays + 1)(false)
      //fill for each coverages (one lot of cycles)
      coverages.foreach(coverage => (coverage.eff to coverage.term).foreach(days(_) = true))
      //try to find longest filled section
      var (eff, term, tempEff, tempTerm) = (0, 0, 0, 0)
      for (i <- 1 to maxDays) {
        if (days(i)) {
          if (tempEff == 0) tempEff = i
          tempTerm = i
        } else {
          if (tempTerm != 0 && term - eff <= tempTerm - tempEff) {
            eff = tempEff
            term = tempTerm
            tempEff = 0
            tempTerm = 0
          }
        }
      }
      //flush data before report
      if (tempTerm != 0 && term - eff <= tempTerm - tempEff) {
        eff = tempEff
        term = tempTerm
      }
      Cov(eff, term)
    }
  }
}

object DivideAndConquer {
  def main(args: Array[String]): Unit = {
    val coverages = List(
      Cov(1, 20),
      Cov(21, 30),
      Cov(15, 25),
      Cov(28, 40),
      Cov(50, 60),
      Cov(61, 200)
    )
    println(Cov(coverages))
    println(BruteForceSolution.longest(coverages))
  }
}
