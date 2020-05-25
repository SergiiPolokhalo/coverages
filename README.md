# coverages
Solution for coverages problem

Two variants:

*BruteForceSolution* with limitation (up to 64k-1 days) and one lot(O(n^2) in worst case) of write data in memory

*Cov* companion object without limitation by numbers, only by memory size. Worst case when we have one lot of coverages which have at least one day gap between each other

use "sbt test run" 
  