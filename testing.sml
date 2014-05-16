structure TestingSctructure =
struct

  structure IntCompare : COMPARE =
  struct
    datatype order = LESS | EQUAL | GREATER

    type t = int

    fun compare (x, y) =
      if (x < y) then LESS
      else if (x = y) then EQUAL
      else GREATER
  end

  structure H = BinomialHeap(IntCompare)


  val testHeap1 = H.insert (H.insert (H.insert (H.insert(H.insert (H.createHeap) 5) 1) 8) 3) 8
  val testHeap1' = H.insert (H.insert (H.insert (H.insert (H.insert (H.createHeap) 8) 3) 1) 5) 8
  val testHeap2 = H.insert (H.insert (H.insert (H.insert (H.createHeap) 14) 7) 8) 2
  val testHeap2' = H.insert (H.insert (H.insert (H.insert (H.createHeap) 2) 8) 7) 14
  val mergedHeap = H.merge testHeap1 testHeap2

  (*
  testHeap 1 = testHeap1':

    8        1
           / |
          3  5
          |
          8

  testHeap 2 = testHeap2':

             2
           / |
          7  8
          |
          14

  mergedHeap:

    8        1
           / | \
          2  3  5
        / |  |
       7  8  8
       |
       14
  *)

  val true = H.isEmpty (H.createHeap)
  val false = H.isEmpty (mergedHeap)

  val 5 = H.size testHeap1
  val 4 = H.size testHeap2
  val 9 = H.size mergedHeap

  val 1 = H.findMin testHeap1
  val 2 = H.findMin testHeap2
  val 1 = H.findMin mergedHeap

  val testHeap1 = (H.insert (H.insert(H.insert (H.createHeap) 5) 1) 8)
  val testHeap1' = (H.insert (H.insert (H.insert (H.createHeap) 1) 5) 8)

  val testHeap1 = (H.insert (H.insert (H.insert(H.insert (H.createHeap) 5) 1) 8) 3)
  val testHeap1' = (H.insert (H.insert (H.insert (H.insert (H.createHeap) 3) 8) 1) 5)

  val true = H.heapEqual (testHeap1, testHeap1')
  val true = H.heapEqual (testHeap2, testHeap2')

end
