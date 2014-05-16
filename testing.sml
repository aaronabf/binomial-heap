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

  structure heap = BinomialHeap(IntCompare)
end
