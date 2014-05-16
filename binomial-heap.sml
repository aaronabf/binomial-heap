functor BinomialHeap (C : COMPARE) : HEAP =
struct
  exception Empty

  (* Binomial tree defined as a recursive datatype:
      (value, list of children trees) *)
  datatype 'a bintree = Node of 'a * ('a bintree) list

  (* Heap data structure defined as a full binomial heap with
      each tree stored along with its the rank *)
  type 'a heap = ('a bintree * int) list


  (* createHeap : 'a heap *)
  val createHeap = []


  (* findMin : 'a heap -> 'a
     REQUIRES: h is a valid binomial heap
     ENSURES: findMin h returns the min element of
              h or raises Empty if h is empty
  *)
  fun findMin [] = raise Empty
    | findMin [(Node(e, _), _)] = e
    | findMin ((Node(e1, _), _) :: xs) =
      let
        val e2 = findMin xs
      in
        case C.compare(e1, e2) of
          C.LESS => e1
          | _ => e2
      end


  (* merge : 'a heap -> 'a heap -> 'a heap
     REQUIRES: h1 and h2 are valid binomial heaps
     ENSURES: merge h1 h2 returns a binomial heap containing
              the elements of h1 and h2
  *)
  fun merge h1 [] = h1
    | merge [] h2 = h2
    | merge ((t1 as (Node(e1, b1), r1)) :: l1)
            ((t2 as (Node(e2, b2), r2)) :: l2) =
        if (r1 < r2) then
          (t1 :: (merge l1 (t2 :: l2)))
        else if (r2 > r1) then
          (t2 :: (merge (t1 :: l1) l2))
        else
          (case C.compare(e1, e2) of
            C.LESS => ((Node(e1, b1), r1) :: (merge l1 l2))
            | _ => ((Node(e2, b2), r2) :: (merge l1 l2)))


  (* insert : 'a heap -> 'a heap
     REQUIRES: h is a valid binomial heap
     ENSURES: insert h x returns a binomial heap containing
              the elements of h with x added on
  *)
  fun insert h x = merge h [(Node(x, []), 0)]


  (* size : 'a heap -> int
     REQUIRES: h is a valid binomial heap
     ENSURES: size h returns the number of elements in the binomial heap
  *)
  fun size [] = 0
    | size ((_, i) :: xs) =
      let
        fun power (m, 0) = 1
          | power (m, n) = (m * power(m, n - 1))
      in
        power(2, i) + (size xs)
      end


  (* isEmpty : 'a heap -> bool
     REQUIRES: h is a valid binomial heap
     ENSURES: isEmpty h returns true if the heap is empty; false otherwise
  *)
  fun isEmpty [] = true
    | isEmpty _ = false

end
