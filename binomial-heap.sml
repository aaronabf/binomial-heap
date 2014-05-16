functor BinomialHeap (C : COMPARE) : HEAP =
struct
  exception Empty

  (* Binomial tree defined as a recursive datatype:
      (value, list of children trees) *)
  datatype 'a bintree = Node of 'a * ('a bintree) list


  (* Heap data structure defined as a full binomial heap with
      each tree stored along with its the rank *)
  type t = C.t
  type 'a heap = (t bintree * int) list


  (* createHeap : t heap *)
  val createHeap = []


  (* findMin : t heap -> t
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


  (* merge : t heap -> t heap -> t heap
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
        else if (r2 < r1) then
          (t2 :: (merge (t1 :: l1) l2))
        else
          let
            val newTree =
              case C.compare(e1, e2) of
                C.LESS => (Node(e1, (Node(e2, b2) :: b1)), r1 + 1)
                | _ => (Node(e2, (Node(e1, b1) :: b2)), r2 + 1)
          in
             merge [newTree] (merge l1 l2)
          end


(*
fun insTree ([], T) = [T]
       | insTree (H as T'::H', T) =
       if rank T < rank T' then T::H
       else if rank T' < rank T then T' :: insTree (H', T)
       else insTree (H', link (T, T'))

fun insert (H, k, d) = insTree (H, Node(0,k,d,[]))

      | merge (H1 as T1::H1', H2 as T2::H2') =
         else insTree (merge (H1', H2'), link (T1, T2))*)



  (* insert : t heap -> t heap
     REQUIRES: h is a valid binomial heap
     ENSURES: insert h x returns a binomial heap containing
              the elements of h with x added on
  *)
  fun insert h x = merge [(Node(x, []), 0)] h


  (* size : t heap -> int
     REQUIRES: h is a valid binomial heap
     ENSURES: size h returns the number of elements in the binomial heap
  *)
  fun size [] = 0
    | size ((_, i) :: xs) =
      let
        fun power2 0 = 1
          | power2 n = 2 * power2(n - 1)
      in
        (power2 i) + (size xs)
      end


  (* isEmpty : t heap -> bool
     REQUIRES: h is a valid binomial heap
     ENSURES: isEmpty h returns true if the heap is empty; false otherwise
  *)
  fun isEmpty [] = true
    | isEmpty _ = false


  (* heapEqual : t heap * t heap -> bool
     REQUIRES: h1 and h2 are valid binomial heaps
     ENSURES: heapEqual h1 h2 returns true if heaps are equal; false otherwise
  *)
  fun heapEqual ([], []) = true
    | heapEqual ((t1, r1 : int) :: h1, (t2, r2 : int) :: h2) =
      let
        fun treeEqual ([], []) = true
          | treeEqual (Node(e1,b1) :: xs, Node(e2,b2) :: ys) =
             (case C.compare(e1, e2) of
                C.EQUAL => treeEqual (b1, b2) andalso treeEqual(xs,ys)
                | _ => false)
          | treeEqual (_,_) = false
      in
        (r1 = r2) andalso treeEqual ([t1], [t2]) andalso heapEqual (h1, h2)
      end
    | heapEqual (_,_) = false

end
