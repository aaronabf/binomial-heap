functor BinomialHeap (C : COMPARE) : HEAP =
struct
  (* Helper Function *)
  (* power : (int * int) -> int *)
  fun power (m, 0) = 1
    | power (m, n) = (m * power(m, n - 1))

  (* Signature functions *)

  exception Empty

  datatype 'a bintree = Node of 'a * ('a bintree) list
  type 'a heap = ('a bintree * int) list

  (* createHeap : 'a heap *)
  val createHeap = []

  (* heapify : list -> 'a heap *)
  fun heapify h = raise Fail ""

  (* findMin : 'a heap -> 'a *)
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

  (* findMax : 'a heap -> 'a *)
  fun findMax h = raise Fail ""

  (* deleteMin : 'a heap -> 'a heap *)
  fun deleteMin h = raise Fail ""

  (* merge : 'a heap -> 'a heap -> 'a heap *)
  fun merge h1 [] = h1
    | merge [] h2 = h2
    | merge (x :: xs) (y :: ys) =
      let
        fun addTrees (t1 as (Node(e1, _), r1)) (t2 as (Node(e2, _), r2) = t1
      in
        (addTrees x y) :: (merge xs ys)
      end


      case C.compare(e1, e2) of
        C.LESS => e1
        | _ => e2

  (* increaseKey : 'a heap -> 'a heap *)
  fun increaseKey h = raise Fail ""

  (* decreaseKey : 'a heap -> 'a heap *)
  fun decreaseKey h = raise Fail ""

  (* insert : 'a heap -> 'a heap *)
  fun insert h x = raise Fail ""

  (* size : 'a heap -> int *)
  fun size [] = 0
    | size ((_, i) :: xs) = power(2, i) + (size xs)

  (* isEmpty : 'a heap -> bool *)
  fun isEmpty [] = true
    | isEmpty _ = false

end
