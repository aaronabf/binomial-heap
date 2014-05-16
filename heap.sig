signature HEAP =
sig

  type t
  type 'a heap

  val createHeap : t heap
  (*val heapify : t list -> t heap*)
  val findMin : t heap -> t
  (*val deleteMin : t heap -> t heap*)
  val merge : t heap -> t heap -> t heap
  (*val decreaseKey : t heap -> t heap*)
  val insert : t heap -> t -> t heap
  val size : t heap -> int
  val isEmpty : t heap -> bool
  val heapEqual : t heap * t heap -> bool

end
