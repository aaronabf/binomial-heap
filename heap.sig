signature HEAP =
sig

  type 'a heap

  val createHeap : 'a heap
  val heapify : 'a list -> 'a heap
  val findMin : 'a heap -> 'a
  val findMax : 'a heap -> 'a
  val deleteMin : 'a heap -> 'a heap
  val merge : 'a heap -> 'a heap -> 'a heap
  val increaseKey : 'a heap -> 'a heap
  val decreaseKey : 'a heap -> 'a heap
  val insert : 'a heap -> 'a -> 'a heap
  val size : 'a heap -> int
  val isEmpty : 'a heap -> bool

end
