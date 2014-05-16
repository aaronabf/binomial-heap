signature COMPARE =
sig

  datatype order = LESS | EQUAL | GREATER

  type t
  val compare : t * t -> order

end
