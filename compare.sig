signature COMPARE =
sig

  datatype order = LESS | EQUAL | GREATER

  val compare : 'a * 'a -> order

end
