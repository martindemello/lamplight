type address = WordAddr of int | ByteAddr of int | PackedAddr of int
  
let int_of_address = function WordAddr i -> i | ByteAddr i -> i | PackedAddr i -> i

