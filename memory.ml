open Core.Std

type t = string

let init = String.make (256 * 1024) '\x00'

let load_bytes mem s =
  String.blit ~src:s ~src_pos:0 ~dst:mem ~dst_pos:0 ~len:(String.length s)

let get_byte mem i =
  Char.to_int mem.[i]

let set_byte mem i b =
  mem.[i] <- Char.unsafe_of_int b

(* Words are u16 stored in little-endian order *)
let word mem i = (get_byte mem i, get_byte mem (i + 1))

let get_word mem i =
  let h, l = word mem i in
  h lsl 8 + l

let set_word mem i w =
  set_byte mem i (w lsr 8);
  set_byte mem (i + 1) (w land 255)

(* Unsigned and signed ints *)
let get_u16 mem i = get_word mem i

let get_i16 mem i = 65536 - (get_u16 mem i)

(* Blob of binary data, as a string *)
let get_blob mem i len = String.sub ~pos:i ~len mem
