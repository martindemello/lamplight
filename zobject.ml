open Core.Std
open Ztypes
open Printf

type property = {
  size_byte: int;
  size: int;
  number: int;
  data: string
}

(* v1 - v3 *)
type zobject = {
  number: int;
  flags : int32;
  parent : int;
  sibling : int;
  child : int;
  properties_addr : address;
  description: string;
  properties : property list
}

(* v1 - v3 *)
let obj_size = 9
let offset = 62

let read_property mem addr =
  let size_byte = Memory.get_byte mem addr in
  let size = size_byte lsr 5 + 1 in
  let number = size_byte land 0x1F in
  let data = Memory.get_blob mem (addr + 1) size in
  { size_byte; size; number; data }

let read_properties mem addr =
  let rec read i acc =
    let p = read_property mem i in
    if p.size_byte = 0 then List.rev acc else read (i + p.size + 1) (p :: acc)
  in
  read addr []

let object_address game num =
  let base = game.header.obj_table in
  base + offset + (num - 1) * obj_size

let init game number =
  let mem = game.mem in
  let addr = object_address game number in
  let open Memory in
  let get_flag_byte n acc =
    let to_int32 b = match Int32.of_int b with Some x -> x | None -> Int32.zero in
    let b = to_int32 @@ get_byte mem (addr + n) in
    let f = Int32.shift_left b (24 - n * 8) in
    Int32.bit_or acc f
  in
  let flags = List.fold_right ~f:get_flag_byte ~init:Int32.zero [0; 1; 2; 3] in
  let parent = get_byte mem (addr + 4) in
  let sibling = get_byte mem (addr + 5) in
  let child = get_byte mem (addr + 6) in
  let addr = get_word mem (addr + 7) in
  let len = Memory.get_byte mem addr in
  let description = if len = 0 then "" else Zscii.read_string game (addr + 1) in
  let properties = read_properties mem (addr + len * 2 + 1) in
  { number; flags; parent; sibling; child; description;
    properties_addr = ByteAddr addr; properties
  }

(* Following infodump from ztools *)
let dump obj =
  let hex_of_char c = sprintf "%02x" @@ Char.to_int c in
  let hex_of_string s = String.concat_map ~sep:" " ~f:hex_of_char s in
  printf "%3d. Attributes: %08lx\n" obj.number obj.flags;
  printf "%4s Parent object: %3d  Sibling object: %3d  Child object: %3d\n"
    "" obj.parent obj.sibling obj.child;
  printf "%4s Property address: %04x\n" "" (int_of_address obj.properties_addr);
  printf "%8s Description: \"%s\"\n" "" obj.description;
  printf "%9s Properties:\n" "";
  List.iter ~f:(fun p ->
      printf "%13s [%2d] %s\n" "" p.number (hex_of_string p.data))
    obj.properties;
  printf "\n"

(* Section 12 Remarks: The largest valid object number is not directly stored
 * anywhere in the Z-machine. Utility programs like Infodump deduce this number
 * by assuming that, initially, the object entries end where the first property
 * table begins.
 *)

let dump_all game =
  let rec get_all i min_prop acc =
    if object_address game i >= min_prop then
      (i - 1, List.rev acc)
    else begin
      let o = init game i in
      let p = int_of_address o.properties_addr in
      get_all (i + 1) (min p min_prop) (o :: acc)
    end
  in
  let n, objs = get_all 1 0xffff [] in
  printf "
    **** Objects ****

  Object count = %d\n\n" n;

  List.iter ~f:dump objs
