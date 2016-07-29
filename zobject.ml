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
    printf "Read property: %d %d\n" p.number p.size;
    if p.size_byte = 0 then List.rev acc else read (i + p.size + 1) (p :: acc)
  in
  read addr []

let init game num =
  let mem = game.mem in
  let base = game.header.obj_table in
  let addr = base + offset + (num - 1) * obj_size in
  let open Memory in
  let get n acc = 
    let to_int32 b = match Int32.of_int b with Some x -> x | None -> Int32.zero in
    let b = to_int32 @@ get_byte mem (addr + n) in
    let f = Int32.shift_left b (24 - n * 8) in
    Int32.bit_or acc f
  in
  let flags = List.fold_right ~f:get ~init:Int32.zero [0; 1; 2; 3] in
  let parent = get_byte mem (addr + 4) in
  let sibling = get_byte mem (addr + 5) in
  let child = get_byte mem (addr + 6) in
  let addr = get_word mem (addr + 7) in
  let len = Memory.get_byte mem addr in
  let description = Zscii.read_string game (addr + 1) in
  let properties = read_properties mem (addr + len * 2 + 1) in
  { flags; parent; sibling; child; description;
    properties_addr = ByteAddr addr; properties
  }

let dump obj =
  let d = int_of_address in
  let hex_of_char c = sprintf "%x" @@ Char.to_int c in
  let hex_of_string s = String.concat_map ~sep:" " ~f:hex_of_char s in
  printf "flags: %08lx\nparent: %d\nsibling: %d\nchild: %d\n"
    obj.flags obj.parent obj.sibling obj.child;
    printf "Property address: %04x\n    Description: %s\n     Properties:\n" (d obj.properties_addr) obj.description;
  List.iter ~f:(fun p -> printf "         [%d] %s\n" p.number (hex_of_string p.data)) obj.properties

