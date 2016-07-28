open Ztypes
open Printf

(* v1 - v3 *)
type zobject = {
  flags : int32;
  parent : int;
  sibling : int;
  child : int;
  properties_addr : address
}

type properties = {
  description : string;
  properties : int list
}

(* v1 - v3 *)
let obj_size = 9
let offset = 62

let init game num =
  let mem = game.mem in
  let base = game.header.obj_table in
  let addr = base + offset + (num - 1) * obj_size in
  let open Memory in
  let get acc n = 
    let b = get_byte mem (addr + n) in
    let f = Int32.(shift_left (of_int b) (24 - n * 8)) in
    Int32.logor acc f
  in
  let acc = Int32.zero in
  let acc = get acc 0 in
  let acc = get acc 1 in
  let acc = get acc 2 in
  let acc = get acc 3 in
  {
    flags = acc;
    parent = get_byte mem (addr + 4);
    sibling = get_byte mem (addr + 5);
    child = get_byte mem (addr + 6);
    properties_addr = ByteAddr (get_word mem (addr + 7))
  }

let properties game addr =
  let mem = game.mem in
  let addr = int_of_address addr in
  let len = Memory.get_byte mem addr in
  let desc = Zscii.read_string game (addr + 1) in
  (len, desc)

let dump obj =
  let d = int_of_address in
  Printf.printf "flags: %08lx\nparent: %d\nsibling: %d\nchild: %d\nproperty_addr: %04x\n" 
    obj.flags obj.parent obj.sibling obj.child (d obj.properties_addr)

let dump_props len desc =
  Printf.printf " len: %d\n description: %s\n" len desc
