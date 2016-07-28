type address = ByteAddr of int | WordAddr of int | PackedAddr of int

let int_of_address = function
  | ByteAddr i -> i
  | WordAddr i -> 2 * i
  | PackedAddr i -> 2 * i (* v1-3 *)

type header = {
  version : int;
  hi_mem: int;
  init_pc: int;
  dict: int;
  obj_table: int;
  globals: int;
  static_mem: int;
  abbr_table: int
}

type zmachine = {
  mutable header : header;
  mutable pc : int;
  stack: Zstack.t;
  mutable call_state: int list;
  mem : Memory.t
}

