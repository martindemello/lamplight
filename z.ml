open Core.Std

module Memory = struct
  type t = string

  let init = String.make (256 * 1024) '\x00'

  let load_bytes mem s =
    String.blit ~src:s ~src_pos:0 ~dst:mem ~dst_pos:0 ~len:(String.length s)

  let get_byte mem i =
    Char.to_int mem.[i]

  let set_byte mem i b =
    mem.[i] <- Char.unsafe_of_int b

  (* Words are u16 stored in little-endian order *)

  let get_word mem i =
    ((get_byte mem i) lsl 8) + (get_byte mem (i + 1))

  let set_word mem i w =
    set_byte mem i (w lsr 8);
    set_byte mem (i + 1) (w land 255)
end

module Header = struct
  type t = {
    version : int;
    hi_mem: int;
    init_pc: int;
    dict: int;
    obj_table: int;
    globals: int;
    static_mem: int;
    abbr_table: int
  }

  let init mem = 
    let open Memory in
    {
      version    = get_byte mem 0x00;
      hi_mem     = get_word mem 0x04;
      init_pc    = get_word mem 0x06;
      dict       = get_word mem 0x08;
      obj_table  = get_word mem 0x0a;
      globals    = get_word mem 0x0c;
      static_mem = get_word mem 0x0e;
      abbr_table = get_word mem 0x18
    }

  let dump h =
    Printf.printf "version: %d\nhi_mem: %d\ninit_pc: %d\ndict: %d\nobj_t: %d\nglobals: %d\nstatic_mem: %d\nabbrs: %d\n" 
    h.version h.hi_mem h.init_pc h.dict h.obj_table h.globals h.static_mem h.abbr_table
end

type zmachine = {
  mutable header : Header.t;
  mutable pc : int;
  stack: Zstack.t;
  mutable call_state: int list;
  mem : Memory.t
}

let init ~stack_size ~fname =
  let contents = In_channel.read_all fname in
  let mem = Memory.init in
  Memory.load_bytes mem contents;
  {
    stack = Zstack.make stack_size;
    call_state = [];
    pc = 0;
    mem = mem;
    header = Header.init mem
  }

let _ =
  let fname = "ZORK1.DAT" in
  let contents = In_channel.read_all fname in
  let mem = Memory.init in
  Memory.load_bytes mem contents;
  let header = Header.init mem in
  Header.dump header

