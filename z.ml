open Core.Std
open Ztypes

exception Stack_error

module Header = struct
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
    Printf.printf "version: %d\nhi_mem: %x\ninit_pc: %x\ndict: %x\nobj_t: %x\nglobals: %x\nstatic_mem: %x\nabbrs: %x\n" 
    h.version h.hi_mem h.init_pc h.dict h.obj_table h.globals h.static_mem h.abbr_table
end

let init ~stack_size ~fname =
  let contents = In_channel.read_all fname in
  let mem = Memory.init in
  Memory.load_bytes mem contents;
  Printf.printf "read %d bytes\n" (String.length contents);
  let header = Header.init mem in
  {
    stack = Zstack.make stack_size;
    call_state = [];
    pc = 0;
    mem = mem;
    header = header
  }

let dump_header game =
  Header.dump game.header

let read_byte game =
  let ret = Memory.get_byte game.mem game.pc in
  game.pc <- game.pc + 1;
  ret

let read_u16 game =
  let ret = Memory.get_u16 game.mem game.pc in
  game.pc <- game.pc + 2;
  ret

let get_global game var =
  42

let get_variable game var =
  if var = 0 then
    match Zstack.pop game.stack with
    | Ok ret -> ret
    | _ -> raise Stack_error
  else if var < 0x10 then
    Zstack.get game.stack (var - 1)
  else
    get_global game var
