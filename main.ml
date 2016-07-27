open Core.Std
open Printf

let _ =
  let fname = "ZORK1.DAT" in
  let game = Z.init 65536 fname in
  Z.dump_header game;
  let mem = Z.(game.mem) in
  let b = Memory.get_byte mem 0x1f1 in
  printf "byte: %x\n" b;
  let header = Z.(game.header) in
  let base = Z.Header.(header.obj_table) in
  let obj = Zobject.init mem base 1 in
  Zobject.dump obj
