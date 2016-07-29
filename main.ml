open Core.Std
open Printf
open Ztypes

let _ =
  let fname = "ZORK1.DAT" in
  let game = Z.init 65536 fname in
  Z.dump_header game;
  let obj = Zobject.init game 1 in
  Zobject.dump obj
