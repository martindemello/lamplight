open Core.Std
open Ztypes

type zchar = int

let unpack_word w =
  let bit = w lsr 15 in
  let first : zchar = (w lsr 10) land 31 in
  let second : zchar = (w lsr 5) land 31 in
  let third : zchar = w land 31 in
  (bit, first, second, third)

(* Alphabets *)

type alphabet = A0 | A1 | A2

let a0 = "abcdefghijklmnopqrstuvwxyz"
let a1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let a2 = " ^0123456789.,!?_#'\"/\\-:()"

type shift = S2 | S3 (* See section 3.2.2 *)

let shift_alphabet = function
  | S2 -> (function A0 -> A1 | A1 -> A2 | A2 -> A0)
  | S3 -> (function A0 -> A2 | A1 -> A0 | A2 -> A1)

(* Reader *)

type ztoken = Char of char | Abbr of int | Newline
            | Shift of shift | Lock of shift
            | Digraph | LowCharError

exception String_read_error

type reader_state = {
  mutable base : alphabet;
  mutable current : alphabet;
}

let shift state x =
  state.current <- shift_alphabet x state.base

let shift_lock state x =
  state.current <- shift_alphabet x state.base;
  state.base <- state.current

let unshift state =
  state.current <- state.base

let read_char zchar alpha =
  let x = zchar - 6 in
  match alpha, zchar with
  | A0, _ -> Char a0.[x]
  | A1, _ -> Char a1.[x]
  | A2, 6 -> Digraph
  | A2, 7  -> Newline
  | A2, _ -> Char a2.[x]

module type READER = sig
  val read_low_char : zchar -> ztoken
end

module Z1Reader : READER = struct
  let read_low_char z = match z with
    | 0 -> Char ' '
    | 1 -> Newline
    | 2 -> Shift S2
    | 3 -> Shift S3
    | 4 -> Lock S2
    | 5 -> Lock S3
    | _ -> LowCharError
end

module Z2Reader : READER = struct
  let read_low_char z = match z with
    | 0 -> Char ' '
    | 1 -> Abbr z
    | 2 -> Shift S2
    | 3 -> Shift S3
    | 4 -> Lock S2
    | 5 -> Lock S3
    | _ -> LowCharError
end

module Z3Reader : READER = struct
  let read_low_char z = match z with
    | 0 -> Char ' '
    | 1 | 2 | 3 -> Abbr z
    | 4 -> Shift S2
    | 5 -> Shift S3
    | _ -> LowCharError
end

let read_zstring mem addr =
  let rec read mem addr zchars =
    let w = Memory.get_word mem addr in
    let bit, first, second, third = unpack_word w in
    let zchars = third :: second :: first :: zchars in
    if bit = 1 then
      List.rev zchars
    else
      read mem (addr + 2) zchars
  in
  read mem addr []


(* ZSCII *)

module Input = struct
  exception Unrecognized_input

  type input =
    | Delete
    | Newline
    | Escape
    | Ascii of char
    | Cursor_Up
    | Cursor_Down
    | Cursor_Left
    | Cursor_Right
    | Fn of int
    | Keypad of int
    | Extra of int
    | Menu_click
    | Double_click
    | Single_click

  let of_zscii z = match z with
    | 8 -> Delete
    | 13 -> Newline
    | 27 -> Escape
    | 129 -> Cursor_Up
    | 130 -> Cursor_Down
    | 131 -> Cursor_Left
    | 132 -> Cursor_Right
    | 252 -> Menu_click
    | 253 -> Double_click
    | 254 -> Single_click
    | x when (32 <= x && x <= 126) -> Ascii (Char.of_int_exn x)
    | x when (133 <= x && x <= 144) -> Fn (x - 132)
    | x when (145 <= x && x <= 154) -> Keypad (x - 145)
    | x when (155 <= x && x <= 251) -> Extra x
    | _ -> raise Unrecognized_input
end

module Output = struct
  exception Unrecognized_output

  type output =
    | Null
    | Newline
    | Ascii of char
    | Extra of int

  let of_zscii z = match z with
    | 0 -> Null
    | 13 -> Newline
    | x when (32 <= x && x <= 126) -> Ascii (Char.of_int_exn x)
    | x when (155 <= x && x <= 251) -> Extra x
    | _ -> raise Unrecognized_output
end

(* Digraphs *)
let digraph h l = "TODO"

let parse_string game read_low_char zchars =
  let open Z in
  let open Header in
  let mem = game.mem in
  let abbr_table = game.header.abbr_table in
  let rec abbr i j =
    let ptr = abbr_table + 2 * (32 * (i - 1) + j) in
    let address = WordAddr (Memory.get_u16 mem ptr) in
    let address = int_of_address address in
    let zs = read_zstring mem address in
    parse true zs
  and parse is_abbr zchars =
    let state = { base = A0; current = A0 } in
    let buf = Buffer.create 32 in
    let rec read zchars = match zchars with
      | [] -> ()
      | z :: zs -> begin
          let token =
            if z < 6 then read_low_char z else read_char z state.current
          in
          unshift state; (* restore state after reading char *)
          match token with
          | Shift s -> shift state s; read zs
          | Lock s -> shift_lock state s; read zs
          | Char c -> Buffer.add_char buf c; read zs
          | Newline -> Buffer.add_char buf '\n'; read zs
          | Abbr a -> (
              match is_abbr, zs with
              | true, _ -> raise String_read_error (* nested abbreviation *)
              | _, [] -> raise String_read_error
              | _, z' :: zs' ->
                Buffer.add_string buf (abbr a z'); read zs'
            )
          | Digraph -> (
              match zs with
              | h :: l :: zs' -> Buffer.add_string buf (digraph h l); read zs'
              | _ -> raise String_read_error
            )
          | LowCharError -> raise String_read_error
        end
    in
    read zchars;
    Buffer.contents buf
  in
  parse false zchars

let read_string game addr =
  parse_string game Z3Reader.read_low_char (read_zstring game.mem addr)
