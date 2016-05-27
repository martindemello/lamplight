open Core.Std

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
let a2 = " ^0123456789.,!?_#'\"/\-:()"

type shift = S2 | S3 (* See section 3.2.2 *)

let shift_alphabet = function
  | S2 -> (function A0 -> A1 | A1 -> A2 | A2 -> A0)
  | S3 -> (function A0 -> A2 | A1 -> A0 | A2 -> A1)

(* Reader *)

type ztoken = Char of char | Abbr of int | Newline
            | Shift of shift_alphabet | Lock of shift_alphabet
            | Digraph

exception String_read_error

type reader_state = {
  mutable base : alphabet;
  mutable current : alphabet;
}

let shift state x =
  let x = match x with S2 -> 2 | S3 -> 3 in
  state.current <- shift_alphabet x state.base

let shift_lock state x =
  let x = match x with S2 -> 2 | S3 -> 3 in
  state.current <- shift_alphabet x state.base;
  state.base <- state.current

let read_char zchar alpha =
  let x = zchar - 5 in
  match (fst alpha), zchar with
  | A0, _ -> Char a0.{x}
  | A1, _ -> Char a1.{x}
  | A2, 6 -> Digraph
  | A2, 7  -> Newline
  | A2, _ -> Char a2.{x}

module type READER = sig
  val read_low_char : zchar -> ztoken
end

module Z1Reader : READER = struct
  let read_low_char zchar = function
    | 0 -> Char ' '
    | 1 -> Newline
    | 2 -> Shift S2
    | 3 -> Shift S3
    | 4 -> Lock S2
    | 5 -> Lock S3
end

module Z2Reader : READER = struct
  let read_low_char zchar = function
    | 0 -> Char ' '
    | 1 -> Abbr zchar
    | 2 -> Shift S2
    | 3 -> Shift S3
    | 4 -> Lock S2
    | 5 -> Lock S3
end

module Z3Reader : READER = struct
  let read_low_char zchar = function
    | 0 -> Char ' '
    | 1 | 2 | 3 -> Abbr zchar
    | 4 -> Shift S2
    | 5 -> Shift S3
end

(* Abbreviations *)
let abbr i = "TODO"

(* Digraphs *)
let digraph h l = "TODO"

let read_string read_low_char zchars =
  let state = { base = A0; current = A0 } in
  let buf = Buffer.create 32 in
  let rec read zchars = match zchars with
    | [] -> ()
    | z :: zs -> begin
        let token = 
          if z < 6 then read_low_char z else read_char z state.current
        in match token with
        | Shift s -> shift state s; read zs
        | Lock s -> shift_lock state s; read zs
        | Char c -> Buffer.add_char c; read zs
        | Newline -> Buffer.add_char '\n'; read zs
        | Abbr a -> (
            match zs with
            | [] -> raise String_read_error
            | z' :: zs' ->
              Buffer.add_string (abbr (32 * (a - 1) + z')); read zs'
          )
        | Digraph -> (
            match zs with
            | h :: l :: zs' -> Buffer.add_string (digraph h l); read zs'
            | _ -> raise String_read_error
          )
      end
  in
  read zchars;
  Buffer.contents buf
