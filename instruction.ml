open Core.Std
open Ztypes
open Opcode

exception Operand_error

type instruction = {
  opcode: opcode;
  operand_types: operand_type list;
  operands: int list;
  store_variable: char;
  branch_offset: int;
  text: string
}

(* get the bit at position n, 0-indexed *)
let ( <?> ) x n =
  (x lsr n) land 0x01

(* get the two bits at position n and n-1, 0-indexed *)
let ( <??> ) x n =
  (x lsr n) land 0x03

let execute game op values = ()

let get_operand game op_type =
  match op_type with
  | OPERAND_LARGE -> Z.read_u16 game
  | OPERAND_SMALL -> Z.read_byte game
  | OPERAND_VARIABLE ->
      let var = Z.read_byte game in
      Z.get_variable game var
  | OPERAND_OMITTED -> raise Operand_error

let get_operands game operand_types =
  List.map ~f:(get_operand game) operand_types

let unpack_short game opcode =
  let op_type = get_operand_type (opcode <??> 4) in
  let instruction = opcode land 0x0f in
  match op_type with
  | OPERAND_OMITTED -> (get_opcode_0op instruction, [])
  | _ -> (get_opcode_1op instruction, [op_type])

let unpack_var game opcode =
  let instruction = opcode land 0x1f in
  let op = match opcode <?> 5 with
    | 0 -> get_opcode_2op instruction
    | _ -> get_opcode_var instruction
  in
  let op_types_byte = Z.read_byte game in
  let get_type n = get_operand_type (op_types_byte <??> n) in
  let operand_types =
    List.map ~f:get_type [6; 4; 2; 0]
    |> List.filter ~f:(fun x -> x <> OPERAND_OMITTED)
  in
  (op, operand_types)

let unpack_long game opcode =
	let instruction = opcode land 0x1f in
  let get_type n = get_operand_type (opcode <?> n + 1) in
  let operand_types = List.map ~f:get_type [6; 5] in
  let op = get_opcode_2op instruction in
  (op, operand_types)

let get_instruction game =
  let opcode = Z.read_byte game in
  let form = opcode <??> 6 in
  let unpack = match form with
    | 0x02 -> unpack_short
    | 0x03 -> unpack_var
    | _ -> unpack_long
  in
  let op, operand_types = unpack game opcode in
  let values = get_operands game operand_types in
  execute game op values
