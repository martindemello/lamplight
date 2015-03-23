type opcode_2op =
  | NOP
  | JE
  | JL
  | JG
  | DEC_CHK
  | INC_CHK
  | JIN
  | TEST
  | OR
  | AND
  | TEST_ATTR
  | SET_ATTR
  | CLEAR_ATTR
  | STORE
  | INSERT_OBJ
  | LOADW
  | LOADB
  | GET_PROP
  | GET_PROP_ADDR
  | GET_NEXT_PROP
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | CALL_2S
  | CALL_2N
  | SET_COLOUR
  | THROW [@@deriving enum]

type opcode_1op =
  | JZ
  | GET_SIBLING
  | GET_CHILD
  | GET_PARENT
  | GET_PROP_LEN
  | INC
  | DEC
  | PRINT_ADDR
  | CALL_1S
  | REMOVE_OBJ
  | PRINT_OBJ
  | RET
  | JUMP
  | PRINT_PADDR
  | LOAD
  | NOT [@@deriving enum]

type opcode_0op =
  | RTRUE
  | RFALSE
  | PRINT
  | PRINT_RET
  | NOP
  | SAVE
  | RESTORE
  | RESTART
  | RET_POPPED
  | POP
  | CATCH
  | QUIT
  | NEW_LINE
  | SHOW_STATUS
  | VERIFY
  | EXTENDED
  | PIRACY [@@deriving enum]

type opcode_var =
  | CALL
  | STOREW
  | STOREB
  | PUT_PROP
  | SREAD
  | PRINT_CHAR
  | PRINT_NUM
  | RANDOM
  | PUSH
  | PULL
  | SPLIT_WINDOW
  | SET_WINDOW
  | CALL_VS2
  | ERASE_WINDOW
  | ERASE_LINE
  | SET_CURSOR
  | GET_CURSOR
  | SET_TSTYLE
  | BUFFER_MODE
  | OUTPUT_STREAM
  | INPUT_STREAM
  | SOUND_EFFECT
  | READ_CHAR
  | SCAN_TABLE
  | NOT_5
  | CALL_VN
  | CALL_VN2
  | TOKENIZE
  | ENCODE_TEXT
  | COPY_TABLE
  | PRINT_TABLE
  | CHECK_ARG_COUNT [@@deriving enum]

  (* v5 onwards *)
type opcode_ext =
  | SAVE_EXT
  | RESTORE_EXT
  | LOG_SHIFT
  | ART_SHIFT
  | SET_FONT
  | DRAW_PICTURE
  | PICTURE_DATA
  | ERASE_PICTURE
  | SET_MARGINS
  | SAVE_UNDO
  | RESTORE_UNDO
  | PRINT_UNICODE
  | CHECK_UNICODE
  | MOVE_WINDOW
  | WINDOW_SIZE
  | WINDOW_STYLE
  | GET_WIND_PROP
  | SCROLL_WINDOW
  | POP_STACK
  | READ_MOUSE
  | MOUSE_WINDOW
  | PUSH_STACK
  | PUT_WIND_PROP
  | PRINT_FORM
  | MAKE_MENU [@@deriving enum]

(* following txd from the inform tools *)
type op_class = EXTENDED_OPERAND | TWO_OPERAND | ONE_OPERAND | ZERO_OPERAND | VARIABLE_OPERAND

let class_of_code version opcode =
  if version > 4 && opcode == 0xbe then
    EXTENDED_OPERAND (* not supported yet *)
  else if (opcode < 0x80) then
    TWO_OPERAND
  else if (opcode < 0xb0) then
    ONE_OPERAND
  else if (opcode < 0xc0) then
    ZERO_OPERAND
  else
    VARIABLE_OPERAND

type opcode = Op2 of opcode_2op | Op1 of opcode_1op | Op0 of opcode_0op
            | OpVar of opcode_var | OpExt of opcode_ext

let opcode_of_int version code =
  let f x = match x with Some i -> i | None -> raise (Failure "opcode_of_int failed!") in
  match class_of_code version code with
  | EXTENDED_OPERAND -> OpExt (f (opcode_ext_of_enum (code land 0x3f)))
  | TWO_OPERAND -> Op2 (f (opcode_2op_of_enum (code land 0x1f)))
  | VARIABLE_OPERAND when ((code > 191) && (code < 224)) ->
      Op2 (f (opcode_2op_of_enum (code land 0x3f)))
  | VARIABLE_OPERAND -> OpVar (f (opcode_var_of_enum (code land 0x3f)))
  | ONE_OPERAND -> Op1 (f (opcode_1op_of_enum (code land 0x0f)))
  | ZERO_OPERAND -> Op0 (f (opcode_0op_of_enum (code land 0x0f)))
