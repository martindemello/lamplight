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

type opcode = Op2op of opcode_2op | Op1op of opcode_1op | Op0op of opcode_0op
            | OpVar of opcode_var | OpExt of opcode_ext

type operand_type =
    OPERAND_LARGE | OPERAND_SMALL | OPERAND_VARIABLE
  | OPERAND_OMITTED [@@deriving enum]

exception Trusted_enum_failure

let trust = function
  | Some x -> x
  | None -> raise Trusted_enum_failure

(* The "cannot fail" versions *)
let get_operand_type n = trust @@ operand_type_of_enum n
let get_opcode_0op n = Op0op (trust @@ opcode_0op_of_enum n)
let get_opcode_1op n = Op1op (trust @@ opcode_1op_of_enum n)
let get_opcode_2op n = Op2op (trust @@ opcode_2op_of_enum n)
let get_opcode_var n = OpVar (trust @@ opcode_var_of_enum n)
let get_opcode_ext n = OpExt (trust @@ opcode_ext_of_enum n)
