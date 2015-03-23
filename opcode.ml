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
  | JZ [@value 128]
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
  | NOT_2
  | CALL_1N [@@deriving enum]

type opcode_0op =
  | RTRUE [@value 176]
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
  | CALL [@value 224]
  | CALL_VS
  | STOREW
  | STOREB
  | PUT_PROP
  | SREAD
  | AREAD
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
  | NOT
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
