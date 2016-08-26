open Core
open Core.Result

type t = {
  mutable top: int;
  mutable local: int;
  max : int;
  stack: int array
}

let make size = {
  top = 0;
  local = 0;
  max = size - 1;
  stack = Array.make size 0
}

let push s v = 
  if s.top == s.max then
    Error "Stack overflow"
  else begin
    s.top <- s.top + 1;
    s.stack.(s.top) <- v;
    Ok ()
  end

let pop s =
  if s.top == 0 then
    Error "Stack underflow"
  else begin
    let ret = s.stack.(s.top) in
    s.top <- s.top - 1;
    Ok ret
  end

let peek s =
  s.stack.(s.top)

let get s n =
  s.stack.(n)
