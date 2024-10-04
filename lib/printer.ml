open Instruction


let line_of_instruction = fun instr pos ->
  match instr with 
  | Binary (mnemonic, arg1, arg2, len) -> Printf.sprintf
    "        %-4s   %-9s  %-9s   ;  0x%04X" mnemonic arg1 arg2 pos
  | Unary (mnemonic, arg, len_) -> Printf.sprintf
    "        %-4s   %-9s              ;  0x%04X" mnemonic arg pos
  | Nullary (mnemonic, len) -> Printf.sprintf
    "        %-4s                          ;  0x%04X" mnemonic pos


let print_code = fun (is, ls) file_ch ->
  let rec aux = fun (is, ls) pos ->
  match is, ls with
  | [], _ -> ()
  | i :: is, l :: ls -> 
    let len = match i with Binary (_, _, _, len) | Unary (_, _, len) | Nullary (_, len) -> len
    in if pos == fst l
    then let _ = output_string file_ch (snd l ^ " " ^ (string_of_int (fst l)) ^ "\n") in aux (i :: is, ls) pos
    else if pos > fst l 
    then aux (i :: is, ls) pos
    else let _ = output_string file_ch (line_of_instruction i pos ^ "\n") in aux (is, l :: ls) (pos + len)
  | i :: is, [] ->
    let len = match i with Binary (_, _, _, len) | Unary (_, _, len) | Nullary (_, len) -> len
    in let _ = output_string file_ch (line_of_instruction i pos ^ "\n") in aux (is, []) (pos + len)
  in aux (is, ls) 0


