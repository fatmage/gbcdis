open Gbcdis


let () = 
  let argv = Sys.argv in
  if Array.length argv < 3 then begin
    prerr_endline "Usage : main <binary_filename> <output_filename>";
    exit 2
  end;

  let binary_channel = open_in_bin argv.(1) in
  let output_channel = open_out argv.(2) in
  let is,ls = Disassembler.process_file binary_channel in
  Printer.print_code (is,ls) output_channel;

