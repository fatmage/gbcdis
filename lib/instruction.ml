type r8 = A | B | C | D | E | H | L 
type r16 = AF | BC | DE | HL | SP | PC
type flag = Fz | Fn | Fh | Fc
type condition = Cnz | Cz | Cnc | Cc | Cnotnz | Cnotz | Cnotnc | Cnotc

(* Instruction type *)
type instruction =
(*             mnemonic 1st arg  2nd arg  size*)
  | Binary  of string * string * string * int
  | Unary   of string * string * int
  | Nullary of string * int

let str_of_r8 = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | H -> "H"
  | L -> "L"

let str_of_r16 = function
  | AF -> "AF"
  | BC -> "BC"
  | DE -> "DE"
  | HL -> "HL"
  | SP -> "SP"
  | PC -> "PC"

let str_of_cond = function 
  | Cnz    -> "NZ"
  | Cz     -> "Z"
  | Cnc    -> "NC"
  | Cc     -> "C"
  | Cnotnz -> "!NZ"
  | Cnotz  -> "!Z"
  | Cnotnc -> "!NC"
  | CnotC  -> "!C"

let str_of_flag = function
  | Fz -> "z"
  | Fn -> "n"
  | Fh -> "h"
  | Fc -> "c"

let str_of_int = Printf.sprintf "0x%X"

let make_ptr = fun str -> "[" ^ str ^ "]"
let make_inc = fun str -> str ^ "+"
let make_dec = fun str -> str ^ "-"

let strptr_of_r8 = fun r -> r8 |> str_of_r8 |> make_ptr
let strptr_of_r16 = fun r -> f16 |> str_of_16 |> make_ptr

let strptrdec_of_r8 = fun r -> r8 |> str_of_r8 |> make_dec |> make_ptr
let strptrdec_of_r16 = fun r -> f16 |> str_of_16 |> make_dec |> make_ptr

let strptrinc_of_r8 = fun r -> r8 |> str_of_r8 |> make_inc |> make_ptr
let strptrinc_of_r16 = fun r -> f16 |> str_of_16 |> make_inc |> make_ptr

let strptr_of_int = fun n -> n |> str_of_int |> make_ptr

let str_A = str_of_r8 A
let str_C = str_of_r8 C
let str_HL = str_of_r16 HL


(** All operations as values of the algebraic type operation **)

(* 8-bit Arithmetic and Logic Instructions *)
let iADC_Ar8  = fun r -> Binary ("ADC", str_A, str_of_r8 r, 1)
let iADC_AHLp =          Binary ("ADC", str_A, strptr_of_r16 HL, 1) 
let iADC_An8  = fun n -> Binary ("ADC", str_A, str_of_int n, 2)
let iADD_Ar8  = fun r -> Binary ("ADD", str_A, str_of_r8 r, 1)
let iADD_AHLp =          Binary ("ADD", str_A, strptr_of_r16 HL, 1) 
let iADD_An8  = fun n -> Binary ("ADD", str_A, str_of_int n, 2)
let iAND_Ar8  = fun r -> Binary ("AND", str_A, str_of_r8 r, 1)
let iAND_AHLp =          Binary ("AND", str_A, strptr_of_r16 HL, 1) 
let iAND_An8  = fun n -> Binary ("AND", str_A, str_of_int n, 2)
let iCP_Ar8   = fun r -> Binary ("CP",  str_A, str_of_r8 r, 1)
let iCP_AHLp  =          Binary ("CP",  str_A, strptr_of_r16 HL, 1) 
let iCP_An8   = fun n -> Binary ("CP",  str_A, str_of_int n, 2)
let iDEC_r8   = fun r -> Unary ("DEC", str_of_r8 r, 1)
let iDEC_HLp  =          Unary ("DEC", strptr_of_r16 HL, 1) 
let iINC_r8  = fun r ->  Unary ("INC", str_of_r8 r, 1)
let iINC_HLp =           Unary ("INC", strptr_of_r16 HL, 1) 
let iOR_Ar8   = fun r -> Binary ("OR",  str_A, str_of_r8 r, 1)
let iOR_AHLp  =          Binary ("OR",  str_A, strptr_of_r16 HL, 1)
let iOR_An8   = fun n -> Binary ("OR",  str_A, str_of_int n, 2)
let iSBC_Ar8  = fun r -> Binary ("SBC", str_A, str_of_r8 r, 1)
let iSBC_AHLp =          Binary ("SBC", str_A, strptr_of_r16 HL, 1) 
let iSBC_An8  = fun n -> Binary ("SBC", str_A, str_of_int n, 2)
let iSUB_Ar8  = fun r -> Binary ("SUB", str_A, str_of_r8 r, 1)
let iSUB_AHLp =          Binary ("SUB", str_A, strptr_of_r16 HL, 1)
let iSUB_An8  = fun n -> Binary ("SUB", str_A, str_of_int n, 2)
let iXOR_Ar8  = fun r -> Binary ("XOR", str_A, str_of_r8 r, 1)
let iXOR_AHLp =          Binary ("XOR", str_A, strptr_of_r16 HL, 1)
let iXOR_An8  = fun n -> Binary ("XOR", str_A, str_of_int n, 2)

(* 16-bit Arithmetic Instructions *)
let iADD_HLr16 = fun r -> Binary ("ADD", str_HL, str_of_r16 r, 1)
let iDEC_r16   = fun r -> Unary ("DEC", str_of_r16 r, 1)
let iINC_r16   = fun r -> Unary ("INC", str_of_r16 r, 1)

(* Bit Operations Instructions *)
let iBIT_u3r8  = fun u r -> Binary ("BIT", str_of_int u, str_of_r8 r, 2)
let iBIT_u3HLp = fun u ->   Binary ("BIT", str_of_int u, strptr_of_r16 HL, 2)
let iRES_u3r8  = fun u r -> Binary ("RES", str_of_int u, str_of_r8 r, 2)
let iRES_u3HLp = fun u ->   Binary ("RES", str_of_int u, strptr_of_r16 HL, 2)
let iSET_u3r8  = fun u r -> Binary ("SET", str_of_int u, str_of_r8 r, 2)
let iSET_u3HLp = fun u ->   Binary ("SET", str_of_int u, strptr_of_r16 HL, 2)
let iSWAP_r8   = fun r ->   Unary ("SWAP", str_of_r8 r, 2)
let iSWAP_HLp  =            Unary ("SWAP", strptr_of_r16 HL, 2)

(* Bit Shift Instructions *)
let iRL_r8   = fun r -> Unary ("RL", str_of_r8 r, 2)
let iRL_HLp  =          Unary ("RL", strptr_of_r16 HL, 2)
let iRLA     =          Nullary ("RLA", 1)
let iRLC_r8  = fun r -> Unary ("RLC", str_of_r8 r, 2)
let iRLC_HLp =          Unary ("RLC", strptr_of_r16 HL, 2)
let iRLCA    =          Nullary ("RLCA", 1)
let iRR_r8   = fun r -> Unary ("RR", str_of_r8 r, 2)
let iRR_HLp  =          Unary ("RR", strptr_of_r16 HL, 2)
let iRRA     =          Nullary ("RRA", 1)
let iRRC_r8  = fun r -> Unary ("RRC", str_of_r8 r, 2)
let iRRC_HLp =          Unary ("RRC", strptr_of_r16 HL, 2)
let iRRCA    =          Nullary ("RRCA", 1)
let iSLA_r8  = fun r -> Unary ("SLA", str_of_r8 r, 2)
let iSLA_HLp =          Unary ("SLA", strptr_of_r16 HL, 2)
let iSRA_r8  = fun r -> Unary ("SRA", str_of_r8 r, 2)
let iSRA_HLp =          Unary ("SRA", strptr_of_r16 HL, 2)
let iSRL_r8  = fun r -> Unary ("SRL", str_of_r8 r, 2)
let iSRL_HLp =          Unary ("SRL", strptr_of_r16 HL, 2)

(* Load Instructions *)
let iLD_rr8    = fun ra rb -> Binary ("LD", str_of_r8 ra, str_of_r8 rb, 1)
let iLD_rn8    = fun r n ->   Binary ("LD", str_of_r8 r, str_of_int n, 2)
let iLD_rn16   = fun r n ->   Binary ("LD", str_of_r16 r, str_of_int n, 3)
let iLD_HLpr8  = fun r ->     Binary ("LD", strptr_of_r16 HL, str_of_r8 r, 1)
let iLD_HLpn8  = fun n ->     Binary ("LD", strptr_of_r16 HL, str_of_int n, 2)
let iLD_r8HLp  = fun r ->     Binary ("LD", str_of_r8 r, strptr_of_r16 HL, 1)
let iLD_r16pA  = fun r ->     Binary ("LD", strptr_of_r16 r, str_A, 1)
let iLD_n16pA  = fun n ->     Binary ("LD", strptr_of_int n, str_A, 3)
let iLDH_n16pA = fun n ->     Binary ("LDH", strptr_of_int n, str_A, 2)
let iLDH_CpA   =              Binary ("LDH", strptr_of_r8 C, str_A, 1)
let iLD_Ar16p  = fun r ->     Binary ("LD", str_A, strptr_of_r16 r, 1)
let iLD_An16p  = fun n ->     Binary ("LD", str_A, strptr_of_int n, 3)
let iLDH_An16p = fun n ->     Binary ("LDH", str_A, strptr_of_int n, 2)
let iLDH_ACp   =              Binary ("LDH", str_A, strptr_of_r8 C, 1)
let iLD_HLIpA  =              Binary ("LD", strptrinc_of_r16 HL, str_A, 1)
let iLD_HLDpA  =              Binary ("LD", strptrdec_of_r16 HL, str_A, 1)
let iLD_AHLIp  =              Binary ("LD", str_A, strptrinc_of_r16 HL, 1)
let iLD_AHLDp  =              Binary ("LD", str_A, strptrdec_of_r16 HL, 1)

(* Jumps and Subroutines *)
let iCALL_n16  = fun n ->   Unary ("CALL", str_of_int n, 3)
let iCALL_cn16 = fun c n -> Binary ("CALL", str_of_cond c, str_of_int n, 3)
let iJP_HL     =            Unary ("JP", str_HL, 1)
let iJP_n16    = fun n ->   Unary ("JP", str_of_int n, 3)
let iJP_cn16   = fun c n -> Binary ("JP", str_of_cond c, str_of_int n, 3)
let iJR_n16    = fun n ->   Unary ("JR", str_of_int n, 2)
let iJR_cn16   = fun c n -> Binary ("JR", str_of_cond c, str_of_int n, 2)
let iRET_c     = fun c ->   Unary ("RET", str_of_cond c, 1)
let iRET       =            Nullary ("RET", 1)
let iRETI      =            Nullary ("RETI", 1)
let iRST_vec   = fun n ->   Unary ("RST", str_of_int n, 1)

(* Stack Operations Instructions *)
let iADD_HLSP  =          Binary ("ADD", str_HL, str_SP, 1)
let iADD_SPe8  = fun e -> Binary ("ADD", str_SP, str_of_int e, 2)
let iDEC_SP    =          Unary ("DEC", str_SP, 1)
let iINC_SP    =          Unary ("INC", str_SP, 1)
let iLD_SPn16  = fun n -> Binary ("LD", str_SP, str_of_int n, 3)
let iLD_n16pSP = fun n -> Binary ("LD", strptr_of_int n, str_SP, 3)
let iLD_HLSPe8 = fun e -> Binary ("LD", str_HL, str_SP ^ "+" ^ str_of_int e, 2)
let iLD_SPHL   =          Binary ("LD", str_SP, str_HL, 1)
let iPOP_AF    =          Unary ("POP", str_AF, 1)
let iPOP_r16   = fun r -> Unary ("POP", str_of_r16 r, 1)
let iPUSH_AF   =          Unary ("PUSH", str_AF, 1)
let iPUSH_r16  =          Unary ("PUSH", str_of_r16 r, 1)

(* Miscellaneous Instructions *)
let iCCF  = Nullary ("CCF", 1)
let iCPL  = Nullary ("CPL", 1)
let iDAA  = Nullary ("DAA", 1)
let iDI   = Nullary ("DI", 1)
let iEI   = Nullary ("EI", 1)
let iHALT = Nullary ("HALT", 1)
let iNOP  = Nullary ("NOP", 1)
let iSCF  = Nullary ("SCF", 1)
let iSTOP = Nullary ("STOP", 2)