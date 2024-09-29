type r8 = A | B | C | D | E | H | L 
type r16 = AF | BC | DE | HL | SP | PC
type flag = Fz | Fn | Fh | Fc
type condition = Cnz | Cz | Cnc | Cc | Cnotnz | Cnotz | Cnotnc | Cnotc

(* Instruction type *)
type instruction =
(*             mnemonic 1st arg  2nd arg *)
  | Binary  of string * string * string
  | Unary   of string * string
  | Nullary of string
  | Noop

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

let strptr_of_r8 = fun r8 -> r8 |> str_of_r8 |> make_ptr
let strptr_of_r16 = fun r16 -> f16 |> str_of_16 |> make_ptr

let strptrdec_of_r8 = fun r8 -> r8 |> str_of_r8 |> make_dec |> make_ptr
let strptrdec_of_r16 = fun r16 -> f16 |> str_of_16 |> make_dec |> make_ptr

let strptrinc_of_r8 = fun r8 -> r8 |> str_of_r8 |> make_inc |> make_ptr
let strptrinc_of_r16 = fun r16 -> f16 |> str_of_16 |> make_inc |> make_ptr

let strptr_of_int = fun n -> n |> str_of_int |> make_ptr

let str_A = str_of_r8 A
let str_C = str_of_r8 C
let str_HL = str_of_r16 HL


(** All operations as values of the algebraic type operation **)

(* 8-bit Arithmetic and Logic Instructions *)
let iADC_Ar8  = fun r8 -> Binary ("ADC", str_A, str_of_r8 r8)
let iADC_AHLp =           Binary ("ADC", str_A, strptr_of_r16 HL) 
let iADC_An8  = fun n8 -> Binary ("ADC", str_A, str_of_int n8)
let iADD_Ar8  = fun r8 -> Binary ("ADD", str_A, str_of_r8 r8)
let iADD_AHLp =           Binary ("ADD", str_A, strptr_of_r16 HL) 
let iADD_An8  = fun n8 -> Binary ("ADD", str_A, str_of_int n8)
let iAND_Ar8  = fun r8 -> Binary ("AND", str_A, str_of_r8 r8)
let iAND_AHLp =           Binary ("AND", str_A, strptr_of_r16 HL) 
let iAND_An8  = fun n8 -> Binary ("AND", str_A, str_of_int n8)
let iCP_Ar8   = fun r8 -> Binary ("CP",  str_A, str_of_r8 r8)
let iCP_AHLp  =           Binary ("CP",  str_A, strptr_of_r16 HL) 
let iCP_An8   = fun n8 -> Binary ("CP",  str_A, str_of_int n8)
let iDEC_Ar8  = fun r8 -> Binary ("DEC", str_A, str_of_r8 r8)
let iDEC_AHLp =           Binary ("DEC", str_A, strptr_of_r16 HL) 
let iINC_Ar8  = fun r8 -> Binary ("INC", str_A, str_of_r8 r8)
let iINC_AHLp =           Binary ("INC", str_A, strptr_of_r16 HL) 
let iOR_Ar8   = fun r8 -> Binary ("OR",  str_A, str_of_r8 r8)
let iOR_AHLp  =           Binary ("OR",  str_A, strptr_of_r16 HL)
let iOR_An8   = fun n8 -> Binary ("OR",  str_A, str_of_int n8)
let iSBC_Ar8  = fun r8 -> Binary ("SBC", str_A, str_of_r8 r8)
let iSBC_AHLp =           Binary ("SBC", str_A, strptr_of_r16 HL) 
let iSBC_An8  = fun n8 -> Binary ("SBC", str_A, str_of_int n8)
let iSUB_Ar8  = fun r8 -> Binary ("SUB", str_A, str_of_r8 r8)
let iSUB_AHLp =           Binary ("SUB", str_A, strptr_of_r16 HL)
let iSUB_An8  = fun n8 -> Binary ("SUB", str_A, str_of_int n8)
let iXOR_Ar8  = fun r8 -> Binary ("XOR", str_A, str_of_r8 r8)
let iXOR_AHLp =           Binary ("XOR", str_A, strptr_of_r16 HL)
let iXOR_An8  = fun n8 -> Binary ("XOR", str_A, str_of_int n8)

(* 16-bit Arithmetic Instructions *)
let iADD_HLr16 = fun r16 -> Binary ("ADD", str_HL, str_of_r16 r16)
let iDEC_r16   = fun r16 -> Unary ("DEC", str_of_r16 r16)
let iINC_r16   = fun r16 -> Unary ("INC", str_of_r16 r16)

(* Bit Operations Instructions *)
let iBIT_u3r8  = fun u3 r8 -> Binary ("BIT", str_of_int u3, str_of_r8 r8)
let iBIT_u3HLp = fun u3 ->    Binary ("BIT", str_of_int u3, strptr_of_r16 HL)
let iRES_u3r8  = fun u3 r8 -> Binary ("RES", str_of_int u3, str_of_r8 r8)
let iRES_u3HLp = fun u3 ->    Binary ("RES", str_of_int u3, strptr_of_r16 HL)
let iSET_u3r8  = fun u3 r8 -> Binary ("SET", str_of_int u3, str_of_r8 r8)
let iSET_u3HLp = fun u3 ->    Binary ("SET", str_of_int u3, strptr_of_r16 HL)
let iSWAP_r8   = fun r8 ->    Unary ("SWAP", str_of_r8 r8)
let iSWAP_HLp  =              Unary ("SWAP", strptr_of_r16 HL)

(* Bit Shift Instructions *)
let iRL_r8   = fun r8 -> Unary ("RL", str_of_r8 r8)
let iRL_HLp  =           Unary ("RL", strptr_of_r16 HL)
let iRLA     =           Nullary ("RLA")
let iRLC_r8  = fun r8 -> Unary ("RLC", str_of_r8 r8)
let iRLC_HLp =           Unary ("RLC", strptr_of_r16 HL)
let iRLCA    =           Nullary ("RLCA")
let iRR_r8   = fun r8 -> Unary ("RR", str_of_r8 r8)
let iRR_HLp  =           Unary ("RR", strptr_of_r16 HL)
let iRRA     =           Nullary ("RRA")
let iRRC_r8  = fun r8 -> Unary ("RRC", str_of_r8 r8)
let iRRC_HLp =           Unary ("RRC", strptr_of_r16 HL)
let iRRCA    =           Nullary ("RRCA")
let iSLA_r8  = fun r8 -> Unary ("SLA", str_of_r8 r8)
let iSLA_HLp =           Unary ("SLA", strptr_of_r16 HL)
let iSRA_r8  = fun r8 -> Unary ("SRA", str_of_r8 r8)
let iSRA_HLp =           Unary ("SRA", strptr_of_r16 HL)
let iSRL_r8  = fun r8 -> Unary ("SRL", str_of_r8 r8)
let iSRL_HLp =           Unary ("SRL", strptr_of_r16 HL)

(* Load Instructions *)
let iLD_rr8    = fun r8a r8b -> Binary ("LD", str_of_r8 r8a, str_of_r8 r8b)
let iLD_rn8    = fun r8 n8 ->   Binary ("LD", str_of_r8 r8, str_of_int n8)
let iLD_rn16   = fun r16 n16 -> Binary ("LD", str_of_r16 r16, str_of_int n16)
let iLD_HLpr8  = fun r8 ->      Binary ("LD", strptr_of_r16 HL, str_of_r8 r8)
let iLD_HLpn8  = fun n8 ->      Binary ("LD", strptr_of_r16 HL, str_of_int n8)
let iLD_r8HLp  = fun r8 ->      Binary ("LD", str_of_r8 r8, strptr_of_r16 HL)
let iLD_r16pA  = fun r16 ->     Binary ("LD", strptr_of_r16 r16, str_A)
let iLD_n16pA  = fun n16 ->     Binary ("LD", strptr_of_int n16, str_A)
let iLDH_n16pA = fun n16 ->     Binary ("LDH", strptr_of_int n16, str_A)
let iLDH_CpA   =                Binary ("LDH", strptr_of_r8 C, str_A)
let iLD_Ar16p  = fun r16 ->     Binary ("LD", str_A, strptr_of_r16 r16)
let iLD_An16p  = fun n16 ->     Binary ("LD", str_A, strptr_of_int n16)
let iLDH_An16p = fun n16 ->     Binary ("LDH", str_A, strptr_of_int n16)
let iLDH_ACp   =                Binary ("LDH", str_A, strptr_of_r8 C)
let iLD_HLIpA  =                Binary ("LD", strptrinc_of_r16 HL, str_A)
let iLD_HLDpA  =                Binary ("LD", strptrdec_of_r16 HL, str_A)
let iLD_AHLIp  =                Binary ("LD", str_A, strptrinc_of_r16 HL)
let iLD_AHLDp  =                Binary ("LD", str_A, strptrdec_of_r16 HL)

(* Jumps and Subroutines *)
let iCALL_cn16 = fun c n16 -> Binary ("CALL", str_of_cond c, str_of_int n16)
let iJP_HL     =              Unary ("JP", str_HL)
let iJP_n16    = fun n16 ->   Unary ("JP", str_of_int n16)
let iJP_cn16   = fun c n16 -> Binary ("JP", str_of_cond c, str_of_int n16)
let iJR_n16    = fun n16 ->   Unary ("JR", str_of_int n16)
let iJR_cn16   = fun c n16 -> Binary ("JR", str_of_cond c, str_of_int n16)
let iRET_c     = fun c ->     Unary ("RET", str_of_cond c)
let iRET       =              Nullary ("RET")
let iRETI      =              Nullary ("RETI")
let iRST_vec   = fun n ->     Unary ("RST", str_of_int n)

(* Stack Operations Instructions *)
let iADD_HLSP  =            Binary ("ADD", str_HL, str_SP)
let iADD_SPe8  = fun e8 ->  Binary ("ADD", str_SP, str_of_int e8)
let iDEC_SP    =            Unary ("DEC", str_SP)
let iINC_SP    =            Unary ("INC", str_SP)
let iLD_SPn16  = fun n16 -> Binary ("LD", str_SP, str_of_int n16)
let iLD_n16pSP = fun n16 -> Binary ("LD", strptr_of_int n16, str_SP)
let iLD_HLSPe8 = fun e8 ->  Binary ("LD", str_HL, str_SP ^ "+" ^ str_of_int e8)
let iLD_SPHL   =            Binary ("LD", str_SP, str_HL)
let iPOP_AF    =            Unary ("POP", str_AF)
let iPOP_r16   = fun r16 -> Unary ("POP", str_of_r16 r16)
let iPUSH_AF   =            Unary ("PUSH", str_AF)
let iPUSH_r16  =            Unary ("PUSH", str_of_r16 r16)

(* Miscellaneous Instructions *)
let iCCF  = Nullary ("CCF")
let iCPL  = Nullary ("CPL")
let iDAA  = Nullary ("DAA")
let iDI   = Nullary ("DI")
let iEI   = Nullary ("EI")
let iHALT = Nullary ("HALT")
let iNOP  = Noop
let iSCF  = Nullary ("SCF")
let iSTOP = Nullary ("STOP")