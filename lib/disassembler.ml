open Instruction

(*           label    position *)
type label = string * int 

let read_byte ch = 
  match In_channel.input_byte ch with 
  | None -> failwith "Not enough bytes to read."
  | Some byte -> byte

let read_2bytes ch = 
  let first  = read_byte ch in 
  let second = read_byte ch in 
  first * 256 + second

let decode_file = fun file_ch ->
  let rec decode_aux = fun acc ->
    match In_channel.input_byte file_ch with 
    | None      -> List.rev acc
    | Some byte -> 
      let next_instruction =
        match byte with 
        | 0x00 -> iNOP
        | 0x01 -> let n16 = read_2bytes file_ch in iLD_rn16 BC n16
        | 0x02 -> iLD_r16pA BC
        | 0x03 -> iINC_r16 BC 
        | 0x04 -> iINC_r8 B 
        | 0x05 -> iDEC_r8 B 
        | 0x06 -> let n8 = read_byte file_ch in iLD_rn8 B n8 
        | 0x07 -> iRLCA 
        | 0x08 -> let n16 = read_2bytes file_ch in iLD_n16pSP n16 
        | 0x09 -> iADD_HLr16 BC 
        | 0x0A -> iLD_Ar16p BC 
        | 0x0B -> iDEC_r16 BC 
        | 0x0C -> iINC_r8 C 
        | 0x0D -> iDEC_r8 C 
        | 0x0E -> let n8 = read_byte file_ch in iLD_rn8 C n8 
        | 0x0F -> iRRCA 
        | 0x10 -> let n8 = read_byte file_ch in iSTOP n8 
        | 0x11 -> let n16 = read_2bytes file_ch in iLD_rn16 DE n16 
        | 0x12 -> iLD_r16pA DE 
        | 0x13 -> iINC_r16 DE 
        | 0x14 -> iINC_r8 D 
        | 0x15 -> iDEC_r8 D 
        | 0x16 -> let n8 = read_byte file_ch in iLD_rn8 D n8 
        | 0x17 -> iRLA
        | 0x18 -> let n8 = read_byte file_ch in iJR_n16 n8 
        | 0x19 -> iADD_HLr16 DE
        | 0x1A -> iLD_Ar16p DE
        | 0x1B -> iDEC_r16 DE
        | 0x1C -> iINC_r8 E
        | 0x1D -> iDEC_r8 E
        | 0x1E -> let n8 = read_byte file_ch in iLD_rn8 E n8
        | 0x1F -> iRRA
        | 0x20 -> let n8 = read_byte file_ch in iJR_cn16 Cnz n8
        | 0x21 -> let n16 = read_2bytes file_ch in iLD_rn16 HL n16
        | 0x22 -> iLD_HLIpA
        | 0x23 -> iINC_r16 HL
        | 0x24 -> iINC_r8 H
        | 0x25 -> iDEC_r8 H
        | 0x26 -> let n8 = read_byte file_ch in iLD_rn8 H n8 
        | 0x27 -> iDAA
        | 0x28 -> let n8 = read_byte file_ch in iJR_cn16 Cz n8
        | 0x29 -> iADD_HLr16 HL 
        | 0x2A -> iLD_AHLIp
        | 0x2B -> iDEC_r16 HL
        | 0x2C -> iINC_r8 L
        | 0x2D -> iDEC_r8 L
        | 0x2E -> let n8 = read_byte file_ch in iLD_rn8 L n8
        | 0x2F -> iCPL
        | 0x30 -> let n8 = read_byte file_ch in iJR_cn16 Cnc n8 
        | 0x31 -> let n8 = read_byte file_ch in iLD_SPn16 n8
        | 0x32 -> iLD_HLDpA
        | 0x33 -> iINC_SP
        | 0x34 -> iINC_HLp
        | 0x35 -> iDEC_HLp
        | 0x36 -> let n8 = read_byte file_ch in iLD_HLpn8 n8
        | 0x37 -> iSCF
        | 0x38 -> let n8 = read_byte file_ch in iJR_cn16 Cc n8 
        | 0x39 -> iADD_HLSP
        | 0x3A -> iLD_AHLDp
        | 0x3B -> iDEC_SP
        | 0x3C -> iINC_r8 A
        | 0x3D -> iDEC_r8 A
        | 0x3E -> let n8 = read_byte file_ch in iLD_rn8 A n8
        | 0x3F -> iCCF
        | 0x40 -> iLD_rr8 B B
        | 0x41 -> iLD_rr8 B C
        | 0x42 -> iLD_rr8 B D
        | 0x43 -> iLD_rr8 B E
        | 0x44 -> iLD_rr8 B H
        | 0x45 -> iLD_rr8 B L
        | 0x46 -> iLD_r8HLp B
        | 0x47 -> iLD_rr8 B A
        | 0x48 -> iLD_rr8 C B
        | 0x49 -> iLD_rr8 C C
        | 0x4A -> iLD_rr8 C D
        | 0x4B -> iLD_rr8 C E
        | 0x4C -> iLD_rr8 C H
        | 0x4D -> iLD_rr8 C L
        | 0x4E -> iLD_r8HLp C
        | 0x4F -> iLD_rr8 C A
        | 0x50 -> iLD_rr8 D B
        | 0x51 -> iLD_rr8 D C
        | 0x52 -> iLD_rr8 D D
        | 0x53 -> iLD_rr8 D E
        | 0x54 -> iLD_rr8 D H
        | 0x55 -> iLD_rr8 D L
        | 0x56 -> iLD_r8HLp D
        | 0x57 -> iLD_rr8 D A
        | 0x58 -> iLD_rr8 E B
        | 0x59 -> iLD_rr8 E C
        | 0x5A -> iLD_rr8 E D
        | 0x5B -> iLD_rr8 E E
        | 0x5C -> iLD_rr8 E H
        | 0x5D -> iLD_rr8 E L
        | 0x5E -> iLD_r8HLp E
        | 0x5F -> iLD_rr8 E A
        | 0x60 -> iLD_rr8 H B
        | 0x61 -> iLD_rr8 H C
        | 0x62 -> iLD_rr8 H D
        | 0x63 -> iLD_rr8 H E
        | 0x64 -> iLD_rr8 H H
        | 0x65 -> iLD_rr8 H L
        | 0x66 -> iLD_r8HLp H
        | 0x67 -> iLD_rr8 H A
        | 0x68 -> iLD_rr8 L B
        | 0x69 -> iLD_rr8 L C
        | 0x6A -> iLD_rr8 L D
        | 0x6B -> iLD_rr8 L E
        | 0x6C -> iLD_rr8 L H
        | 0x6D -> iLD_rr8 L L
        | 0x6E -> iLD_r8HLp L
        | 0x6F -> iLD_rr8 L A
        | 0x70 -> iLD_HLpr8 B
        | 0x71 -> iLD_HLpr8 C
        | 0x72 -> iLD_HLpr8 D
        | 0x73 -> iLD_HLpr8 E
        | 0x74 -> iLD_HLpr8 H
        | 0x75 -> iLD_HLpr8 L
        | 0x76 -> iHALT
        | 0x77 -> iLD_HLpr8 A 
        | 0x78 -> iLD_rr8 A B 
        | 0x79 -> iLD_rr8 A C 
        | 0x7A -> iLD_rr8 A D 
        | 0x7B -> iLD_rr8 A E
        | 0x7C -> iLD_rr8 A H
        | 0x7D -> iLD_rr8 A L
        | 0x7E -> iLD_r8HLp A
        | 0x7F -> iLD_rr8 A A
        | 0x80 -> iADD_Ar8 B
        | 0x81 -> iADD_Ar8 C
        | 0x82 -> iADD_Ar8 D
        | 0x83 -> iADD_Ar8 E
        | 0x84 -> iADD_Ar8 H
        | 0x85 -> iADD_Ar8 L
        | 0x86 -> iADD_AHLp
        | 0x87 -> iADD_Ar8 A
        | 0x88 -> iADC_Ar8 B
        | 0x89 -> iADC_Ar8 C
        | 0x8A -> iADC_Ar8 D
        | 0x8B -> iADC_Ar8 E
        | 0x8C -> iADC_Ar8 H
        | 0x8D -> iADC_Ar8 L
        | 0x8E -> iADC_AHLp
        | 0x8F -> iADC_Ar8 A
        | 0x90 -> iSUB_Ar8 B
        | 0x91 -> iSUB_Ar8 C 
        | 0x92 -> iSUB_Ar8 D
        | 0x93 -> iSUB_Ar8 E
        | 0x94 -> iSUB_Ar8 H
        | 0x95 -> iSUB_Ar8 L
        | 0x96 -> iSUB_AHLp
        | 0x97 -> iSUB_Ar8 A
        | 0x98 -> iSBC_Ar8 B
        | 0x99 -> iSBC_Ar8 C
        | 0x9A -> iSBC_Ar8 D
        | 0x9B -> iSBC_Ar8 E
        | 0x9C -> iSBC_Ar8 H
        | 0x9D -> iSBC_Ar8 L
        | 0x9E -> iSBC_AHLp
        | 0x9F -> iSBC_Ar8 A
        | 0xA0 -> iAND_Ar8 B
        | 0xA1 -> iAND_Ar8 C
        | 0xA2 -> iAND_Ar8 D
        | 0xA3 -> iAND_Ar8 E
        | 0xA4 -> iAND_Ar8 H
        | 0xA5 -> iAND_Ar8 L
        | 0xA6 -> iAND_AHLp
        | 0xA7 -> iAND_Ar8 A
        | 0xA8 -> iXOR_Ar8 B
        | 0xA9 -> iXOR_Ar8 C
        | 0xAA -> iXOR_Ar8 D
        | 0xAB -> iXOR_Ar8 E
        | 0xAC -> iXOR_Ar8 H
        | 0xAD -> iXOR_Ar8 L
        | 0xAE -> iXOR_AHLp
        | 0xAF -> iXOR_Ar8 A
        | 0xB0 -> iOR_Ar8 B
        | 0xB1 -> iOR_Ar8 C
        | 0xB2 -> iOR_Ar8 D
        | 0xB3 -> iOR_Ar8 E
        | 0xB4 -> iOR_Ar8 H
        | 0xB5 -> iOR_Ar8 L
        | 0xB6 -> iOR_AHLp
        | 0xB7 -> iOR_Ar8 A
        | 0xB8 -> iCP_Ar8 B
        | 0xB9 -> iCP_Ar8 C
        | 0xBA -> iCP_Ar8 D 
        | 0xBB -> iCP_Ar8 E
        | 0xBC -> iCP_Ar8 H
        | 0xBD -> iCP_Ar8 L
        | 0xBE -> iCP_AHLp
        | 0xBF -> iCP_Ar8 A
        | 0xC0 -> iRET_c Cnz
        | 0xC1 -> iPOP_r16 BC
        | 0xC2 -> let n16 = read_2bytes file_ch in iJP_cn16 Cnz n16 
        | 0xC3 -> let n16 = read_2bytes file_ch in iJP_n16 n16
        | 0xC4 -> let n16 = read_2bytes file_ch in iCALL_cn16 Cnz n16
        | 0xC5 -> iPUSH_r16 BC
        | 0xC6 -> let n8 = read_2bytes file_ch in iADD_An8 n8
        | 0xC7 -> iRST_vec 0x00
        | 0xC8 -> iRET_c Cz
        | 0xC9 -> iRET
        | 0xCA -> let n16 = read_2bytes file_ch in iJP_cn16 Cz n16
        (* | 0xCB ->  prefix CB *)
        | 0xCC -> let n16 = read_2bytes file_ch in iCALL_cn16 Cz n16 
        | 0xCD -> let n16 = read_2bytes file_ch in iCALL_n16 n16
        | 0xCE -> let n8 = read_byte file_ch in iADC_An8 n8
        | 0xCF -> iRST_vec 0x08
        | 0xD0 -> iRET_c Cnc
        | 0xD1 -> iPOP_r16 DE
        | 0xD2 -> let n16 = read_2bytes file_ch in iJP_cn16 Cnc n16
        (* | 0xD3 ->  iNOP*)
        | 0xD4 -> let n16 = read_2bytes file_ch in iCALL_cn16 Cnc n16
        | 0xD5 -> iPUSH_r16 DE
        | 0xD6 -> let n8 = read_byte file_ch in iSUB_An8 n8
        | 0xD7 -> iRST_vec 0x10
        | 0xD8 -> iRET_c Cc
        | 0xD9 -> iRETI
        | 0xDA -> let n16 = read_2bytes file_ch in iJP_cn16 Cc n16
        (* | 0xDB -> iNOP  *)
        | 0xDC -> let n16 = read_2bytes file_ch in iCALL_cn16 Cc n16
        (* | 0xDD -> iNOP  *)
        | 0xDE -> let n8 = read_byte file_ch in iSBC_An8 n8
        | 0xDF -> iRST_vec 0x18
        | 0xE0 -> let n8 = read_byte file_ch in iLDH_n16pA n8
        | 0xE1 -> iPOP_r16 HL
        | 0xE2 -> iLDH_CpA
        (* | 0xE3 -> iNOP  *)
        (* | 0xE4 -> iNOP  *)
        | 0xE5 -> iPUSH_r16 HL
        | 0xE6 -> let n8 = read_byte file_ch in iAND_An8 n8
        | 0xE7 -> iRST_vec 0x20
        | 0xE8 -> let n8 = read_byte file_ch in iADD_SPe8 n8
        | 0xE9 -> iJP_HL
        | 0xEA -> let n16 = read_byte file_ch in iLD_n16pA n16
        (* | 0xEB -> iNOP  *)
        (* | 0xEC -> iNOP  *)
        (* | 0xED -> iNOP  *)
        | 0xEE -> let n8 = read_byte file_ch in iXOR_An8 n8
        | 0xEF -> iRST_vec 0x28
        | 0xF0 -> let n8 = read_byte file_ch in iLDH_An16p n8 
        | 0xF1 -> iPOP_AF
        | 0xF2 -> iLDH_ACp
        | 0xF3 -> iDI
        (* | 0xF4 -> iNOP  *)
        | 0xF5 -> iPUSH_AF
        | 0xF6 -> let n8 = read_byte file_ch in iOR_An8 n8
        | 0xF7 -> iRST_vec 0x30
        | 0xF8 -> let n8 = read_byte file_ch in iLD_HLSPe8 n8
        | 0xF9 -> iLD_SPHL
        | 0xFA -> let n16 = read_2bytes file_ch in iLD_An16p n16
        | 0xFB -> iEI
        (* | 0xFC -> iNOP  *)
        (* | 0xFD -> iNOP  *)
        | 0xFE -> let n8 = read_byte file_ch in iCP_An8 n8
        | 0xFF -> iRST_vec 0x38
        | _    -> iNOP 
      in decode_aux (next_instruction :: acc)
    in decode_aux []

let make_labels = fun instructions ->
  let rec label_aux = fun is acc pos ->
    match is with 
    | []      -> List.rev acc
    | i :: is -> label_aux is (i :: acc) pos
      (* do matching on mnemonic -> add a label if necessary *)
  in
  label_aux instructions [] 0