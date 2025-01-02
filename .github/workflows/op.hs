module EVM.Op
  ( Op
  , GenericOp (..)
  , opString
  , intToOpName
  , getOp
  , readOp
  ) where

import EVM.Expr qualified as Expr
import EVM.Types

import Data.Vector qualified as V
import Data.Word (Word8)
import Numeric (showHex)
import Witch (into)

intToOpName:: Int -> String
intToOpName a =
  case a of
    0x00 -> "STOP"
    0x01 -> "ADD"
    0x02 -> "MUL"
    0x03 -> "SUB"
    0x04 -> "DIV"
    0x05 -> "SDIV"
    0x06 -> "MOD"
    0x07 -> "SMOD"
    0x08 -> "ADDMOD"
    0x09 -> "MULMOD"
    0x0a -> "EXP"
    0x0b -> "SIGNEXTEND"
    --
    0x10 -> "LT"
    0x11 -> "GT"
    0x12 -> "SLT"
    0x13 -> "SGT"
    0x14 -> "EQ"
    0x15 -> "ISZERO"
    0x16 -> "AND"
    0x17 -> "OR"
    0x18 -> "XOR"
    0x19 -> "NOT"
    0x1a -> "BYTE"
    0x1b -> "SHL"
    0x1c -> "SHR"
    0x1d -> "SAR"
    0x20 -> "SHA3"
    0x30 -> "ADDRESS"
    --
    0x31 -> "BALANCE"
    0x32 -> "ORIGIN"
    0x33 -> "CALLER"
    0x34 -> "CALLVALUE"
    0x35 -> "CALLDATALOAD"
    0x36 -> "CALLDATASIZE"
    0x37 -> "CALLDATACOPY"
    0x38 -> "CODESIZE"
    0x39 -> "CODECOPY"
    0x3a -> "GASPRICE"
    0x3b -> "EXTCODESIZE"
    0x3c -> "EXTCODECOPY"
    0x3d -> "RETURNDATASIZE"
    0x3e -> "RETURNDATACOPY"
    0x3f -> "EXTCODEHASH"
    0x40 -> "BLOCKHASH"
    0x41 -> "COINBASE"
    0x42 -> "TIMESTAMP"
    0x43 -> "NUMBER"
    0x44 -> "PREVRANDAO"
    0x45 -> "GASLIMIT"
    0x46 -> "CHAINID"
    0x47 -> "SELFBALANCE"
    0x48 -> "BASEFEE"
    0x49 -> "BLOBHASH"
    0x4A -> "BLOBBASEFEE"
    0x50 -> "POP"
    0x51 -> "MLOAD"
    0x52 -> "MSTORE"
    0x53 -> "MSTORE8"
    0x54 -> "SLOAD"
    0x55 -> "SSTORE"
    0x56 -> "JUMP"
    0x57 -> "JUMPI"
    0x58 -> "PC"
    0x59 -> "MSIZE"
    0x5a -> "GAS"
    0x5b -> "JUMPDEST"
    0x5c -> "TLOAD"
    0x5d -> "TSTORE"
    --
    0x5e -> "MCOPY"
    0x5f -> "PUSH0"
    0x60 -> "PUSH1"
    0x61 -> "PUSH2"
    0x62 -> "PUSH3"
    0x63 -> "PUSH4"
    0x64 -> "PUSH5"
    0x65 -> "PUSH6"
    0x66 -> "PUSH7"
    0x67 -> "PUSH8"
    0x68 -> "PUSH9"
    0x69 -> "PUSH10"
    0x6a -> "PUSH11"
    0x6b -> "PUSH12"
    0x6c -> "PUSH13"
    0x6d -> "PUSH14"
    0x6e -> "PUSH15"
    0x6f -> "PUSH16"
    0x70 -> "PUSH17"
    0x71 -> "PUSH18"
    0x72 -> "PUSH19"
    0x73 -> "PUSH20"
    0x74 -> "PUSH21"
    0x75 -> "PUSH22"
    0x76 -> "PUSH23"
    0x77 -> "PUSH24"
    0x78 -> "PUSH25"
    0x79 -> "PUSH26"
    0x7a -> "PUSH27"
    0x7b -> "PUSH28"
    0x7c -> "PUSH29"
    0x7d -> "PUSH30"
    0x7e -> "PUSH31"
    0x7f -> "PUSH32"
    --
    0x80 -> "DUP1"
    0x81 -> "DUP2"
    0x82 -> "DUP3"
    0x83 -> "DUP4"
    0x84 -> "DUP5"
    0x85 -> "DUP6"
    0x86 -> "DUP7"
    0x87 -> "DUP8"
    0x88 -> "DUP9"
    0x89 -> "DUP10"
    0x8a -> "DUP11"
    0x8b -> "DUP12"
    0x8c -> "DUP13"
    0x8d -> "DUP14"
    0x8e -> "DUP15"
    0x8f -> "DUP16"
    --
    0x90 -> "SWAP1"
    0x91 -> "SWAP2"
    0x92 -> "SWAP3"
    0x93 -> "SWAP4"
    0x94 -> "SWAP5"
    0x95 -> "SWAP6"
    0x96 -> "SWAP7"
    0x97 -> "SWAP8"
    0x98 -> "SWAP9"
    0x99 -> "SWAP10"
    0x9a -> "SWAP11"
    0x9b -> "SWAP12"
    0x9c -> "SWAP13"
    0x9d -> "SWAP14"
    0x9e -> "SWAP15"
    0x9f -> "SWAP16"
    --
    0xa0 -> "LOG0"
    0xa1 -> "LOG1"
    0xa2 -> "LOG2"
    0xa3 -> "LOG3"
    0xa4 -> "LOG4"
    --
    0xf0 -> "CREATE"
    0xf1 -> "CALL"
    0xf2 -> "CALLCODE"
    0xf3 -> "RETURN"
    0xf4 -> "DELEGATECALL"
    0xf5 -> "CREATE2"
    0xfa -> "STATICCALL"
    0xfd -> "REVERT"
    0xfe -> "INVALID"
    0xff -> "SELFDESTRUCT"
    _ -> "UNKNOWN (0x" <> showHex a "" <> ")"

opString :: (Integral a, Show a) => (a, Op) -> String
opString (i, o) = let showPc x | x < 0x10 = '0' : showHex x ""
                               | otherwise = showHex x ""
                  in showPc i <> " " ++ case o of
  OpStop -> "STOP"
  OpAdd -> "ADD"
  OpMul -> "MUL"
  OpSub -> "SUB"
  OpDiv -> "DIV"
  OpSdiv -> "SDIV"
  OpMod -> "MOD"
  OpSmod -> "SMOD"
  OpAddmod -> "ADDMOD"
  OpMulmod -> "MULMOD"
  OpExp -> "EXP"
  OpSignextend -> "SIGNEXTEND"
  OpLt -> "LT"
  OpGt -> "GT"
  OpSlt -> "SLT"
  OpSgt -> "SGT"
  OpEq -> "EQ"
  OpIszero -> "ISZERO"
  OpAnd -> "AND"
  OpOr -> "OR"
  OpXor -> "XOR"
  OpNot -> "NOT"
  OpByte -> "BYTE"
  OpShl -> "SHL"
  OpShr -> "SHR"
  OpSar -> "SAR"
  OpSha3 -> "SHA3"
  OpAddress -> "ADDRESS"
  OpBalance -> "BALANCE"
  OpOrigin -> "ORIGIN"
  OpCaller -> "CALLER"
  OpCallvalue -> "CALLVALUE"
  OpCalldataload -> "CALLDATALOAD"
  OpCalldatasize -> "CALLDATASIZE"
  OpCalldatacopy -> "CALLDATACOPY"
  OpCodesize -> "CODESIZE"
  OpCodecopy -> "CODECOPY"
  OpGasprice -> "GASPRICE"
  OpExtcodesize -> "EXTCODESIZE"
  OpExtcodecopy -> "EXTCODECOPY"
  OpReturndatasize -> "RETURNDATASIZE"
  OpReturndatacopy -> "RETURNDATACOPY"
  OpExtcodehash -> "EXTCODEHASH"
  OpBlockhash -> "BLOCKHASH"
  OpCoinbase -> "COINBASE"
  OpTimestamp -> "TIMESTAMP"
  OpNumber -> "NUMBER"
  OpPrevRandao -> "PREVRANDAO"
  OpGaslimit -> "GASLIMIT"
  OpChainid -> "CHAINID"
  OpSelfbalance -> "SELFBALANCE"
  OpBaseFee -> "BASEFEE"
  OpBlobhash -> "BLOBHASH"
  OpBlobBaseFee -> "BLOBBASEFEE"
  OpPop -> "POP"
  OpMcopy -> "MCOPY"
  OpMload -> "MLOAD"
  OpMstore -> "MSTORE"
  OpMstore8 -> "MSTORE8"
  OpSload -> "SLOAD"
  OpSstore -> "SSTORE"
  OpTload -> "TLOAD"
  OpTstore -> "TSTORE"
  OpJump -> "JUMP"
  OpJumpi -> "JUMPI"
  OpPc -> "PC"
  OpMsize -> "MSIZE"
  OpGas -> "GAS"
  OpJumpdest -> "JUMPDEST"
  OpCreate -> "CREATE"
  OpCall -> "CALL"
  OpStaticcall -> "STATICCALL"
  OpCallcode -> "CALLCODE"
  OpReturn -> "RETURN"
  OpDelegatecall -> "DELEGATECALL"
  OpCreate2 -> "CREATE2"
  OpSelfdestruct -> "SELFDESTRUCT"
  OpDup x -> "DUP" ++ show x
  OpSwap x -> "SWAP" ++ show x
  OpLog x -> "LOG" ++ show x
  OpPush0  -> "PUSH0"
  OpPush x -> case x of
    Lit x' -> "PUSH 0x" ++ (showHex x' "")
    _ -> "PUSH " ++ show x
  OpRevert -> "REVERT"
  OpUnknown x -> case x of
    254 -> "INVALID"
    _ -> "UNKNOWN " ++ (showHex x "")

readOp :: Word8 -> [Expr Byte] -> Op
readOp x xs =
  (\n -> Expr.readBytes (into n) (Lit 0) (Expr.fromList $ V.fromList xs)) <$> getOp x

getOp :: Word8 -> GenericOp Word8
getOp x | x >= 0x80 && x <= 0x8f = OpDup (x - 0x80 + 1)
getOp x | x >= 0x90 && x <= 0x9f = OpSwap (x - 0x90 + 1)
getOp x | x >= 0xa0 && x <= 0xa4 = OpLog (x - 0xa0)
getOp x | x >= 0x60 && x <= 0x7f = OpPush (x - 0x60 + 1)
getOp x = case x of
  0x00 -> OpStop
  0x01 -> OpAdd
  0x02 -> OpMul
  0x03 -> OpSub
  0x04 -> OpDiv
  0x05 -> OpSdiv
  0x06 -> OpMod
  0x07 -> OpSmod
  0x08 -> OpAddmod
  0x09 -> OpMulmod
  0x0a -> OpExp
  0x0b -> OpSignextend
  0x10 -> OpLt
  0x11 -> OpGt
  0x12 -> OpSlt
  0x13 -> OpSgt
  0x14 -> OpEq
  0x15 -> OpIszero
  0x16 -> OpAnd
  0x17 -> OpOr
  0x18 -> OpXor
  0x19 -> OpNot
  0x1a -> OpByte
  0x1b -> OpShl
  0x1c -> OpShr
  0x1d -> OpSar
  0x20 -> OpSha3
  0x30 -> OpAddress
  0x31 -> OpBalance
  0x32 -> OpOrigin
  0x33 -> OpCaller
  0x34 -> OpCallvalue
  0x35 -> OpCalldataload
  0x36 -> OpCalldatasize
  0x37 -> OpCalldatacopy
  0x38 -> OpCodesize
  0x39 -> OpCodecopy
  0x3a -> OpGasprice
  0x3b -> OpExtcodesize
  0x3c -> OpExtcodecopy
  0x3d -> OpReturndatasize
  0x3e -> OpReturndatacopy
  0x3f -> OpExtcodehash
  0x40 -> OpBlockhash
  0x41 -> OpCoinbase
  0x42 -> OpTimestamp
  0x43 -> OpNumber
  0x44 -> OpPrevRandao
  0x45 -> OpGaslimit
  0x46 -> OpChainid
  0x47 -> OpSelfbalance
  0x48 -> OpBaseFee
  0x49 -> OpBlobhash
  0x4A -> OpBlobBaseFee
  0x50 -> OpPop
  0x51 -> OpMload
  0x52 -> OpMstore
  0x53 -> OpMstore8
  0x54 -> OpSload
  0x55 -> OpSstore
  0x56 -> OpJump
  0x57 -> OpJumpi
  0x58 -> OpPc
  0x59 -> OpMsize
  0x5a -> OpGas
  0x5b -> OpJumpdest
  0x5c -> OpTload
  0x5d -> OpTstore
  0x5e -> OpMcopy
  0x5f -> OpPush0
  0xf0 -> OpCreate
  0xf1 -> OpCall
  0xf2 -> OpCallcode
  0xf3 -> OpReturn
  0xf4 -> OpDelegatecall
  0xf5 -> OpCreate2
  0xfd -> OpRevert
  0xfa -> OpStaticcall
  0xff -> OpSelfdestruct
  _    -> OpUnknown x
