{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module EVM where

import Prelude hiding (exponent)

import Optics.Core
import Optics.State
import Optics.State.Operators
import Optics.Zoom
import Optics.Operators.Unsafe

import EVM.ABI
import EVM.Expr (readStorage, writeStorage, readByte, readWord, writeWord,
  writeByte, bufLength, indexWord, readBytes, copySlice, wordToAddr)
import EVM.Expr qualified as Expr
import EVM.FeeSchedule (FeeSchedule (..))
import EVM.Op
import EVM.Precompiled qualified
import EVM.Solidity
import EVM.Types
import EVM.Types qualified as Expr (Expr(Gas))
import EVM.Sign qualified
import EVM.Concrete qualified as Concrete
import EVM.CheatsTH

import Control.Monad (unless, when)
import Control.Monad.ST (ST)
import Control.Monad.State.Strict (MonadState, State, get, gets, lift, modify', put)
import Data.Bits (FiniteBits, countLeadingZeros, finiteBitSize)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy qualified as LS
import Data.ByteString.Char8 qualified as Char8
import Data.DoubleWord (Int256, Word256)
import Data.Either (partitionEithers)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (toList)
import Data.List (find, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Set (insert, member, fromList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tree
import Data.Tree.Zipper qualified as Zipper
import Data.Typeable
import Data.Vector qualified as V
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SV
import Data.Vector.Unboxed qualified as VUnboxed
import Data.Vector.Unboxed.Mutable qualified as VUnboxed.Mutable
import Data.Word (Word8, Word32, Word64)
import Text.Read (readMaybe)
import Witch (into, tryFrom, unsafeInto, tryInto)

import Crypto.Hash (Digest, SHA256, RIPEMD160)
import Crypto.Hash qualified as Crypto
import Crypto.Number.ModArithmetic (expFast)

blankState :: VMOps t => ST s (FrameState t s)
blankState = do
  memory <- ConcreteMemory <$> VUnboxed.Mutable.new 0
  pure $ FrameState
    { contract     = LitAddr 0
    , codeContract = LitAddr 0
    , code         = RuntimeCode (ConcreteRuntimeCode "")
    , pc           = 0
    , stack        = mempty
    , memory
    , memorySize   = 0
    , calldata     = mempty
    , callvalue    = Lit 0
    , caller       = LitAddr 0
    , gas          = initialGas
    , returndata   = mempty
    , static       = False
    , overrideCaller = Nothing
    , resetCaller  = False
    }

-- | An "external" view of a contract's bytecode, appropriate for
-- e.g. @EXTCODEHASH@.
bytecode :: Getter Contract (Maybe (Expr Buf))
bytecode = #code % to f
  where f (InitCode _ _) = Just mempty
        f (RuntimeCode (ConcreteRuntimeCode bs)) = Just $ ConcreteBuf bs
        f (RuntimeCode (SymbolicRuntimeCode ops)) = Just $ Expr.fromList ops
        f (UnknownCode _) = Nothing

-- * Data accessors

currentContract :: VM t s -> Maybe Contract
currentContract vm =
  Map.lookup vm.state.codeContract vm.env.contracts

-- * Data constructors
--
makeVm :: VMOps t => VMOpts t -> ST s (VM t s)
makeVm o = do
  let txaccessList = o.txAccessList
      txorigin = o.origin
      txtoAddr = o.address
      initialAccessedAddrs = fromList $
           [txorigin, txtoAddr, o.coinbase]
        ++ (fmap LitAddr [1..9])
        ++ (Map.keys txaccessList)
      initialAccessedStorageKeys = fromList $ foldMap (uncurry (map . (,))) (Map.toList txaccessList)
      touched = if o.create then [txorigin] else [txorigin, txtoAddr]
  memory <- ConcreteMemory <$> VUnboxed.Mutable.new 0
  pure $ setEIP4788Storage o $ VM
    { result = Nothing
    , frames = mempty
    , tx = TxState
      { gasprice = o.gasprice
      , gaslimit = o.gaslimit
      , priorityFee = o.priorityFee
      , origin = txorigin
      , toAddr = txtoAddr
      , value = o.value
      , subState = SubState mempty touched initialAccessedAddrs initialAccessedStorageKeys mempty mempty
      , isCreate = o.create
      , txReversion = Map.fromList ((o.address,o.contract):o.otherContracts)
      }
    , logs = []
    , traces = Zipper.fromForest []
    , block = block
    , state = FrameState
      { pc = 0
      , stack = mempty
      , memory
      , memorySize = 0
      , code = o.contract.code
      , contract = o.address
      , codeContract = o.address
      , calldata = fst o.calldata
      , callvalue = o.value
      , caller = o.caller
      , gas = o.gas
      , returndata = mempty
      , static = False
      , overrideCaller = Nothing
      , resetCaller = False
      }
    , env = env
    , cache = cache
    , burned = initialGas
    , constraints = snd o.calldata
    , iterations = mempty
    , config = RuntimeConfig
      { allowFFI = o.allowFFI
      , baseState = o.baseState
      }
    , forks = Seq.singleton (ForkState env block cache "")
    , currentFork = 0
    , labels = mempty
    , osEnv = mempty
    }
    where
    env = Env
      { chainId = o.chainId
      , contracts = Map.fromList ((o.address,o.contract):o.otherContracts)
      , freshAddresses = o.freshAddresses
      , freshGasVals = 0
      }
    block = Block
      { coinbase = o.coinbase
      , timestamp = o.timestamp
      , number = o.number
      , prevRandao = o.prevRandao
      , maxCodeSize = o.maxCodeSize
      , gaslimit = o.blockGaslimit
      , baseFee = o.baseFee
      , schedule = o.schedule
      }
    cache = Cache mempty mempty

-- https://eips.ethereum.org/EIPS/eip-4788
setEIP4788Storage :: VMOpts t -> VM t s -> VM t s
setEIP4788Storage o vm = do
  let beaconRootsAddress = LitAddr 0x000F3df6D732807Ef1319fB7B8bB8522d0Beac02
  case Map.lookup beaconRootsAddress vm.env.contracts of
    Just beaconRootsContract -> do
      -- https://eips.ethereum.org/EIPS/eip-4788#pseudocode
      -- https://eips.ethereum.org/EIPS/eip-4788#block-processing
      -- > Clients may decide to omit an explicit EVM call and directly set the
      -- > storage values. Note: While this is a valid optimization for Ethereum
      -- > mainnet, it could be problematic on non-mainnet situations in case
      -- > a different contract is used.
      let
        historyBufferLength = 8191
        timestampIdx = Expr.mod o.timestamp (Lit historyBufferLength)
        rootIdx = Expr.add timestampIdx (Lit historyBufferLength)
        storage =
          Expr.writeStorage timestampIdx o.timestamp .
          Expr.writeStorage rootIdx (Lit o.beaconRoot) $
          beaconRootsContract.storage
      vm {
        env = vm.env {
          contracts = Map.insert beaconRootsAddress
                                 (beaconRootsContract { storage } :: Contract)
                                 vm.env.contracts
        }
      }
    Nothing -> vm

-- | Initialize an abstract contract with unknown code
unknownContract :: Expr EAddr -> Contract
unknownContract addr = Contract
  { code        = UnknownCode addr
  , storage     = AbstractStore addr Nothing
  , tStorage    = AbstractStore addr Nothing
  , origStorage = AbstractStore addr Nothing
  , balance     = Balance addr
  , nonce       = Nothing
  , codehash    = hashcode (UnknownCode addr)
  , opIxMap     = mempty
  , codeOps     = mempty
  , external    = False
  }

-- | Initialize an abstract contract with known code
abstractContract :: ContractCode -> Expr EAddr -> Contract
abstractContract code addr = Contract
  { code        = code
  , storage     = AbstractStore addr Nothing
  , tStorage    = AbstractStore addr Nothing
  , origStorage = AbstractStore addr Nothing
  , balance     = Balance addr
  , nonce       = if isCreation code then Just 1 else Just 0
  , codehash    = hashcode code
  , opIxMap     = mkOpIxMap code
  , codeOps     = mkCodeOps code
  , external    = False
  }

-- | Initialize an empty contract without code
emptyContract :: Contract
emptyContract = initialContract (RuntimeCode (ConcreteRuntimeCode ""))

-- | Initialize empty contract with given code
initialContract :: ContractCode -> Contract
initialContract code = Contract
  { code        = code
  , storage     = ConcreteStore mempty
  , tStorage    = ConcreteStore mempty
  , origStorage = ConcreteStore mempty
  , balance     = Lit 0
  , nonce       = if isCreation code then Just 1 else Just 0
  , codehash    = hashcode code
  , opIxMap     = mkOpIxMap code
  , codeOps     = mkCodeOps code
  , external    = False
  }

isCreation :: ContractCode -> Bool
isCreation = \case
  InitCode _ _  -> True
  RuntimeCode _ -> False
  UnknownCode _ -> False

-- * Opcode dispatch (exec1)

-- | Update program counter
next :: (?op :: Word8) => EVM t s ()
next = modifying (#state % #pc) (+ (opSize ?op))

getOpW8 :: forall (t :: VMType) s . FrameState t s -> Word8
getOpW8 state = case state.code of
      UnknownCode _ -> internalError "Cannot execute unknown code"
      InitCode conc _ -> BS.index conc state.pc
      RuntimeCode (ConcreteRuntimeCode bs) -> BS.index bs state.pc
      RuntimeCode (SymbolicRuntimeCode ops) ->
        fromMaybe (internalError "could not analyze symbolic code") $
          maybeLitByte $ ops V.! state.pc

getOpName :: forall (t :: VMType) s . FrameState t s -> [Char]
getOpName state = intToOpName $ fromEnum $ getOpW8 state

-- | Executes the EVM one step
exec1 :: forall (t :: VMType) s. VMOps t => EVM t s ()
exec1 = do
  vm <- get

  let
    -- Convenient aliases
    stk  = vm.state.stack
    self = vm.state.contract
    this = fromMaybe (internalError "state contract") (Map.lookup self vm.env.contracts)

    fees@FeeSchedule {..} = vm.block.schedule

    doStop = finishFrame (FrameReturned mempty)

    litSelf = maybeLitAddr self

  if isJust litSelf && (fromJust litSelf) > 0x0 && (fromJust litSelf) <= 0xa then do
    -- call to precompile
    let ?op = 0x00 -- dummy value
    let calldatasize = bufLength vm.state.calldata
    copyBytesToMemory vm.state.calldata calldatasize (Lit 0) (Lit 0)
    executePrecompile (fromJust litSelf) vm.state.gas (Lit 0) calldatasize (Lit 0) (Lit 0) []
    vmx <- get
    case vmx.state.stack of
      x:_ -> case x of
        Lit 0 ->
          fetchAccount self $ \_ -> do
            touchAccount self
            vmError PrecompileFailure
        Lit _ ->
          fetchAccount self $ \_ -> do
            touchAccount self
            out <- use (#state % #returndata)
            finishFrame (FrameReturned out)
        e -> partial $
               UnexpectedSymbolicArg vmx.state.pc (getOpName vmx.state) "precompile returned a symbolic value" (wrap [e])
      _ ->
        underrun

  else if vm.state.pc >= opslen vm.state.code
    then doStop

    else do
      let ?op = getOpW8 vm.state
      let opName = getOpName vm.state

      case getOp (?op) of

        OpPush0 -> do
          limitStack 1 $
            burn g_base $ do
              next
              pushSym (Lit 0)

        OpPush n' -> do
          let n = into n'
              !xs = case vm.state.code of
                UnknownCode _ -> internalError "Cannot execute unknown code"
                InitCode conc _ -> Lit $ word $ padRight n $ BS.take n (BS.drop (1 + vm.state.pc) conc)
                RuntimeCode (ConcreteRuntimeCode bs) -> Lit $ word $ BS.take n $ BS.drop (1 + vm.state.pc) bs
                RuntimeCode (SymbolicRuntimeCode ops) ->
                  let bytes = V.take n $ V.drop (1 + vm.state.pc) ops
                  in readWord (Lit 0) $ Expr.fromList $ padLeft' 32 bytes
          limitStack 1 $
            burn g_verylow $ do
              next
              pushSym xs

        OpDup i ->
          case preview (ix (into i - 1)) stk of
            Nothing -> underrun
            Just y ->
              limitStack 1 $
                burn g_verylow $ do
                  next
                  pushSym y

        OpSwap i ->
          if length stk < (into i) + 1
            then underrun
            else
              burn g_verylow $ do
                next
                zoom (#state % #stack) $ do
                  assign (ix 0) (stk ^?! ix (into i))
                  assign (ix (into i)) (stk ^?! ix 0)

        OpLog n ->
          notStatic $
          case stk of
            (xOffset:xSize:xs) ->
              if length xs < (into n)
              then underrun
              else do
                bytes <- readMemory xOffset xSize
                let (topics, xs') = splitAt (into n) xs
                    logs'         = (LogEntry (WAddr self) bytes topics) : vm.logs
                burnLog xSize n $
                  accessMemoryRange xOffset xSize $ do
                    traceTopLog logs'
                    next
                    assign (#state % #stack) xs'
                    assign #logs logs'
            _ ->
              underrun

        OpStop -> doStop

        OpAdd -> stackOp2 g_verylow Expr.add
        OpMul -> stackOp2 g_low Expr.mul
        OpSub -> stackOp2 g_verylow Expr.sub

        OpDiv -> stackOp2 g_low Expr.div

        OpSdiv -> stackOp2 g_low Expr.sdiv

        OpMod -> stackOp2 g_low Expr.mod

        OpSmod -> stackOp2 g_low Expr.smod
        OpAddmod -> stackOp3 g_mid Expr.addmod
        OpMulmod -> stackOp3 g_mid Expr.mulmod

        OpLt -> stackOp2 g_verylow Expr.lt
        OpGt -> stackOp2 g_verylow Expr.gt
        OpSlt -> stackOp2 g_verylow Expr.slt
        OpSgt -> stackOp2 g_verylow Expr.sgt

        OpEq -> stackOp2 g_verylow Expr.eq
        OpIszero -> stackOp1 g_verylow Expr.iszero

        OpAnd -> stackOp2 g_verylow Expr.and
        OpOr -> stackOp2 g_verylow Expr.or
        OpXor -> stackOp2 g_verylow Expr.xor
        OpNot -> stackOp1 g_verylow Expr.not

        OpByte -> stackOp2 g_verylow (\i w -> Expr.padByte $ Expr.indexWord i w)

        OpShl -> stackOp2 g_verylow Expr.shl
        OpShr -> stackOp2 g_verylow Expr.shr
        OpSar -> stackOp2 g_verylow Expr.sar

        -- more accurately referred to as KECCAK
        OpSha3 ->
          case stk of
            xOffset:xSize:xs ->
              burnSha3 xSize $
                accessMemoryRange xOffset xSize $ do
                  hash <- readMemory xOffset xSize >>= \case
                    orig@(ConcreteBuf bs) ->
                      whenSymbolicElse
                        (pure $ Keccak orig)
                        (pure $ Lit (keccak' bs))
                    buf -> pure $ Keccak buf
                  next
                  assign (#state % #stack) (hash : xs)
            _ -> underrun

        OpAddress ->
          limitStack 1 $
            burn g_base (next >> pushAddr self)

        OpBalance ->
          case stk of
            x:xs -> forceAddr x "BALANCE" $ \a ->
              accessAndBurn a $
                fetchAccount a $ \c -> do
                  next
                  assign (#state % #stack) xs
                  pushSym c.balance
            [] ->
              underrun

        OpOrigin ->
          limitStack 1 . burn g_base $
            next >> pushAddr vm.tx.origin

        OpCaller ->
          limitStack 1 . burn g_base $
            next >> pushAddr vm.state.caller

        OpCallvalue ->
          limitStack 1 . burn g_base $
            next >> pushSym vm.state.callvalue

        OpCalldataload -> stackOp1 g_verylow $
          \ind -> Expr.readWord ind vm.state.calldata

        OpCalldatasize ->
          limitStack 1 . burn g_base $
            next >> pushSym (bufLength vm.state.calldata)

        OpCalldatacopy ->
          case stk of
            xTo:xFrom:xSize:xs ->
              burnCalldatacopy xSize $
                accessMemoryRange xTo xSize $ do
                  next
                  assign (#state % #stack) xs
                  copyBytesToMemory vm.state.calldata xSize xFrom xTo
            _ -> underrun

        OpCodesize ->
          limitStack 1 . burn g_base $
            next >> pushSym (codelen vm.state.code)

        OpCodecopy ->
          case stk of
            memOffset:codeOffset:n:xs ->
              burnCodecopy n $ do
                accessMemoryRange memOffset n $ do
                  next
                  assign (#state % #stack) xs
                  case toBuf vm.state.code of
                    Just b -> copyBytesToMemory b n codeOffset memOffset
                    Nothing -> internalError "Cannot produce a buffer from UnknownCode"
            _ -> underrun

        OpGasprice ->
          limitStack 1 . burn g_base $
            next >> push vm.tx.gasprice

        OpExtcodesize ->
          case stk of
            x':xs -> forceAddr x' "EXTCODESIZE" $ \x -> do
              let impl = accessAndBurn x $
                           fetchAccount x $ \c -> do
                             next
                             assign (#state % #stack) xs
                             case view bytecode c of
                               Just b -> pushSym (bufLength b)
                               Nothing -> pushSym $ CodeSize x
              case x of
                a@(LitAddr _) -> if a == cheatCode
                  then do
                    next
                    assign (#state % #stack) xs
                    pushSym (Lit 1)
                  else impl
                _ -> impl
            [] ->
              underrun

        OpExtcodecopy ->
          case stk of
            extAccount':memOffset:codeOffset:codeSize:xs ->
              forceAddr extAccount' "EXTCODECOPY" $ \extAccount -> do
                burnExtcodecopy extAccount codeSize $
                  accessMemoryRange memOffset codeSize $
                    fetchAccount extAccount $ \c -> do
                      next
                      assign (#state % #stack) xs
                      case view bytecode c of
                        Just b -> copyBytesToMemory b codeSize codeOffset memOffset
                        Nothing -> do
                          pc <- use (#state % #pc)
                          partial $ UnexpectedSymbolicArg pc opName "Cannot copy from unknown code at" (wrap [extAccount])
            _ -> underrun

        OpReturndatasize ->
          limitStack 1 . burn g_base $
            next >> pushSym (bufLength vm.state.returndata)

        OpReturndatacopy ->
          case stk of
            xTo:xFrom:xSize:xs ->
              burnReturndatacopy xSize $
                accessMemoryRange xTo xSize $ do
                  next
                  assign (#state % #stack) xs

                  let jump True = vmError ReturnDataOutOfBounds
                      jump False = copyBytesToMemory vm.state.returndata xSize xFrom xTo

                  case (xFrom, bufLength vm.state.returndata, xSize) of
                    (Lit f, Lit l, Lit sz) ->
                      jump $ l < f + sz || f + sz < f
                    _ -> do
                      let oob = Expr.lt (bufLength vm.state.returndata) (Expr.add xFrom xSize)
                          overflow = Expr.lt (Expr.add xFrom xSize) (xFrom)
                      branch (Expr.or oob overflow) jump
            _ -> underrun

        OpExtcodehash ->
          case stk of
            x':xs -> forceAddr x' "EXTCODEHASH" $ \x ->
              accessAndBurn x $ do
                next
                assign (#state % #stack) xs
                fetchAccount x $ \c ->
                   if accountEmpty c
                     then push (W256 0)
                     else case view bytecode c of
                            Just b -> pushSym $ keccak b
                            Nothing -> pushSym $ CodeHash x
            [] ->
              underrun

        OpBlockhash -> do
          -- We adopt the fake block hash scheme of the VMTests,
          -- so that blockhash(i) is the hash of i as decimal ASCII.
          stackOp1 g_blockhash $ \case
            Lit i -> if i + 256 < vm.block.number || i >= vm.block.number
                     then Lit 0
                     else (into i :: Integer) & show & Char8.pack & keccak' & Lit
            i -> BlockHash i

        OpCoinbase ->
          limitStack 1 . burn g_base $
            next >> pushAddr vm.block.coinbase

        OpTimestamp ->
          limitStack 1 . burn g_base $
            next >> pushSym vm.block.timestamp

        OpNumber ->
          limitStack 1 . burn g_base $
            next >> push vm.block.number

        OpPrevRandao -> do
          limitStack 1 . burn g_base $
            next >> push vm.block.prevRandao

        OpGaslimit ->
          limitStack 1 . burn g_base $
            next >> push (into vm.block.gaslimit)

        OpChainid ->
          limitStack 1 . burn g_base $
            next >> push vm.env.chainId

        OpSelfbalance ->
          limitStack 1 . burn g_low $
            next >> pushSym this.balance

        OpBaseFee ->
          limitStack 1 . burn g_base $
            next >> push vm.block.baseFee

        OpBlobhash ->
          stackOp1 g_verylow $ \_ -> Lit 0

        OpBlobBaseFee ->
          limitStack 1 . burn g_base $
            next >> push 0

        OpPop ->
          case stk of
            _:xs -> burn g_base (next >> assign (#state % #stack) xs)
            _    -> underrun

        OpMload ->
          case stk of
            x:xs ->
              burn g_verylow $
                accessMemoryWord x $ do
                  next
                  buf <- readMemory x (Lit 32)
                  let w = Expr.readWordFromBytes (Lit 0) buf
                  assign (#state % #stack) (w : xs)
            _ -> underrun


        OpMcopy ->
          case stk of
            dstOff:srcOff:sz:xs ->  do
              case sz of
                Lit sz' -> do
                  let words_copied = (sz' + 31) `div` 32
                  let g_mcopy = 3*words_copied -- memory access cost is part of accessMemoryRange
                  burn (g_verylow + (unsafeInto g_mcopy)) $
                    accessMemoryRange srcOff sz $ accessMemoryRange dstOff sz $ do
                      next
                      mcopy sz srcOff dstOff
                _ -> do
                  -- symbolic, ignore gas
                  next
                  mcopy sz srcOff dstOff
              assign (#state % #stack) xs
            _ -> underrun
            where
            mcopy sz srcOff dstOff = do
                  m <- gets (.state.memory)
                  case m of
                    ConcreteMemory mem -> do
                      buf <- freezeMemory mem
                      copyBytesToMemory buf sz srcOff dstOff
                    SymbolicMemory mem -> do
                      assign (#state % #memory) (SymbolicMemory $ copySlice srcOff dstOff sz mem mem)

        OpMstore ->
          case stk of
            x:y:xs ->
              burn g_verylow $
                accessMemoryWord x $ do
                  next
                  gets (.state.memory) >>= \case
                    ConcreteMemory mem -> do
                      case y of
                        Lit w ->
                          copyBytesToMemory (ConcreteBuf (word256Bytes w)) (Lit 32) (Lit 0) x
                        _ -> do
                          -- copy out and move to symbolic memory
                          buf <- freezeMemory mem
                          assign (#state % #memory) (SymbolicMemory $ writeWord x y buf)
                    SymbolicMemory mem ->
                      assign (#state % #memory) (SymbolicMemory $ writeWord x y mem)
                  assign (#state % #stack) xs
            _ -> underrun

        OpMstore8 ->
          case stk of
            x:y:xs ->
              burn g_verylow $
                accessMemoryRange x (Lit 1) $ do
                  let yByte = indexWord (Lit 31) y
                  next
                  gets (.state.memory) >>= \case
                    ConcreteMemory mem -> do
                      case yByte of
                        LitByte byte ->
                          copyBytesToMemory (ConcreteBuf (BS.pack [byte])) (Lit 1) (Lit 0) x
                        _ -> do
                          -- copy out and move to symbolic memory
                          buf <- freezeMemory mem
                          assign (#state % #memory) (SymbolicMemory $ writeByte x yByte buf)
                    SymbolicMemory mem ->
                      assign (#state % #memory) (SymbolicMemory $ writeByte x yByte mem)

                  assign (#state % #stack) xs
            _ -> underrun

        OpSload ->
          case stk of
            x:xs -> do
              acc <- accessStorageForGas self x
              let cost = if acc then g_warm_storage_read else g_cold_sload
              burn cost $
                accessStorage self x $ \y -> do
                  next
                  assign (#state % #stack) (y:xs)
            _ -> underrun

        OpSstore ->
          notStatic $
          case stk of
            x:new:xs ->
              accessStorage self x $ \current -> do
                ensureGas g_callstipend $ do
                  let
                    original =
                      case Expr.concKeccakSimpExpr $ SLoad x this.origStorage of
                        Lit v -> v
                        _ -> 0
                    storage_cost =
                      case (maybeLitWord current, maybeLitWord new) of
                        (Just current', Just new') ->
                           if (current' == new') then g_sload
                           else if (current' == original) && (original == 0) then g_sset
                           else if (current' == original) then g_sreset
                           else g_sload

                        -- if any of the arguments are symbolic,
                        -- assume worst case scenario
                        _-> g_sset

                  acc <- accessStorageForGas self x
                  let cold_storage_cost = if acc then 0 else g_cold_sload
                  burn (storage_cost + cold_storage_cost) $ do
                    next
                    assign (#state % #stack) xs
                    modifying (#env % #contracts % ix self % #storage) (writeStorage x new)

                    case (maybeLitWord current, maybeLitWord new) of
                       (Just current', Just new') ->
                          unless (current' == new') $
                            if current' == original then
                              when (original /= 0 && new' == 0) $
                                refund (g_sreset + g_access_list_storage_key)
                            else do
                              when (original /= 0) $
                                if current' == 0
                                then unRefund (g_sreset + g_access_list_storage_key)
                                else when (new' == 0) $ refund (g_sreset + g_access_list_storage_key)
                              when (original == new') $
                                if original == 0
                                then refund (g_sset - g_sload)
                                else refund (g_sreset - g_sload)
                       -- if any of the arguments are symbolic,
                       -- don't change the refund counter
                       _ -> noop
            _ -> underrun

        OpTload ->
          case stk of
            x:xs -> do
              burn g_warm_storage_read $
                accessTStorage self x $ \y -> do
                  next
                  assign (#state % #stack) (y:xs)
            _ -> underrun

        OpTstore ->
          notStatic $
          case stk of
            x:new:xs ->
              burn g_sload $ do
                next
                modifying (#env % #contracts % ix self % #tStorage) (writeStorage x new)
                assign (#state % #stack) xs
            _ -> underrun

        OpJump ->
          case stk of
            x:xs ->
              burn g_mid $ forceConcrete x "JUMP: symbolic jumpdest" $ \x' ->
                case tryInto x' of
                  Left _ -> vmError BadJumpDestination
                  Right i -> checkJump i xs
            _ -> underrun

        OpJumpi ->
          case stk of
            x:y:xs -> forceConcrete x "JUMPI: symbolic jumpdest" $ \x' ->
              burn g_high $
                let jump :: Bool -> EVM t s ()
                    jump False = assign (#state % #stack) xs >> next
                    jump _    = case tryInto x' of
                      Left _ -> vmError BadJumpDestination
                      Right i -> checkJump i xs
                in branch y jump
            _ -> underrun

        OpPc ->
          limitStack 1 . burn g_base $
            next >> push (unsafeInto vm.state.pc)

        OpMsize ->
          limitStack 1 . burn g_base $
            next >> push (into vm.state.memorySize)

        OpGas ->
          limitStack 1 . burn g_base $
            next >> pushGas

        OpJumpdest -> burn g_jumpdest next

        OpExp ->
          -- NOTE: this can be done symbolically using unrolling like this:
          --       https://hackage.haskell.org/package/sbv-9.0/docs/src/Data.SBV.Core.Model.html#.%5E
          --       However, it requires symbolic gas, since the gas depends on the exponent
          case stk of
            base:exponent:xs ->
              burnExp exponent $ do
                next
                (#state % #stack) .= Expr.exp base exponent : xs
            _ -> underrun

        OpSignextend -> stackOp2 g_low Expr.sex

        OpCreate ->
          notStatic $
          case stk of
            xValue:xOffset:xSize:xs ->
              accessMemoryRange xOffset xSize $ do
                availableGas <- use (#state % #gas)
                let (cost, gas') = costOfCreate fees availableGas xSize False

                -- handle `prank`
                let from' = fromMaybe self vm.state.overrideCaller
                resetCaller <- use (#state % #resetCaller)
                when resetCaller $ do
                  assign (#state % #overrideCaller) Nothing
                  assign (#state % #resetCaller) False

                newAddr <- createAddress from' this.nonce
                _ <- accessAccountForGas newAddr
                burn' cost $ do
                  initCode <- readMemory xOffset xSize
                  create from' this xSize gas' xValue xs newAddr initCode
            _ -> underrun

        OpCall ->
          case stk of
            xGas:xTo':xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs ->
              branch (Expr.gt xValue (Lit 0)) $ \gt0 -> do
                (if gt0 then notStatic else id) $
                  forceAddr xTo' "unable to determine a call target" $ \xTo ->
                    case gasTryFrom xGas of
                      Left _ -> vmError IllegalOverflow
                      Right gas -> do
                        overrideC <- use $ #state % #overrideCaller
                        delegateCall this gas xTo xTo xValue xInOffset xInSize xOutOffset xOutSize xs $
                          \callee -> do
                            let from' = fromMaybe self overrideC
                            zoom #state $ do
                              assign #callvalue xValue
                              assign #caller from'
                              assign #contract callee
                            touchAccount from'
                            touchAccount callee
                            transfer from' callee xValue
            _ ->
              underrun

        OpCallcode ->
          case stk of
            xGas:xTo':xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs ->
              forceAddr xTo' "unable to determine a call target" $ \xTo ->
                case gasTryFrom xGas of
                  Left _ -> vmError IllegalOverflow
                  Right gas -> do
                    overrideC <- use $ #state % #overrideCaller
                    delegateCall this gas xTo self xValue xInOffset xInSize xOutOffset xOutSize xs $ \_ -> do
                      zoom #state $ do
                        assign #callvalue xValue
                        assign #caller $ fromMaybe self overrideC
                      touchAccount self
            _ ->
              underrun

        OpReturn ->
          case stk of
            xOffset:xSize:_ ->
              accessMemoryRange xOffset xSize $ do
                output <- readMemory xOffset xSize
                let
                  codesize = fromMaybe (internalError "processing opcode RETURN. Cannot return dynamically sized abstract data")
                               . maybeLitWord . bufLength $ output
                  maxsize = vm.block.maxCodeSize
                  creation = case vm.frames of
                    [] -> vm.tx.isCreate
                    frame:_ -> case frame.context of
                       CreationContext {} -> True
                       CallContext {} -> False
                if creation
                then
                  if codesize > maxsize
                  then
                    finishFrame (FrameErrored (MaxCodeSizeExceeded maxsize codesize))
                  else do
                    let frameReturned = burn (g_codedeposit * unsafeInto codesize) $
                                          finishFrame (FrameReturned output)
                        frameErrored = finishFrame $ FrameErrored InvalidFormat
                    case readByte (Lit 0) output of
                      LitByte 0xef -> frameErrored
                      LitByte _ -> frameReturned
                      y -> branch (Expr.eqByte y (LitByte 0xef)) $ \case
                          True -> frameErrored
                          False -> frameReturned
                else
                   finishFrame (FrameReturned output)
            _ -> underrun

        OpDelegatecall ->
          case stk of
            xGas:xTo:xInOffset:xInSize:xOutOffset:xOutSize:xs ->
              case wordToAddr xTo of
                Nothing -> do
                  loc <- codeloc
                  let msg = "Unable to determine a call target"
                  partial $ UnexpectedSymbolicArg (snd loc) opName msg [SomeExpr xTo]
                Just xTo' ->
                  case gasTryFrom xGas of
                    Left _ -> vmError IllegalOverflow
                    Right gas ->
                      delegateCall this gas xTo' self (Lit 0) xInOffset xInSize xOutOffset xOutSize xs $
                        \_ -> touchAccount self
            _ -> underrun

        OpCreate2 -> notStatic $
          case stk of
            xValue:xOffset:xSize:xSalt':xs ->
              forceConcrete xSalt' "CREATE2" $ \(xSalt) ->
                accessMemoryRange xOffset xSize $ do
                  availableGas <- use (#state % #gas)
                  buf <- readMemory xOffset xSize
                  forceConcreteBuf buf "CREATE2" $
                    \initCode -> do
                      -- handle `prank`
                      let from' = fromMaybe self vm.state.overrideCaller
                      resetCaller <- use (#state % #resetCaller)
                      when resetCaller $ do
                        assign (#state % #overrideCaller) Nothing
                        assign (#state % #resetCaller) False

                      let (cost, gas') = costOfCreate fees availableGas xSize True
                      newAddr <- create2Address self xSalt initCode
                      _ <- accessAccountForGas newAddr
                      burn' cost $
                        create from' this xSize gas' xValue xs newAddr (ConcreteBuf initCode)
            _ -> underrun

        OpStaticcall ->
          case stk of
            xGas:xTo:xInOffset:xInSize:xOutOffset:xOutSize:xs ->
              case wordToAddr xTo of
                Nothing -> do
                  loc <- codeloc
                  let msg = "Unable to determine a call target"
                  partial $ UnexpectedSymbolicArg (snd loc) opName msg [SomeExpr xTo]
                Just xTo' ->
                  case gasTryFrom xGas of
                    Left _ -> vmError IllegalOverflow
                    Right gas -> do
                      overrideC <- use $ #state % #overrideCaller
                      delegateCall this gas xTo' xTo' (Lit 0) xInOffset xInSize xOutOffset xOutSize xs $
                        \callee -> do
                          zoom #state $ do
                            assign #callvalue (Lit 0)
                            assign #caller $ fromMaybe self overrideC
                            assign #contract callee
                            assign #static True
                          touchAccount self
                          touchAccount callee
            _ ->
              underrun

        OpSelfdestruct ->
          notStatic $
          case stk of
            [] -> underrun
            (xTo':_) -> forceAddr xTo' "SELFDESTRUCT" $ \case
              xTo@(LitAddr _) -> do
                cc <- gets (.tx.subState.createdContracts)
                let createdThisTr = self `member` cc
                acc <- accessAccountForGas xTo
                let cost = if acc then 0 else g_cold_account_access
                    funds = this.balance
                    recipientExists = accountExists xTo vm
                branch (Expr.iszero $ Expr.eq funds (Lit 0)) $ \hasFunds -> do
                  let c_new = if (not recipientExists) && hasFunds
                              then g_selfdestruct_newaccount
                              else 0
                  burn (g_selfdestruct + c_new + cost) $ do
                    when (createdThisTr || isCreation this.code) $ do
                      selfdestruct self
                    touchAccount xTo

                    if hasFunds
                    then fetchAccount xTo $ \_ -> do
                      when (createdThisTr || xTo /= self) $ do
                        #env % #contracts % ix xTo % #balance %= (Expr.add funds)
                        assign (#env % #contracts % ix self % #balance) (Lit 0)
                      doStop
                    else
                      doStop
              a -> do
                pc <- use (#state % #pc)
                partial $ UnexpectedSymbolicArg pc opName "trying to self destruct to a symbolic address" (wrap [a])

        OpRevert ->
          case stk of
            xOffset:xSize:_ ->
              accessMemoryRange xOffset xSize $ do
                output <- readMemory xOffset xSize
                finishFrame (FrameReverted output)
            _ -> underrun

        OpUnknown xxx ->
          vmError $ UnrecognizedOpcode xxx

transfer :: VMOps t => Expr EAddr -> Expr EAddr -> Expr EWord -> EVM t s ()
transfer _ _ (Lit 0) = pure ()
transfer src dst val = do
  sb <- preuse $ #env % #contracts % ix src % #balance
  db <- preuse $ #env % #contracts % ix dst % #balance
  baseState <- use (#config % #baseState)
  let mkc = case baseState of
              AbstractBase -> unknownContract
              EmptyBase -> const emptyContract
  case (sb, db) of
    -- both sender and recipient in state
    (Just srcBal, Just _) ->
      branch (Expr.gt val srcBal) $ \case
        True -> vmError $ BalanceTooLow val srcBal
        False -> do
          (#env % #contracts % ix src % #balance) %= (`Expr.sub` val)
          (#env % #contracts % ix dst % #balance) %= (`Expr.add` val)
    -- sender not in state
    (Nothing, Just _) -> do
      case src of
        LitAddr _ -> do
          (#env % #contracts) %= (Map.insert src (mkc src))
          transfer src dst val
        SymAddr _ -> do
          pc <- use (#state % #pc)
          state <- use #state
          partial $ UnexpectedSymbolicArg pc (getOpName state) "Attempting to transfer eth from a symbolic address that is not present in the state" (wrap [src])
        GVar _ -> internalError "Unexpected GVar"
    -- recipient not in state
    (_ , Nothing) -> do
      case dst of
        LitAddr _ -> do
          (#env % #contracts) %= (Map.insert dst (mkc dst))
          transfer src dst val
        SymAddr _ -> do
          pc <- use (#state % #pc)
          state <- use #state
          partial $ UnexpectedSymbolicArg pc (getOpName state) "Attempting to transfer eth to a symbolic address that is not present in the state" (wrap [dst])
        GVar _ -> internalError "Unexpected GVar"

-- | Checks a *CALL for failure; OOG, too many callframes, memory access etc.
callChecks
  :: forall (t :: VMType) s. (?op :: Word8, VMOps t)
  => Contract
  -> Gas t
  -> Expr EAddr
  -> Expr EAddr
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> [Expr EWord]
  -- continuation with gas available for call
  -> (Gas t -> EVM t s ())
  -> EVM t s ()
callChecks this xGas xContext xTo xValue xInOffset xInSize xOutOffset xOutSize xs continue = do
  vm <- get
  let fees = vm.block.schedule
  accessMemoryRange xInOffset xInSize $
    accessMemoryRange xOutOffset xOutSize $ do
      availableGas <- use (#state % #gas)
      let recipientExists = accountExists xContext vm
      let from = fromMaybe vm.state.contract vm.state.overrideCaller
      fromBal <- preuse $ #env % #contracts % ix from % #balance
      costOfCall fees recipientExists xValue availableGas xGas xTo $ \cost gas' -> do
        let checkCallDepth =
              if length vm.frames >= 1024
              then do
                assign (#state % #stack) (Lit 0 : xs)
                assign (#state % #returndata) mempty
                pushTrace $ ErrorTrace CallDepthLimitReached
                next
              else continue (toGas gas')
        case (fromBal, xValue) of
          -- we're not transferring any value, and can skip the balance check
          (_, Lit 0) -> burn (cost - gas') checkCallDepth

          -- from is in the state, we check if they have enough balance
          (Just fb, _) -> do
            burn (cost - gas') $
              branch (Expr.gt xValue fb) $ \case
                True -> do
                  assign (#state % #stack) (Lit 0 : xs)
                  assign (#state % #returndata) mempty
                  pushTrace $ ErrorTrace (BalanceTooLow xValue this.balance)
                  next
                False -> checkCallDepth

          -- from is not in the state, we insert it if safe to do so and run the checks again
          (Nothing, _) -> case from of
            LitAddr _ -> do
              -- insert an entry in the state
              let contract = case vm.config.baseState of
                    AbstractBase -> unknownContract from
                    EmptyBase -> emptyContract
              (#env % #contracts) %= (Map.insert from contract)
              -- run callChecks again
              callChecks this xGas xContext xTo xValue xInOffset xInSize xOutOffset xOutSize xs continue

            -- adding a symbolic address into the state here would be unsound (due to potential aliasing)
            SymAddr _ -> do
              pc <- use (#state % #pc)
              state <- use #state
              partial $ UnexpectedSymbolicArg pc (getOpName state) "Attempting to transfer eth from a symbolic address that is not present in the state" (wrap [from])
            GVar _ -> internalError "Unexpected GVar"

precompiledContract
  :: (?op :: Word8, VMOps t)
  => Contract
  -> Gas t
  -> Addr
  -> Addr
  -> Expr EWord
  -> Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
  -> [Expr EWord]
  -> EVM t s ()
precompiledContract this xGas precompileAddr recipient xValue inOffset inSize outOffset outSize xs
  = callChecks this xGas (LitAddr recipient) (LitAddr precompileAddr) xValue inOffset inSize outOffset outSize xs $ \gas' ->
    do
      executePrecompile precompileAddr gas' inOffset inSize outOffset outSize xs
      self <- use (#state % #contract)
      stk <- use (#state % #stack)
      pc' <- use (#state % #pc)
      result' <- use #result
      vm <- get
      case result' of
        Nothing -> case stk of
          x:_ -> case maybeLitWord x of
            Just 0 ->
              pure ()
            Just 1 ->
              fetchAccount (LitAddr recipient) $ \_ -> do
                touchAccount self
                touchAccount (LitAddr recipient)
                transfer self (LitAddr recipient) xValue
            _ -> partial $
                   UnexpectedSymbolicArg pc' (getOpName vm.state) "unexpected return value from precompile" (wrap [x])
          _ -> underrun
        _ -> pure ()

executePrecompile
  :: (?op :: Word8, VMOps t)
  => Addr
  -> Gas t -> Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord -> [Expr EWord]
  -> EVM t s ()
executePrecompile preCompileAddr gasCap inOffset inSize outOffset outSize xs  = do
  vm <- get
  input <- readMemory inOffset inSize
  let fees = vm.block.schedule
      cost = costOfPrecompile fees preCompileAddr input
      notImplemented = internalError $ "precompile at address " <> show preCompileAddr <> " not yet implemented"
      precompileFail = burn' (subGas gasCap cost) $ do
                         assign (#state % #stack) (Lit 0 : xs)
                         pushTrace $ ErrorTrace PrecompileFailure
                         next
  if not (enoughGas cost gasCap) then
    burn' gasCap $ do
      assign (#state % #stack) (Lit 0 : xs)
      next
  else burn cost $
    case preCompileAddr of
      -- ECRECOVER
      0x1 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "ECRECOVER" $ \input' -> do
          case EVM.Precompiled.execute 0x1 (truncpadlit 128 input') 32 of
            Nothing -> do
              -- return no output for invalid signature
              assign (#state % #stack) (Lit 1 : xs)
              assign (#state % #returndata) mempty
              next
            Just output -> do
              assign (#state % #stack) (Lit 1 : xs)
              assign (#state % #returndata) (ConcreteBuf output)
              copyBytesToMemory (ConcreteBuf output) outSize (Lit 0) outOffset
              next

      -- SHA2-256
      0x2 ->
        forceConcreteBuf input "SHA2-256" $ \input' -> do
          let
            hash = sha256Buf input'
            sha256Buf x = ConcreteBuf $ BA.convert (Crypto.hash x :: Digest SHA256)
          assign (#state % #stack) (Lit 1 : xs)
          assign (#state % #returndata) hash
          copyBytesToMemory hash outSize (Lit 0) outOffset
          next

      -- RIPEMD-160
      0x3 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "RIPEMD160" $ \input' -> do
          let
            padding = BS.pack $ replicate 12 0
            hash' = BA.convert (Crypto.hash input' :: Digest RIPEMD160)
            hash  = ConcreteBuf $ padding <> hash'
          assign (#state % #stack) (Lit 1 : xs)
          assign (#state % #returndata) hash
          copyBytesToMemory hash outSize (Lit 0) outOffset
          next

      -- IDENTITY
      0x4 -> do
          assign (#state % #stack) (Lit 1 : xs)
          assign (#state % #returndata) input
          copyCallBytesToMemory input outSize outOffset
          next

      -- MODEXP
      0x5 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "MODEXP" $ \input' -> do
          let
            (lenb, lene, lenm) = parseModexpLength input'

            output = ConcreteBuf $
              if isZero (96 + lenb + lene) lenm input'
              then truncpadlit (unsafeInto lenm) (asBE (0 :: Int))
              else
                let
                  b = asInteger $ lazySlice 96 lenb input'
                  e = asInteger $ lazySlice (96 + lenb) lene input'
                  m = asInteger $ lazySlice (96 + lenb + lene) lenm input'
                in
                  padLeft (unsafeInto lenm) (asBE (expFast b e m))
          assign (#state % #stack) (Lit 1 : xs)
          assign (#state % #returndata) output
          copyBytesToMemory output outSize (Lit 0) outOffset
          next

      -- ECADD
      0x6 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "ECADD" $ \input' ->
          case EVM.Precompiled.execute 0x6 (truncpadlit 128 input') 64 of
            Nothing -> precompileFail
            Just output -> do
              let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
              assign (#state % #stack) (Lit 1 : xs)
              assign (#state % #returndata) truncpaddedOutput
              copyBytesToMemory truncpaddedOutput outSize (Lit 0) outOffset
              next

      -- ECMUL
      0x7 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "ECMUL" $ \input' ->
          case EVM.Precompiled.execute 0x7 (truncpadlit 96 input') 64 of
          Nothing -> precompileFail
          Just output -> do
            let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
            assign (#state % #stack) (Lit 1 : xs)
            assign (#state % #returndata) truncpaddedOutput
            copyBytesToMemory truncpaddedOutput outSize (Lit 0) outOffset
            next

      -- ECPAIRING
      0x8 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "ECPAIR" $ \input' ->
          case EVM.Precompiled.execute 0x8 input' 32 of
          Nothing -> precompileFail
          Just output -> do
            let truncpaddedOutput = ConcreteBuf $ truncpadlit 32 output
            assign (#state % #stack) (Lit 1 : xs)
            assign (#state % #returndata) truncpaddedOutput
            copyBytesToMemory truncpaddedOutput outSize (Lit 0) outOffset
            next

      -- BLAKE2
      0x9 ->
        -- TODO: support symbolic variant
        forceConcreteBuf input "BLAKE2" $ \input' -> do
          case (BS.length input', 1 >= BS.last input') of
            (213, True) -> case EVM.Precompiled.execute 0x9 input' 64 of
              Just output -> do
                let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
                assign (#state % #stack) (Lit 1 : xs)
                assign (#state % #returndata) truncpaddedOutput
                copyBytesToMemory truncpaddedOutput outSize (Lit 0) outOffset
                next
              Nothing -> precompileFail
            _ -> precompileFail

      _ -> notImplemented

truncpadlit :: Int -> ByteString -> ByteString
truncpadlit n xs = if m > n then BS.take n xs
                   else BS.append xs (BS.replicate (n - m) 0)
  where m = BS.length xs

lazySlice :: W256 -> W256 -> ByteString -> LS.ByteString
lazySlice offset size bs =
  let bs' = LS.take (unsafeInto size) (LS.drop (unsafeInto offset) (fromStrict bs))
  in bs' <> LS.replicate (unsafeInto size - LS.length bs') 0

parseModexpLength :: ByteString -> (W256, W256, W256)
parseModexpLength input =
  let lenb = word $ LS.toStrict $ lazySlice  0 32 input
      lene = word $ LS.toStrict $ lazySlice 32 64 input
      lenm = word $ LS.toStrict $ lazySlice 64 96 input
  in (lenb, lene, lenm)

--- checks if a range of ByteString bs starting at offset and length size is all zeros.
isZero :: W256 -> W256 -> ByteString -> Bool
isZero offset size bs =
  LS.all (== 0) $
    LS.take (unsafeInto size) $
      LS.drop (unsafeInto offset) $
        fromStrict bs

asInteger :: LS.ByteString -> Integer
asInteger xs = if xs == mempty then 0
  else 256 * asInteger (LS.init xs)
      + into (LS.last xs)

-- * Opcode helper actions

noop :: Monad m => m ()
noop = pure ()

pushTo :: MonadState s m => Lens s s [a] [a] -> a -> m ()
pushTo f x = f %= (x :)

pushToSequence :: MonadState s m => Setter s s (Seq a) (Seq a) -> a -> m ()
pushToSequence f x = f %= (Seq.|> x)

getCodeLocation :: VM t s -> CodeLocation
getCodeLocation vm = (vm.state.contract, vm.state.pc)

query :: Query t s -> EVM t s ()
query = assign #result . Just . HandleEffect . Query

choose :: Choose s -> EVM Symbolic s ()
choose = assign #result . Just . HandleEffect . Choose

-- | Construct RPC Query and halt execution until resolved
fetchAccount :: VMOps t => Expr EAddr -> (Contract -> EVM t s ()) -> EVM t s ()
fetchAccount addr continue =
  use (#env % #contracts % at addr) >>= \case
    Just c -> continue c
    Nothing -> case addr of
      SymAddr _ -> do
        pc <- use (#state % #pc)
        state <- use #state
        partial $ UnexpectedSymbolicArg pc (getOpName state) "trying to access a symbolic address that isn't already present in storage" (wrap [addr])
      LitAddr a -> do
        use (#cache % #fetched % at a) >>= \case
          Just c -> do
            assign (#env % #contracts % at addr) (Just c)
            continue c
          Nothing -> do
            base <- use (#config % #baseState)
            assign (#result) . Just . HandleEffect . Query $
              PleaseFetchContract a base
                (\c -> do assign (#cache % #fetched % at a) (Just c)
                          assign (#env % #contracts % at addr) (Just c)
                          assign #result Nothing
                          continue c)
      GVar _ -> internalError "Unexpected GVar"

accessStorage
  :: VMOps t => Expr EAddr
  -> Expr EWord
  -> (Expr EWord -> EVM t s ())
  -> EVM t s ()
accessStorage addr slot continue = do
  let slotConc = Expr.concKeccakSimpExpr slot
  use (#env % #contracts % at addr) >>= \case
    Just c ->
      -- Try first without concretization. Then if we get a Just, with concretization
      -- if both give a Just, should we `continue`.
      -- --> This is because readStorage can do smart rewrites, but only in case
      --     the expression is of a particular format, which can be destroyed by simplification.
      --     However, without concretization, it may not find things that are actually in the storage
      case readStorage slot c.storage of
        Just x -> case readStorage slotConc c.storage of
          Just _ -> continue x
          Nothing -> rpcCall c slotConc
        Nothing -> rpcCall c slotConc
    Nothing ->
      fetchAccount addr $ \_ ->
        accessStorage addr slot continue
  where
      rpcCall c slotConc = fetchAccount addr $ \_ ->
        if c.external
        then forceConcreteAddr addr "cannot read storage from symbolic addresses via rpc" $ \addr' ->
          forceConcrete slotConc "cannot read symbolic slots via RPC" $ \slot' -> do
            -- check if the slot is cached
            use (#env % #contracts % at (LitAddr addr')) >>= \case
              Nothing -> internalError $ "contract addr " <> show addr' <> " marked external not found in cache"
              Just fetched -> case readStorage (Lit slot') fetched.storage of
                          Nothing -> mkQuery addr' slot'
                          Just val -> continue val
        else do
          modifying (#env % #contracts % ix addr % #storage) (writeStorage slot (Lit 0))
          continue $ Lit 0
      mkQuery a s = query $
        PleaseFetchSlot a s
          (\x -> do
              modifying (#cache % #fetched % ix a % #storage) (writeStorage (Lit s) (Lit x))
              modifying (#env % #contracts % ix (LitAddr a) % #storage) (writeStorage (Lit s) (Lit x))
              assign #result Nothing
              continue (Lit x))

accessTStorage
  :: VMOps t => Expr EAddr
  -> Expr EWord
  -> (Expr EWord -> EVM t s ())
  -> EVM t s ()
accessTStorage addr slot continue = do
  let slotConc = Expr.concKeccakSimpExpr slot
  use (#env % #contracts % at addr) >>= \case
    Just c ->
      -- Try first without concretization. Then if we get a Just, with concretization
      -- See `accessStorage` for more details
      case readStorage slot c.tStorage of
        Just x -> case readStorage slotConc c.tStorage of
          Just _ -> continue x
          Nothing -> continue $ Lit 0
        Nothing -> continue $ Lit 0
    Nothing ->
      fetchAccount addr $ \_ ->
        accessTStorage addr slot continue

clearTStorages :: EVM t s ()
clearTStorages = (#env % #contracts) %= fmap (\c -> c { tStorage = ConcreteStore mempty } :: Contract)

accountExists :: Expr EAddr -> VM t s -> Bool
accountExists addr vm =
  case Map.lookup addr vm.env.contracts of
    Just c -> not (accountEmpty c)
    Nothing -> False

-- EIP 161
accountEmpty :: Contract -> Bool
accountEmpty c =
  case c.code of
    RuntimeCode (ConcreteRuntimeCode "") -> True
    RuntimeCode (SymbolicRuntimeCode b) -> null b
    _ -> False
  && c.nonce == (Just 0)
  -- TODO: handle symbolic balance...
  && c.balance == Lit 0

-- * How to finalize a transaction
finalize :: VMOps t => EVM t s ()
finalize = do
  let
    revertContracts  = use (#tx % #txReversion) >>= assign (#env % #contracts)
    revertSubstate   = assign (#tx % #subState) (SubState mempty mempty mempty mempty mempty mempty)

  use #result >>= \case
    Just (VMFailure (Revert _)) -> do
      revertContracts
      revertSubstate
    Just (VMFailure _) -> do
      -- burn remaining gas
      assign (#state % #gas) initialGas
      revertContracts
      revertSubstate
    Just (VMSuccess output) -> do
      clearTStorages
      -- deposit the code from a creation tx
      pc' <- use (#state % #pc)
      creation <- use (#tx % #isCreate)
      createe  <- use (#state % #contract)
      createeExists <- (Map.member createe) <$> use (#env % #contracts)
      when (creation && createeExists) $
        case output of
          ConcreteBuf bs ->
            replaceCode createe (RuntimeCode (ConcreteRuntimeCode bs))
          _ ->
            case Expr.toList output of
              Nothing -> do
                state <- use #state
                partial $
                  UnexpectedSymbolicArg pc' (getOpName state) "runtime code cannot have an abstract length" (wrap [output])
              Just ops ->
                replaceCode createe (RuntimeCode (SymbolicRuntimeCode ops))
    _ ->
      internalError "Finalising an unfinished tx."

  -- compute and pay the refund to the caller and the
  -- corresponding payment to the miner
  block <- use #block

  payRefunds

  touchAccount block.coinbase

  -- perform state trie clearing (EIP 161), of selfdestructs
  -- and touched accounts. addresses are cleared if they have
  --    a) selfdestructed, or
  --    b) been touched and
  --    c) are empty.
  -- (see Yellow Paper "Accrued Substate")
  --
  -- remove any destructed addresses
  destroyedAddresses <- use (#tx % #subState % #selfdestructs)
  modifying (#env % #contracts)
    (Map.filterWithKey (\k _ -> (k `notElem` destroyedAddresses)))
  -- then, clear any remaining empty and touched addresses
  touchedAddresses <- use (#tx % #subState % #touchedAccounts)
  modifying (#env % #contracts)
    (Map.filterWithKey
      (\k a -> not ((k `elem` touchedAddresses) && accountEmpty a)))

-- | Loads the selected contract as the current contract to execute
loadContract :: Expr EAddr -> State (VM t s) ()
loadContract target =
  preuse (#env % #contracts % ix target % #code) >>=
    \case
      Nothing ->
        internalError "Call target doesn't exist"
      Just targetCode -> do
        assign (#state % #contract) target
        assign (#state % #code)     targetCode
        assign (#state % #codeContract) target

limitStack :: VMOps t => Int -> EVM (t :: VMType) s () -> EVM t s ()
limitStack n continue = do
  stk <- use (#state % #stack)
  if length stk + n > 1024
    then vmError StackLimitExceeded
    else continue

notStatic :: VMOps t => EVM t s () -> EVM t s ()
notStatic continue = do
  bad <- use (#state % #static)
  if bad
    then vmError StateChangeWhileStatic
    else continue

forceAddr :: VMOps t => Expr EWord -> String -> (Expr EAddr -> EVM t s ()) -> EVM t s ()
forceAddr n msg continue = case wordToAddr n of
  Nothing -> do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [n])
  Just c -> continue c

forceConcrete :: VMOps t => Expr EWord -> String -> (W256 -> EVM t s ()) -> EVM t s ()
forceConcrete n msg continue = case maybeLitWord n of
  Nothing -> do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [n])
  Just c -> continue c

forceConcreteAddr :: VMOps t => Expr EAddr -> String -> (Addr -> EVM t s ()) -> EVM t s ()
forceConcreteAddr n msg continue = case maybeLitAddr n of
  Nothing -> do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [n])
  Just c -> continue c

forceConcreteAddr2 :: VMOps t => (Expr EAddr, Expr EAddr) -> String -> ((Addr, Addr) -> EVM t s ()) -> EVM t s ()
forceConcreteAddr2 (n,m) msg continue = case (maybeLitAddr n, maybeLitAddr m) of
  (Just c, Just d) -> continue (c,d)
  _ -> do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [n, m])

forceConcrete2 :: VMOps t => (Expr EWord, Expr EWord) -> String -> ((W256, W256) -> EVM t s ()) -> EVM t s ()
forceConcrete2 (n,m) msg continue = case (maybeLitWord n, maybeLitWord m) of
  (Just c, Just d) -> continue (c, d)
  _ -> do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [n, m])

forceConcreteBuf :: VMOps t => Expr Buf -> String -> (ByteString -> EVM t s ()) -> EVM t s ()
forceConcreteBuf (ConcreteBuf b) _ continue = continue b
forceConcreteBuf b msg _ = do
    vm <- get
    partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg (wrap [b])

-- * Substate manipulation
refund :: Word64 -> EVM t s ()
refund n = do
  self <- use (#state % #contract)
  pushTo (#tx % #subState % #refunds) (self, n)

unRefund :: Word64 -> EVM t s ()
unRefund n = do
  self <- use (#state % #contract)
  refs <- use (#tx % #subState % #refunds)
  assign (#tx % #subState % #refunds)
    (filter (\(a,b) -> not (a == self && b == n)) refs)

touchAccount :: Expr EAddr -> EVM t s ()
touchAccount = pushTo ((#tx % #subState) % #touchedAccounts)

selfdestruct :: Expr EAddr -> EVM t s ()
selfdestruct = pushTo ((#tx % #subState) % #selfdestructs)

accessAndBurn :: VMOps t => Expr EAddr -> EVM t s () -> EVM t s ()
accessAndBurn x cont = do
  FeeSchedule {..} <- use (#block % #schedule)
  acc <- accessAccountForGas x
  let cost = if acc then g_warm_storage_read else g_cold_account_access
  burn cost cont

-- | returns a wrapped boolean- if true, this address has been touched before in the txn (warm gas cost as in EIP 2929)
-- otherwise cold
accessAccountForGas :: Expr EAddr -> EVM t s Bool
accessAccountForGas addr = do
  accessedAddrs <- use (#tx % #subState % #accessedAddresses)
  let accessed = member addr accessedAddrs
  assign (#tx % #subState % #accessedAddresses) (insert addr accessedAddrs)
  pure accessed

-- | returns a wrapped boolean- if true, this slot has been touched before in the txn (warm gas cost as in EIP 2929)
-- otherwise cold
accessStorageForGas :: Expr EAddr -> Expr EWord -> EVM t s Bool
accessStorageForGas addr key = do
  accessedStrkeys <- use (#tx % #subState % #accessedStorageKeys)
  case maybeLitWord key of
    Just litword -> do
      let accessed = member (addr, litword) accessedStrkeys
      assign (#tx % #subState % #accessedStorageKeys) (insert (addr, litword) accessedStrkeys)
      pure accessed
    _ -> pure False

-- * Cheat codes

-- The cheat code is 7109709ecfa91a80626ff3989d68f67f5b1dd12d.
-- Call this address using one of the cheatActions below to do
-- special things, e.g. changing the block timestamp. Beware that
-- these are necessarily hevm specific.
cheatCode :: Expr EAddr
cheatCode = LitAddr $ unsafeInto (keccak' "hevm cheat code")

cheat
  :: (?op :: Word8, VMOps t)
  => Gas t -> (Expr EWord, Expr EWord) -> (Expr EWord, Expr EWord) -> [Expr EWord]
  -> EVM t s ()
cheat gas (inOffset, inSize) (outOffset, outSize) xs = do
  vm <- get
  input <- readMemory (Expr.add inOffset (Lit 4)) (Expr.sub inSize (Lit 4))
  calldata <- readMemory inOffset inSize
  abi <- readBytes 4 (Lit 0) <$> readMemory inOffset (Lit 4)
  let newContext = CallContext cheatCode cheatCode outOffset outSize (Lit 0) (maybeLitWord abi) calldata vm.env.contracts vm.tx.subState

  pushTrace $ FrameTrace newContext
  next
  vm1 <- get
  burn' gas $ pushTo #frames $ Frame
                    { state = vm1.state { stack = xs }
                    , context = newContext
                    }
  case maybeLitWord abi of
    Nothing -> partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) "symbolic cheatcode selector" (wrap [abi])
    Just (unsafeInto -> abi') ->
      case Map.lookup abi' cheatActions of
        Nothing -> vmError (BadCheatCode "Cannot understand cheatcode." abi')
        Just action -> action input

type CheatAction t s = Expr Buf -> EVM t s ()

cheatActions :: VMOps t => Map FunctionSelector (CheatAction t s)
cheatActions = Map.fromList
  [ action "ffi(string[])" $
      \sig input -> do
        vm <- get
        if vm.config.allowFFI then
          case decodeBuf [AbiArrayDynamicType AbiStringType] input of
            CAbi valsArr -> case valsArr of
              [AbiArrayDynamic AbiStringType strsV] ->
                let
                  cmd = fmap
                          (\case
                            (AbiString a) -> toString a
                            _ -> "")
                          (V.toList strsV)
                  cont bs = continueOnce $ do
                    frameReturnBuf bs
                in query (PleaseDoFFI cmd vm.osEnv cont)
              _ -> vmError (BadCheatCode "ffi(string[]) decoding of string failed" sig)
            _ -> vmError (BadCheatCode "ffi(string[]) parameter decoding failed" sig)
        else
          let msg = "ffi disabled: run again with --ffi if you want to allow tests to call external scripts"
          in partial $ UnexpectedSymbolicArg vm.state.pc (getOpName vm.state) msg []

  , action "warp(uint256)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [x]  -> do
          assign (#block % #timestamp) x
          doStop
        _ -> vmError (BadCheatCode "warp(uint256) parameter decoding failed" sig)

  , action "deal(address,uint256)" $
      \sig input -> case decodeStaticArgs 0 2 input of
        [a, amt] ->
          forceAddr a "vm.deal: cannot decode target into an address" $ \usr ->
            fetchAccount usr $ \_ -> do
              assign (#env % #contracts % ix usr % #balance) amt
              doStop
        _ -> vmError (BadCheatCode "deal(address,uint256) parameter decoding failed" sig)

  , action "assume(bool)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [c] -> do
          modifying #constraints ((:) (PEq (Lit 1) c))
          doStop
        _ -> vmError (BadCheatCode "assume(bool) parameter decoding failed." sig)

  , action "roll(uint256)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [x] -> forceConcrete x "cannot roll to a symbolic block number" $ \block -> do
          assign (#block % #number) block
          doStop
        _ -> vmError (BadCheatCode "roll(uint256) parameter decoding failed" sig)

  , action "store(address,bytes32,bytes32)" $
      \sig input -> case decodeStaticArgs 0 3 input of
        [a, slot, new] -> case wordToAddr a of
          Just a'@(LitAddr _) -> fetchAccount a' $ \_ -> do
            modifying (#env % #contracts % ix a' % #storage) (writeStorage slot new)
            doStop
          _ -> vmError (BadCheatCode "store(address,bytes32,bytes32) issue, address provided may not be an address?" sig)
        _ -> vmError (BadCheatCode "store(address,bytes32,bytes32) parameter decoding failed" sig)

  , action "load(address,bytes32)" $
      \sig input -> case decodeStaticArgs 0 2 input of
        [a, slot] -> case wordToAddr a of
          Just a'@(LitAddr _) -> fetchAccount a' $ \_ ->
            accessStorage a' slot $ \res -> do
              let buf = writeWord (Lit 0) res (ConcreteBuf "")
              frameReturnExpr buf -- TODO reivew
          _ -> vmError (BadCheatCode "load(address,bytes32) issue, maybe the address provided is not correct?" sig)
        _ -> vmError (BadCheatCode "load(address,bytes32) parameter decoding failed" sig)

  , action "sign(uint256,bytes32)" $
      \sig input -> case decodeStaticArgs 0 2 input of
        [sk, hash] ->
          forceConcrete2 (sk, hash) "cannot sign symbolic data" $ \(sk', hash') -> do
            let (v,r,s) = EVM.Sign.sign hash' (into sk')
                result = AbiTuple $
                  V.fromList
                    [ AbiUInt 8 $ into v
                    , AbiBytes 32 (word256Bytes r)
                    , AbiBytes 32 (word256Bytes s)
                    ]
            frameReturn result -- TODO ??
        _ -> vmError (BadCheatCode "sign(uint256,bytes32) parameter decoding failed" sig)

  , action "addr(uint256)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [sk] -> forceConcrete sk "cannot derive address for a symbolic key" $ \sk' -> do
          case EVM.Sign.deriveAddr $ into sk' of
            Nothing -> vmError (BadCheatCode "addr(uint256) could not derive address" sig)
            Just address -> do frameReturnBuf $ word256Bytes (into address) -- TODO ??
        _ -> vmError (BadCheatCode "addr(uint256) parameter decoding failed" sig)

  , action "prank(address)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [addr]  -> case wordToAddr addr of
          Just a -> do
            doStop
            assign (#state % #overrideCaller) (Just a)
            assign (#state % #resetCaller) True
          Nothing -> vmError (BadCheatCode "prank(address), could not decode address provided" sig)
        _ -> vmError (BadCheatCode "prank(address) parameter decoding failed" sig)

  , action "startPrank(address)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [addr]  -> case wordToAddr addr of
          Just a -> do
            doStop
            assign (#state % #overrideCaller) (Just a)
            assign (#state % #resetCaller) False
          Nothing -> vmError (BadCheatCode "startPrank(address), could not decode address provided" sig)
        _ -> vmError (BadCheatCode "startPrank(address) parameter decoding failed" sig)

  , action "stopPrank()" $
      \_ _ -> do
        doStop
        assign (#state % #overrideCaller) Nothing
        assign (#state % #resetCaller) False

  , action "createFork(string)" $
      \sig input -> case decodeBuf [AbiStringType] input of
        CAbi valsArr -> case valsArr of
          [AbiString bytes] -> do
            forkId <- length <$> gets (.forks)
            let urlOrAlias = Char8.unpack bytes
            modify' $ \vm -> vm { forks = vm.forks Seq.|> ForkState vm.env vm.block vm.cache urlOrAlias }
            frameReturn $ AbiUInt 256 (fromIntegral forkId)
          _ -> vmError (BadCheatCode "createFork(string) string provided may be incorrect?" sig)
        _ -> vmError (BadCheatCode "createFork(string) parameter decoding failed" sig)

  , action "selectFork(uint256)" $
      \sig input -> case decodeStaticArgs 0 1 input of
        [forkId] ->
          forceConcrete forkId "forkId of 'selectFork' must be concrete" $ \(fromIntegral -> forkId') -> do
            saved <- Seq.lookup forkId' <$> gets (.forks)
            case saved of
              Just forkState -> do
                vm <- get
                let contractAddr = vm.state.contract
                let callerAddr = vm.state.caller
                fetchAccount contractAddr $ \contractAcct -> fetchAccount callerAddr $ \callerAcct -> do
                  let
                    -- the current contract is persisted across forks
                    newContracts = Map.insert callerAddr callerAcct $
                                   Map.insert contractAddr contractAcct forkState.env.contracts
                    newEnv = (forkState.env :: Env) { contracts = newContracts }

                  when (vm.currentFork /= forkId') $ do
                    modify' $ \vm' -> vm'
                      { env = newEnv
                      , block = forkState.block
                      , forks = Seq.adjust' (\state -> (state :: ForkState)
                          { env = vm.env, block = vm.block, cache = vm.cache }
                        ) vm.currentFork  vm.forks
                      , currentFork = forkId'
                      }
                  doStop
              Nothing ->
                vmError (NonexistentFork forkId')
        _ -> vmError (BadCheatCode "selectFork(uint256) parameter decoding failed" sig)

  , action "activeFork()" $
      \_ _ -> do
        vm <- get
        frameReturn $ AbiUInt 256 (fromIntegral vm.currentFork)

  , action "label(address,string)" $
      \sig input -> case decodeBuf [AbiAddressType, AbiStringType] input of
        CAbi valsArr -> case valsArr of
          [AbiAddress addr, AbiString label] -> do
            #labels %= Map.insert addr (decodeUtf8 label)
            doStop
          _ -> vmError (BadCheatCode "label(address,string) address decoding failed" sig)
        _ -> vmError (BadCheatCode "label(address,string) parameter decoding failed" sig)

  , action "setEnv(string,string)" $
      \sig input -> case decodeBuf [AbiStringType, AbiStringType] input of
        CAbi valsArr -> case valsArr of
          [AbiString variable, AbiString value] -> do
            let (varStr, varVal) = (toString variable, toString value)
            #osEnv %= Map.insert varStr varVal
            doStop
          _ -> vmError (BadCheatCode "setEnv(string,string) address decoding failed" sig)
        _ -> vmError (BadCheatCode "setEnv(string,string) parameter decoding failed" sig)

  -- Single-value environment read cheat actions
  , $(envReadSingleCheat "envBool(string)") AbiBool stringToBool
  , $(envReadSingleCheat "envUint(string)") (AbiUInt 256) stringToWord256
  , $(envReadSingleCheat "envInt(string)") (AbiInt 256) stringToInt256
  , $(envReadSingleCheat "envAddress(string)") AbiAddress stringToAddress
  , $(envReadSingleCheat "envBytes32(string)") (AbiBytes 32) stringToBytes32
  , $(envReadSingleCheat "envString(string)") (\x -> AbiTuple $ V.fromList [AbiString x]) stringToByteString
  , $(envReadSingleCheat "envBytes(bytes)") (\x -> AbiTuple $ V.fromList [AbiBytesDynamic x]) stringHexToByteString

  -- Multi-value environment read cheat actions
  , $(envReadMultipleCheat "envBool(string,string)" AbiBoolType) stringToBool
  , $(envReadMultipleCheat "envUint(string,string)" $ AbiUIntType 256) stringToWord256
  , $(envReadMultipleCheat "envInt(string,string)" $ AbiIntType 256) stringToInt256
  , $(envReadMultipleCheat "envAddress(string,string)" AbiAddressType) stringToAddress
  , $(envReadMultipleCheat "envBytes32(string,string)" $ AbiBytesType 32) stringToBytes32
  , $(envReadMultipleCheat "envString(string,string)" AbiStringType) stringToByteString
  , $(envReadMultipleCheat "envBytes(bytes,bytes)" AbiBytesDynamicType) stringHexToByteString
  , action "assertTrue(bool)" $ \sig input ->
      case decodeBuf [AbiBoolType] input of
        CAbi [AbiBool True] -> doStop
        CAbi [AbiBool False] -> frameRevert "assertion failed"
        SAbi [eword] -> case (Expr.simplify (Expr.iszero eword)) of
          Lit 1 -> frameRevert "assertion failed"
          Lit 0 -> doStop
          ew -> branch ew $ \case
            True -> frameRevert "assertion failed"
            False -> doStop
        k -> vmError $ BadCheatCode ("assertTrue(bool) parameter decoding failed: " <> show k) sig
  , action "assertFalse(bool)" $ \sig input ->
      case decodeBuf [AbiBoolType] input of
        CAbi [AbiBool False] -> doStop
        CAbi [AbiBool True] -> frameRevert "assertion failed"
        SAbi [eword] -> case (Expr.simplify (Expr.iszero eword)) of
          Lit 0 -> frameRevert "assertion failed"
          Lit 1 -> doStop
          ew -> branch ew $ \case
            False -> frameRevert "assertion failed"
            True -> doStop
        k -> vmError $ BadCheatCode ("assertFalse(bool) parameter decoding failed: " <> show k) sig
  , action "assertEq(bool,bool)"       $ assertEq AbiBoolType
  , action "assertEq(uint256,uint256)" $ assertEq (AbiUIntType 256)
  , action "assertEq(int256,int256)"   $ assertEq (AbiIntType 256)
  , action "assertEq(address,address)" $ assertEq AbiAddressType
  , action "assertEq(bytes32,bytes32)" $ assertEq (AbiBytesType 32)
  , action "assertEq(string,string)"   $ assertEq (AbiStringType)
  --
  , action "assertNotEq(bool,bool)"       $ assertNotEq AbiBoolType
  , action "assertNotEq(uint256,uint256)" $ assertNotEq (AbiUIntType 256)
  , action "assertNotEq(int256,int256)"   $ assertNotEq (AbiIntType 256)
  , action "assertNotEq(address,address)" $ assertNotEq AbiAddressType
  , action "assertNotEq(bytes32,bytes32)" $ assertNotEq (AbiBytesType 32)
  , action "assertNotEq(string,string)"   $ assertNotEq (AbiStringType)
  --
  , action "assertLt(uint256,uint256)" $ assertLt (AbiUIntType 256)
  , action "assertLt(int256,int256)"   $ assertLt (AbiIntType 256)
  , action "assertLe(uint256,uint256)" $ assertLe (AbiUIntType 256)
  , action "assertLe(int256,int256)"   $ assertLe (AbiIntType 256)
  , action "assertGt(uint256,uint256)" $ assertGt (AbiUIntType 256)
  , action "assertGt(int256,int256)"   $ assertGt (AbiIntType 256)
  , action "assertGe(uint256,uint256)" $ assertGe (AbiUIntType 256)
  , action "assertGe(int256,int256)"   $ assertGe (AbiIntType 256)
  ]
  where
    action s f = (abiKeccak s, f (abiKeccak s))
    either' v l r = either l r v
    frameReturn :: VMOps t => AbiValue -> EVM t s ()
    frameReturn v = frameReturnBuf $ encodeAbiValue v
    frameReturnBuf :: VMOps t => ByteString -> EVM t s ()
    frameReturnBuf buf = frameReturnExpr $ ConcreteBuf buf
    frameReturnExpr :: VMOps t => Expr Buf -> EVM t s ()
    frameReturnExpr e = finishFrame (FrameReturned e)
    frameRevert :: VMOps t => ByteString -> EVM t s ()
    frameRevert err = finishFrame (FrameReverted $ errorMsg err)
    errorMsg :: ByteString -> Expr Buf
    errorMsg err = ConcreteBuf $ selector "Error(string)" <> encodeAbiValue (AbiTuple $ V.fromList [AbiString err])
    continueOnce cont = do
      assign #result Nothing
      cont
    doStop = finishFrame (FrameReturned mempty)
    toString = unpack . decodeUtf8
    strip0x s = if "0x" `isPrefixOf` s then drop 2 s else s
    stringToBool :: String -> Either ByteString Bool
    stringToBool s = case s of
      "true" -> Right True
      "True" -> Right True
      "false" -> Right False
      "False" -> Right False
      _ -> Left "invalid value"
    stringToWord256 :: String -> Either ByteString Word256
    stringToWord256 s = maybeToEither "invalid W256 value" $ readMaybe s
    stringToInt256 :: String -> Either ByteString Int256
    stringToInt256 s = maybeToEither "invalid Int256 value" $ readMaybe s
    stringToAddress :: String -> Either ByteString Addr
    stringToAddress s = fmap Addr $ maybeToEither "invalid address value" $ readMaybe s
    stringToBytes32 :: String -> Either ByteString ByteString
    stringToBytes32 s = fmap word256Bytes $ maybeToEither "invalid bytes32 value" $ readMaybe s
    stringToByteString :: String -> Either ByteString ByteString
    stringToByteString = Right . Char8.pack
    stringHexToByteString :: String -> Either ByteString ByteString
    stringHexToByteString s = either (const $ Left "invalid bytes value") Right $ BS16.decodeBase16Untyped . Char8.pack . strip0x $ s
    paramDecodeErr abitype name abivals = name <> "(" <> (show abitype) <> "," <> (show abitype) <>
      ")  parameter decoding failed. Error: " <> show abivals
    revertErr a b comp = frameRevert $ "assertion failed: " <>
      BS8.pack (show a) <> " " <> comp <> " " <> BS8.pack (show b)
    genAssert comp exprComp invComp name abitype sig input = do
      case decodeBuf [abitype, abitype] input of
        CAbi [a, b] | a `comp` b -> doStop
        CAbi [a, b] -> revertErr a b invComp
        SAbi [ew1, ew2] -> case (Expr.simplify (Expr.iszero $ exprComp ew1 ew2)) of
          Lit 0 -> doStop
          Lit _ -> revertErr ew1 ew2 invComp
          ew -> branch ew $ \case
            False -> doStop
            True -> revertErr ew1 ew2 invComp
        abivals -> vmError (BadCheatCode (paramDecodeErr abitype name abivals) sig)
    assertEq = genAssert (==) Expr.eq "!=" "assertEq"
    assertNotEq = genAssert (/=) (\a b -> Expr.iszero $ Expr.eq a b) "==" "assertNotEq"
    assertLt = genAssert (<) Expr.lt ">=" "assertLt"
    assertGt = genAssert (>) Expr.gt "<=" "assertGt"
    assertLe = genAssert (<=) Expr.leq ">" "assertLe"
    assertGe = genAssert (>=) Expr.geq "<" "assertGe"


-- * General call implementation ("delegateCall")
-- note that the continuation is ignored in the precompile case
delegateCall
  :: (VMOps t, ?op :: Word8)
  => Contract
  -> Gas t
  -> Expr EAddr
  -> Expr EAddr
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> Expr EWord
  -> [Expr EWord]
  -> (Expr EAddr -> EVM t s ())
  -> EVM t s ()
delegateCall this gasGiven xTo xContext xValue xInOffset xInSize xOutOffset xOutSize xs continue
  | isPrecompileAddr xTo
      = forceConcreteAddr2 (xTo, xContext) "Cannot call precompile with symbolic addresses" $
          \(xTo', xContext') ->
            precompiledContract this gasGiven xTo' xContext' xValue xInOffset xInSize xOutOffset xOutSize xs
  | xTo == cheatCode = do
      cheat gasGiven (xInOffset, xInSize) (xOutOffset, xOutSize) xs
  | otherwise =
      callChecks this gasGiven xContext xTo xValue xInOffset xInSize xOutOffset xOutSize xs $
        \xGas -> do
          resetCaller <- use $ #state % #resetCaller
          when resetCaller $ assign (#state % #overrideCaller) Nothing
          vm0 <- get
          fetchAccount xTo $ \target -> case target.code of
              UnknownCode _ -> do
                pc <- use (#state % #pc)
                state <- use #state
                partial $ UnexpectedSymbolicArg pc (getOpName state) "call target has unknown code" (wrap [xTo])
              _ -> do
                burn' xGas $ do
                  calldata <- readMemory xInOffset xInSize
                  abi <- maybeLitWord . readBytes 4 (Lit 0) <$> readMemory xInOffset (Lit 4)
                  let newContext = CallContext
                                    { target    = xTo
                                    , context   = xContext
                                    , offset    = xOutOffset
                                    , size      = xOutSize
                                    , codehash  = target.codehash
                                    , callreversion = vm0.env.contracts
                                    , subState  = vm0.tx.subState
                                    , abi
                                    , calldata
                                    }
                  pushTrace (FrameTrace newContext)
                  next
                  vm1 <- get
                  pushTo #frames $ Frame
                    { state = vm1.state { stack = xs }
                    , context = newContext
                    }

                  let clearInitCode = \case
                        (InitCode _ _) -> InitCode mempty mempty
                        a -> a

                  newMemory <- ConcreteMemory <$> VUnboxed.Mutable.new 0
                  zoom #state $ do
                    assign #gas xGas
                    assign #pc 0
                    assign #code (clearInitCode target.code)
                    assign #codeContract xTo
                    assign #stack mempty
                    assign #memory newMemory
                    assign #memorySize 0
                    assign #returndata mempty
                    assign #calldata calldata
                    assign #overrideCaller Nothing
                    assign #resetCaller False
                  continue xTo

-- -- * Contract creation

-- EIP 684
collision :: Maybe Contract -> Bool
collision c' = case c' of
  Just c -> c.nonce /= Just 0 || case c.code of
    RuntimeCode (ConcreteRuntimeCode "") -> False
    RuntimeCode (SymbolicRuntimeCode b) -> not $ null b
    _ -> True
  Nothing -> False

create :: forall t s. (?op :: Word8, VMOps t)
  => Expr EAddr -> Contract
  -> Expr EWord -> Gas t -> Expr EWord -> [Expr EWord] -> Expr EAddr -> Expr Buf -> EVM t s ()
create self this xSize xGas xValue xs newAddr initCode = do
  vm0 <- get
  if xSize > Lit (vm0.block.maxCodeSize * 2)
  then do
    assign (#state % #stack) (Lit 0 : xs)
    assign (#state % #returndata) mempty
    vmError $ MaxInitCodeSizeExceeded (vm0.block.maxCodeSize * 2) xSize
  else if this.nonce == Just maxBound
  then do
    assign (#state % #stack) (Lit 0 : xs)
    assign (#state % #returndata) mempty
    pushTrace $ ErrorTrace NonceOverflow
    next
  else if length vm0.frames >= 1024
  then do
    assign (#state % #stack) (Lit 0 : xs)
    assign (#state % #returndata) mempty
    pushTrace $ ErrorTrace CallDepthLimitReached
    next
  else if collision $ Map.lookup newAddr vm0.env.contracts
  then burn' xGas $ do
    assign (#state % #stack) (Lit 0 : xs)
    assign (#state % #returndata) mempty
    modifying (#env % #contracts % ix self % #nonce) (fmap ((+) 1))
    next
  -- do we have enough balance
  else branch (Expr.gt xValue this.balance) $ \case
      True -> do
        assign (#state % #stack) (Lit 0 : xs)
        assign (#state % #returndata) mempty
        pushTrace $ ErrorTrace $ BalanceTooLow xValue this.balance
        next
        touchAccount self
        touchAccount newAddr
      -- are we overflowing the nonce
      False -> burn' xGas $ do
        case parseInitCode initCode of
          Nothing ->
            partial $ UnexpectedSymbolicArg vm0.state.pc (getOpName vm0.state) "initcode must have a concrete prefix" []
          Just c -> do
            let
              newContract = initialContract c
              newContext  =
                CreationContext { address   = newAddr
                                , codehash  = newContract.codehash
                                , createreversion = vm0.env.contracts
                                , subState  = vm0.tx.subState
                                }

            zoom (#env % #contracts) $ do
              oldAcc <- use (at newAddr)
              let oldBal = maybe (Lit 0) (.balance) oldAcc

              assign (at newAddr) (Just (newContract & #balance .~ oldBal))
              modifying (ix self % #nonce) (fmap ((+) 1))

            let
              resetStorage :: Expr Storage -> Expr Storage
              resetStorage = \case
                  ConcreteStore _ -> ConcreteStore mempty
                  AbstractStore a Nothing -> AbstractStore a Nothing
                  SStore _ _ p -> resetStorage p
                  AbstractStore _ (Just _) -> internalError "unexpected logical store in EVM.hs"
                  GVar _  -> internalError "unexpected global variable"

            modifying (#env % #contracts % ix newAddr % #storage) resetStorage
            modifying (#env % #contracts % ix newAddr % #origStorage) resetStorage
            modifying (#tx % #subState % #createdContracts) (insert newAddr)

            transfer self newAddr xValue

            pushTrace (FrameTrace newContext)
            next
            vm1 <- get
            pushTo #frames $ Frame
              { context = newContext
              , state   = vm1.state { stack = xs }
              }

            state :: FrameState t s <- lift blankState
            assign #state $ state
              { contract     = newAddr
              , codeContract = newAddr
              , code         = c
              , callvalue    = xValue
              , caller       = self
              , gas          = xGas
              }

-- | Parses a raw Buf into an InitCode
--
-- solidity implements constructor args by appending them to the end of
-- the initcode. we support this internally by treating initCode as a
-- concrete region (initCode) followed by a potentially symbolic region
-- (arguments).
--
-- when constructing a contract that has symbolic constructor args, we
-- need to apply some heuristics to convert the (unstructured) initcode
-- in memory into this structured representation. The (unsound, bad,
-- hacky) way that we do this, is by: looking for the first potentially
-- symbolic byte in the input buffer and then splitting it there into code / data.
parseInitCode :: Expr Buf -> Maybe ContractCode
parseInitCode (ConcreteBuf b) = Just (InitCode b mempty)
parseInitCode buf = if V.null conc
                    then Nothing
                    else Just $ InitCode (BS.pack $ V.toList conc) sym
  where
    conc = Expr.concretePrefix buf
    -- unsafeInto: findIndex will always be positive
    sym = Expr.drop (unsafeInto (V.length conc)) buf

-- | Replace a contract's code, like when CREATE returns
-- from the constructor code.
replaceCode :: Expr EAddr -> ContractCode -> EVM t s ()
replaceCode target newCode =
  zoom (#env % #contracts % at target) $
    get >>= \case
      Just now -> case now.code of
        InitCode _ _ ->
          put . Just $
            ((initialContract newCode) :: Contract)
              { balance = now.balance
              , nonce = now.nonce
              , storage = now.storage
              }
        RuntimeCode _ ->
          internalError $ "Can't replace code of deployed contract " <> show target
        UnknownCode _ ->
          internalError "Can't replace unknown code"
      Nothing ->
        internalError "Can't replace code of nonexistent contract"

replaceCodeOfSelf :: ContractCode -> EVM t s ()
replaceCodeOfSelf newCode = do
  vm <- get
  replaceCode vm.state.contract newCode

resetState :: VMOps t => EVM t s ()
resetState = do
  state <- lift blankState
  modify' $ \vm -> vm { result = Nothing, frames = [], state }

-- * VM error implementation

vmError :: VMOps t => EvmError -> EVM t s ()
vmError e = finishFrame (FrameErrored e)

wrap :: Typeable a => [Expr a] -> [SomeExpr]
wrap = fmap SomeExpr

underrun :: VMOps t => EVM t s ()
underrun = vmError StackUnderrun

-- | A stack frame can be popped in three ways.
data FrameResult
  = FrameReturned (Expr Buf) -- ^ STOP, RETURN, or no more code
  | FrameReverted (Expr Buf) -- ^ REVERT
  | FrameErrored EvmError -- ^ Any other error
  deriving Show

-- | This function defines how to pop the current stack frame in either of
-- the ways specified by 'FrameResult'.
--
-- It also handles the case when the current stack frame is the only one;
-- in this case, we set the final '_result' of the VM execution.
finishFrame :: VMOps t => FrameResult -> EVM t s ()
finishFrame how = do
  oldVm <- get

  case oldVm.frames of
    -- Is the current frame the only one?
    [] -> do
      case how of
          FrameReturned output -> assign #result . Just $ VMSuccess output
          FrameReverted buffer -> assign #result . Just $ VMFailure (Revert buffer)
          FrameErrored e       -> assign #result . Just $ VMFailure e
      finalize

    -- Are there some remaining frames?
    nextFrame : remainingFrames -> do

      -- Insert a debug trace.
      insertTrace $ case how of
        FrameReturned output -> ReturnTrace output nextFrame.context
        FrameReverted e -> ErrorTrace (Revert e)
        FrameErrored e -> ErrorTrace e
      -- Pop to the previous level of the debug trace stack.
      popTrace

      -- Pop the top frame.
      assign #frames remainingFrames
      -- Install the state of the frame to which we shall return.
      assign #state nextFrame.state

      -- Now dispatch on whether we were creating or calling,
      -- and whether we shall return, revert, or internalError(six cases).
      case nextFrame.context of

        -- Were we calling?
        CallContext _ _ outOffset outSize _ _ _ reversion subState' -> do

          -- Excerpt K.1. from the yellow paper:
          -- K.1. Deletion of an Account Despite Out-of-gas.
          -- At block 2675119, in the transaction 0xcf416c536ec1a19ed1fb89e4ec7ffb3cf73aa413b3aa9b77d60e4fd81a4296ba,
          -- an account at address 0x03 was called and an out-of-gas occurred during the call.
          -- Against the equation (197), this added 0x03 in the set of touched addresses, and this transaction turned σ[0x03] into ∅.

          -- In other words, we special case address 0x03 and keep it in the set of touched accounts during revert
          touched <- use (#tx % #subState % #touchedAccounts)

          let
            subState'' = over #touchedAccounts (maybe id cons (find (LitAddr 3 ==) touched)) subState'
            revertContracts = assign (#env % #contracts) reversion
            revertSubstate  = assign (#tx % #subState) subState''

          case how of
            -- Case 1: Returning from a call?
            FrameReturned output -> do
              assign (#state % #returndata) output
              copyCallBytesToMemory output outSize outOffset
              reclaimRemainingGasAllowance oldVm
              push 1

            -- Case 2: Reverting during a call?
            FrameReverted output -> do
              revertContracts
              revertSubstate
              assign (#state % #returndata) output
              copyCallBytesToMemory output outSize outOffset
              reclaimRemainingGasAllowance oldVm
              push 0

            -- Case 3: Error during a call?
            FrameErrored _ -> do
              revertContracts
              revertSubstate
              assign (#state % #returndata) mempty
              push 0
        -- Or were we creating?
        CreationContext _ _ reversion subState' -> do
          creator <- use (#state % #contract)
          let
            createe = oldVm.state.contract
            revertContracts = assign (#env % #contracts) reversion'
            revertSubstate  = assign (#tx % #subState) subState'

            -- persist the nonce through the reversion
            reversion' = (Map.adjust (over #nonce (fmap ((+) 1))) creator) reversion

          case how of
            -- Case 4: Returning during a creation?
            FrameReturned output -> do
              let onContractCode contractCode = do
                    replaceCode createe contractCode
                    assign (#state % #returndata) mempty
                    reclaimRemainingGasAllowance oldVm
                    pushAddr createe
              case output of
                ConcreteBuf bs ->
                  onContractCode $ RuntimeCode (ConcreteRuntimeCode bs)
                _ ->
                  case Expr.toList output of
                    Nothing -> partial $
                      UnexpectedSymbolicArg
                        oldVm.state.pc
                        (getOpName oldVm.state)
                        "runtime code cannot have an abstract length"
                        (wrap [output])
                    Just newCode -> do
                      onContractCode $ RuntimeCode (SymbolicRuntimeCode newCode)

            -- Case 5: Reverting during a creation?
            FrameReverted output -> do
              revertContracts
              revertSubstate
              assign (#state % #returndata) output
              reclaimRemainingGasAllowance oldVm
              push 0

            -- Case 6: Error during a creation?
            FrameErrored _ -> do
              revertContracts
              revertSubstate
              assign (#state % #returndata) mempty
              push 0


-- * Memory helpers

accessUnboundedMemoryRange
  :: VMOps t => Word64
  -> Word64
  -> EVM t s ()
  -> EVM t s ()
accessUnboundedMemoryRange _ 0 continue = continue
accessUnboundedMemoryRange f l continue = do
  m0 <- use (#state % #memorySize)
  fees <- gets (.block.schedule)
  let m1 = 32 * ceilDiv (max m0 (f + l)) 32
  burn (memoryCost fees m1 - memoryCost fees m0) $ do
    assign (#state % #memorySize) m1
    continue

accessMemoryRange
  :: VMOps t
  => Expr EWord
  -> Expr EWord
  -> EVM t s ()
  -> EVM t s ()
accessMemoryRange _ (Lit 0) continue = continue
accessMemoryRange (Lit offs) (Lit sz) continue =
  case (,) <$> toWord64 offs <*> toWord64 sz of
    Nothing -> vmError IllegalOverflow
    Just (offs64, sz64) ->
      if offs64 + sz64 < sz64
        then vmError IllegalOverflow
        -- we need to limit these to <256MB because otherwise we could run out of memory
        -- in e.g. OpCalldatacopy and subsequent memory allocation when running with abstract gas.
        -- In these cases, the system would try to allocate a large (but <2**64 bytes) memory
        -- that leads to out-of-heap. Real-world scenarios cannot allocate 256MB of memory due to gas
        else if offs64 >= 0x0fffffff || sz64 >= 0x0fffffff
             then vmError IllegalOverflow
             else accessUnboundedMemoryRange offs64 sz64 continue
-- we just ignore gas if we get symbolic inputs
accessMemoryRange _ _ continue = continue

accessMemoryWord
  :: VMOps t => Expr EWord -> EVM t s () -> EVM t s ()
accessMemoryWord x = accessMemoryRange x (Lit 32)

copyBytesToMemory
  :: Expr Buf -> Expr EWord -> Expr EWord -> Expr EWord -> EVM t s ()
copyBytesToMemory bs size srcOffset memOffset =
  if size == Lit 0 then noop
  else do
    gets (.state.memory) >>= \case
      ConcreteMemory mem ->
        case (bs, size, srcOffset, memOffset) of
          (ConcreteBuf b, Lit size', Lit srcOffset', Lit memOffset') -> do
            let src =
                  if srcOffset' >= unsafeInto (BS.length b) then
                    BS.replicate (unsafeInto size') 0
                  else
                    BS.take (unsafeInto size') $
                    padRight (unsafeInto size') $
                    BS.drop (unsafeInto srcOffset') b

            writeMemory mem (unsafeInto memOffset') src
          _ -> do
            -- copy out and move to symbolic memory
            buf <- freezeMemory mem
            assign (#state % #memory) $
              SymbolicMemory $ copySlice srcOffset memOffset size bs buf
      SymbolicMemory mem ->
        assign (#state % #memory) $
          SymbolicMemory $ copySlice srcOffset memOffset size bs mem

copyCallBytesToMemory
  :: Expr Buf -> Expr EWord -> Expr EWord -> EVM t s ()
copyCallBytesToMemory bs size yOffset =
  copyBytesToMemory bs (Expr.min size (bufLength bs)) (Lit 0) yOffset

readMemory :: Expr EWord -> Expr EWord -> EVM t s (Expr Buf)
readMemory offset' size' = do
  vm <- get
  case vm.state.memory of
    ConcreteMemory mem -> do
      case (offset', size') of
        (Lit offset, Lit size) -> do
          let memSize :: Word64 = unsafeInto (VUnboxed.Mutable.length mem)
          if size > Expr.maxBytes ||
             offset + size > Expr.maxBytes ||
             offset >= into memSize then
            -- reads past memory are all zeros
            pure $ ConcreteBuf $ BS.replicate (unsafeInto size) 0
          else do
            let pastEnd = (unsafeInto offset + unsafeInto size) - unsafeInto memSize
            let fromMemSize = if pastEnd > 0 then unsafeInto size - pastEnd else unsafeInto size

            buf <- VUnboxed.freeze $ VUnboxed.Mutable.slice (unsafeInto offset) fromMemSize mem
            let dataFromMem = BS.pack $ VUnboxed.toList buf
            pure $ ConcreteBuf $ dataFromMem <> BS.replicate pastEnd 0
        _ -> do
          buf <- freezeMemory mem
          pure $ copySlice offset' (Lit 0) size' buf mempty
    SymbolicMemory mem ->
      pure $ copySlice offset' (Lit 0) size' mem mempty

-- * Tracing

withTraceLocation :: TraceData -> EVM t s Trace
withTraceLocation x = do
  vm <- get
  let this = fromJust $ currentContract vm
  pure Trace
    { tracedata = x
    , contract = this
    , opIx = fromMaybe 0 $ this.opIxMap SV.!? vm.state.pc
    }

pushTrace :: TraceData -> EVM t s ()
pushTrace x = do
  trace <- withTraceLocation x
  modifying #traces $
    \t -> Zipper.children $ Zipper.insert (Node trace []) t

insertTrace :: TraceData -> EVM t s ()
insertTrace x = do
  trace <- withTraceLocation x
  modifying #traces $
    \t -> Zipper.nextSpace $ Zipper.insert (Node trace []) t

popTrace :: EVM t s ()
popTrace =
  modifying #traces $
    \t -> case Zipper.parent t of
            Nothing -> internalError "internal internalError(trace root)"
            Just t' -> Zipper.nextSpace t'

zipperRootForest :: Zipper.TreePos Zipper.Empty a -> Forest a
zipperRootForest z =
  case Zipper.parent z of
    Nothing -> Zipper.toForest z
    Just z' -> zipperRootForest (Zipper.nextSpace z')

traceForest :: VM t s -> Forest Trace
traceForest vm = zipperRootForest vm.traces

traceForest' :: Expr End -> Forest Trace
traceForest' (Success _ (TraceContext f _ _) _ _) = f
traceForest' (Partial _ (TraceContext f _ _) _) = f
traceForest' (Failure _ (TraceContext f _ _) _) = f
traceForest' (ITE {}) = internalError"Internal Error: ITE does not contain a trace"
traceForest' (GVar {}) = internalError"Internal Error: Unexpected GVar"

traceContext :: Expr End -> TraceContext
traceContext (Success _ c _ _) = c
traceContext (Partial _ c _) = c
traceContext (Failure _ c _) = c
traceContext (ITE {}) = internalError"Internal Error: ITE does not contain a trace"
traceContext (GVar {}) = internalError"Internal Error: Unexpected GVar"

traceTopLog :: [Expr Log] -> EVM t s ()
traceTopLog [] = noop
traceTopLog ((LogEntry addr bytes topics) : _) = do
  trace <- withTraceLocation (EventTrace addr bytes topics)
  modifying #traces $
    \t -> Zipper.nextSpace (Zipper.insert (Node trace []) t)
traceTopLog ((GVar _) : _) = internalError "unexpected global variable"

-- * Stack manipulation

push :: W256 -> EVM t s ()
push = pushSym . Lit

pushSym :: Expr EWord -> EVM t s ()
pushSym x = #state % #stack %= (x :)

pushAddr :: Expr EAddr -> EVM t s ()
pushAddr (LitAddr x) = #state % #stack %= (Lit (into x) :)
pushAddr x@(SymAddr _) = #state % #stack %= (WAddr x :)
pushAddr (GVar _) = internalError "Unexpected GVar"

stackOp1
  :: (?op :: Word8, VMOps t)
  => Word64
  -> (Expr EWord -> Expr EWord)
  -> EVM t s ()
stackOp1 cost f =
  use (#state % #stack) >>= \case
    x:xs ->
      burn cost $ do
        next
        let !y = f x
        #state % #stack .= y : xs
    _ ->
      underrun

stackOp2
  :: (?op :: Word8, VMOps t)
  => Word64
  -> (Expr EWord -> Expr EWord -> Expr EWord)
  -> EVM t s ()
stackOp2 cost f =
  use (#state % #stack) >>= \case
    x:y:xs ->
      burn cost $ do
        next
        #state % #stack .= f x y : xs
    _ ->
      underrun

stackOp3
  :: (?op :: Word8, VMOps t)
  => Word64
  -> (Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord)
  -> EVM t s ()
stackOp3 cost f =
  use (#state % #stack) >>= \case
    x:y:z:xs ->
      burn cost $ do
      next
      (#state % #stack) .= f x y z : xs
    _ ->
      underrun

-- * Bytecode data functions

checkJump :: VMOps t => Int -> [Expr EWord] -> EVM t s ()
checkJump x xs = noJumpIntoInitData x $ do
  vm <- get
  case isValidJumpDest vm x of
    True -> do
      #state % #stack .= xs
      #state % #pc .= x
    False -> vmError BadJumpDestination

-- fails with partial if we're trying to jump into the symbolic region of an `InitCode`
noJumpIntoInitData :: VMOps t => Int -> EVM t s () -> EVM t s ()
noJumpIntoInitData idx cont = do
  vm <- get
  case vm.state.code of
    -- init code is totally concrete, so we don't return partial if we're
    -- jumping beyond the range of `ops`
    InitCode _ (ConcreteBuf "") -> cont
    -- init code has a symbolic region, so check if we're trying to jump into
    -- the symbolic region and return partial if we are
    InitCode ops _ -> if idx > BS.length ops
                      then partial $ JumpIntoSymbolicCode vm.state.pc idx
                      else cont
    -- we're not executing init code, so nothing to check here
    _ -> cont

isValidJumpDest :: VM t s -> Int -> Bool
isValidJumpDest vm x = let
    code = vm.state.code
    self = vm.state.codeContract
    contract = fromMaybe
      (internalError "self not found in current contracts")
      (Map.lookup self vm.env.contracts)
    op = case code of
      UnknownCode _ -> internalError "Cannot analyze jumpdests for unknown code"
      InitCode ops _ -> BS.indexMaybe ops x
      RuntimeCode (ConcreteRuntimeCode ops) -> BS.indexMaybe ops x
      RuntimeCode (SymbolicRuntimeCode ops) -> ops V.!? x >>= maybeLitByte
  in case op of
       Nothing -> False
       Just b -> 0x5b == b && OpJumpdest == snd (contract.codeOps V.! (contract.opIxMap SV.! x))

opSize :: Word8 -> Int
opSize x | x >= 0x60 && x <= 0x7f = into x - 0x60 + 2
opSize _                          = 1

--  i of the resulting vector contains the operation index for
-- the program counter value i.  This is needed because source map
-- entries are per operation, not per byte.
mkOpIxMap :: ContractCode -> SV.Vector Int
mkOpIxMap (UnknownCode _) = internalError "Cannot build opIxMap for unknown code"
mkOpIxMap (InitCode conc _)
  = SV.create $ SV.new (BS.length conc) >>= \v ->
      -- Loop over the byte string accumulating a vector-mutating action.
      -- This is somewhat obfuscated, but should be fast.
      let (_, _, _, m) = BS.foldl' (go v) (0 :: Word8, 0, 0, pure ()) conc
      in m >> pure v
      where
        -- concrete case
        go v (0, !i, !j, !m) x | x >= 0x60 && x <= 0x7f =
          {- Start of PUSH op. -} (x - 0x60 + 1, i + 1, j,     m >> SV.write v i j)
        go v (1, !i, !j, !m) _ =
          {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> SV.write v i j)
        go v (0, !i, !j, !m) _ =
          {- Other op. -}         (0,            i + 1, j + 1, m >> SV.write v i j)
        go v (n, !i, !j, !m) _ =
          {- PUSH data. -}        (n - 1,        i + 1, j,     m >> SV.write v i j)

mkOpIxMap (RuntimeCode (ConcreteRuntimeCode ops)) =
  mkOpIxMap (InitCode ops mempty) -- a bit hacky

mkOpIxMap (RuntimeCode (SymbolicRuntimeCode ops))
  = SV.create $ SV.new (length ops) >>= \v ->
      let (_, _, _, m) = foldl (go v) (0, 0, 0, pure ()) (stripBytecodeMetadataSym $ V.toList ops)
      in m >> pure v
      where
        go v (0, !i, !j, !m) x = case maybeLitByte x of
          Just x' -> if x' >= 0x60 && x' <= 0x7f
            -- start of PUSH op --
                     then (x' - 0x60 + 1, i + 1, j,     m >> SV.write v i j)
            -- other data --
                     else (0,             i + 1, j + 1, m >> SV.write v i j)
          _ -> internalError $ "cannot analyze symbolic code:\nx: " <> show x <> " i: " <> show i <> " j: " <> show j

        go v (1, !i, !j, !m) _ =
          {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> SV.write v i j)
        go v (n, !i, !j, !m) _ =
          {- PUSH data. -}        (n - 1,        i + 1, j,     m >> SV.write v i j)


vmOp :: VM t s -> Maybe Op
vmOp vm =
  let i  = vm ^. #state % #pc
      code' = vm ^. #state % #code
      (op, pushdata) = case code' of
        UnknownCode _ -> internalError "cannot get op from unknown code"
        InitCode xs' _ ->
          (BS.index xs' i, fmap LitByte $ BS.unpack $ BS.drop i xs')
        RuntimeCode (ConcreteRuntimeCode xs') ->
          (BS.index xs' i, fmap LitByte $ BS.unpack $ BS.drop i xs')
        RuntimeCode (SymbolicRuntimeCode xs') ->
          ( fromMaybe (internalError "unexpected symbolic code") . maybeLitByte $ xs' V.! i , V.toList $ V.drop i xs')
  in if (opslen code' < i)
     then Nothing
     else Just (readOp op pushdata)

vmOpIx :: VM t s -> Maybe Int
vmOpIx vm =
  do self <- currentContract vm
     self.opIxMap SV.!? vm.state.pc

-- Maps operation indices into a pair of (bytecode index, operation)
mkCodeOps :: ContractCode -> V.Vector (Int, Op)
mkCodeOps contractCode =
  let l = case contractCode of
            UnknownCode _ -> internalError "Cannot make codeOps for unknown code"
            InitCode bytes _ ->
              LitByte <$> (BS.unpack bytes)
            RuntimeCode (ConcreteRuntimeCode ops) ->
              LitByte <$> (BS.unpack $ stripBytecodeMetadata ops)
            RuntimeCode (SymbolicRuntimeCode ops) ->
              stripBytecodeMetadataSym $ V.toList ops
  in V.fromList . toList $ go 0 l
  where
    go !i !xs =
      case uncons xs of
        Nothing ->
          mempty
        Just (x, xs') ->
          let x' = fromMaybe (internalError "unexpected symbolic code argument") $ maybeLitByte x
              j = opSize x'
          in (i, readOp x' xs') Seq.<| go (i + j) (drop j xs)

-- * Gas cost calculation helpers

concreteModexpGasFee :: ByteString -> Word64
concreteModexpGasFee input =
  if lenb < into (maxBound :: Word32) &&
     (lene < into (maxBound :: Word32) || (lenb == 0 && lenm == 0)) &&
     lenm < into (maxBound :: Word64)
  then
    max 200 ((multiplicationComplexity * iterCount) `div` 3)
  else
    maxBound -- TODO: this is not 100% correct, return Nothing on overflow
  where
    (lenb, lene, lenm) = parseModexpLength input
    ez = isZero (96 + lenb) lene input
    e' = word $ LS.toStrict $
      lazySlice (96 + lenb) (min 32 lene) input
    nwords :: Word64
    nwords = ceilDiv (unsafeInto $ max lenb lenm) 8
    multiplicationComplexity = nwords * nwords
    iterCount' :: Word64
    iterCount' | lene <= 32 && ez = 0
               | lene <= 32 = unsafeInto (log2 e')
               | e' == 0 = 8 * (unsafeInto lene - 32)
               | otherwise = unsafeInto (log2 e') + 8 * (unsafeInto lene - 32)
    iterCount = max iterCount' 1

-- Gas cost of precompiles
costOfPrecompile :: FeeSchedule Word64 -> Addr -> Expr Buf -> Word64
costOfPrecompile (FeeSchedule {..}) precompileAddr input =
  let errorDynamicSize = internalError "precompile input cannot have a dynamic size"
      inputLen = case input of
                   ConcreteBuf bs -> unsafeInto $ BS.length bs
                   AbstractBuf _ -> errorDynamicSize
                   buf -> case bufLength buf of
                            Lit l -> unsafeInto l -- TODO: overflow
                            _ -> errorDynamicSize
  in case precompileAddr of
    -- ECRECOVER
    0x1 -> 3000
    -- SHA2-256
    0x2 -> (((inputLen + 31) `div` 32) * 12) + 60
    -- RIPEMD-160
    0x3 -> (((inputLen + 31) `div` 32) * 120) + 600
    -- IDENTITY
    0x4 -> (((inputLen + 31) `div` 32) * 3) + 15
    -- MODEXP
    0x5 -> case input of
             ConcreteBuf i -> concreteModexpGasFee i
             _ -> internalError "Unsupported symbolic modexp gas calc "
    -- ECADD
    0x6 -> g_ecadd
    -- ECMUL
    0x7 -> g_ecmul
    -- ECPAIRING
    0x8 -> (inputLen `div` 192) * g_pairing_point + g_pairing_base
    -- BLAKE2
    0x9 -> case input of
             ConcreteBuf i -> g_fround * (unsafeInto $ asInteger $ lazySlice 0 4 i)
             _ -> internalError "Unsupported symbolic blake2 gas calc"
    _ -> internalError $ "unimplemented precompiled contract " ++ show precompileAddr

-- Gas cost of memory expansion
memoryCost :: FeeSchedule Word64 -> Word64 -> Word64
memoryCost FeeSchedule{..} byteCount =
  let
    wordCount = ceilDiv byteCount 32
    linearCost = g_memory * wordCount
    quadraticCost = div (wordCount * wordCount) 512
  in
    linearCost + quadraticCost

hashcode :: ContractCode -> Expr EWord
hashcode (UnknownCode a) = CodeHash a
hashcode (InitCode ops args) = keccak $ (ConcreteBuf ops) <> args
hashcode (RuntimeCode (ConcreteRuntimeCode ops)) = keccak (ConcreteBuf ops)
hashcode (RuntimeCode (SymbolicRuntimeCode ops)) = keccak . Expr.fromList $ ops

-- | The length of the code ignoring any constructor args.
-- This represents the region that can contain executable opcodes
opslen :: ContractCode -> Int
opslen (UnknownCode _) = internalError "Cannot produce concrete opslen for unknown code"
opslen (InitCode ops _) = BS.length ops
opslen (RuntimeCode (ConcreteRuntimeCode ops)) = BS.length ops
opslen (RuntimeCode (SymbolicRuntimeCode ops)) = length ops

-- | The length of the code including any constructor args.
-- This can return an abstract value
codelen :: ContractCode -> Expr EWord
codelen (UnknownCode a) = CodeSize a
codelen c@(InitCode {}) = case toBuf c of
  Just b -> bufLength b
  Nothing -> internalError "impossible"
-- these are never going to be negative so unsafeInto is fine here
codelen (RuntimeCode (ConcreteRuntimeCode ops)) = Lit . unsafeInto $ BS.length ops
codelen (RuntimeCode (SymbolicRuntimeCode ops)) = Lit . unsafeInto $ length ops

toBuf :: ContractCode -> Maybe (Expr Buf)
toBuf (UnknownCode _) = Nothing
toBuf (InitCode ops args) = Just $ ConcreteBuf ops <> args
toBuf (RuntimeCode (ConcreteRuntimeCode ops)) = Just $ ConcreteBuf ops
toBuf (RuntimeCode (SymbolicRuntimeCode ops)) = Just $ Expr.fromList ops

codeloc :: EVM t s CodeLocation
codeloc = do
  vm <- get
  pure (vm.state.contract, vm.state.pc)

createAddress :: Expr EAddr -> Maybe W64 -> EVM t s (Expr EAddr)
createAddress (LitAddr a) (Just n) = pure $ Concrete.createAddress a n
createAddress (GVar _) _ = internalError "Unexpected GVar"
createAddress _ _ = freshSymAddr

create2Address :: Expr EAddr -> W256 -> ByteString -> EVM t s (Expr EAddr)
create2Address (LitAddr a) s b = pure $ Concrete.create2Address a s b
create2Address (SymAddr _) _ _ = freshSymAddr
create2Address (GVar _) _ _ = internalError "Unexpected GVar"

freshSymAddr :: EVM t s (Expr EAddr)
freshSymAddr = do
  modifying (#env % #freshAddresses) (+ 1)
  n <- use (#env % #freshAddresses)
  pure $ SymAddr ("freshSymAddr" <> (pack $ show n))

isPrecompileAddr :: Expr EAddr -> Bool
isPrecompileAddr = \case
  LitAddr a -> 0x0 < a && a <= 0x09
  SymAddr _ -> False
  GVar _ -> internalError "Unexpected GVar"

-- * Arithmetic

ceilDiv :: (Num a, Integral a) => a -> a -> a
ceilDiv m n = div (m + n - 1) n

allButOne64th :: (Num a, Integral a) => a -> a
allButOne64th n = n - div n 64

log2 :: FiniteBits b => b -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x

writeMemory :: MutableMemory s -> Int -> ByteString -> EVM t s ()
writeMemory memory offset buf = do
  memory' <- expandMemory (offset + BS.length buf)
  mapM_ (uncurry (VUnboxed.Mutable.write memory'))
        (zip [offset..] (BS.unpack buf))
  where
  expandMemory targetSize = do
    let toAlloc = targetSize - VUnboxed.Mutable.length memory
    if toAlloc > 0 then do
      memory' <- VUnboxed.Mutable.grow memory toAlloc
      assign (#state % #memory) (ConcreteMemory memory')
      pure memory'
    else
      pure memory

freezeMemory :: MutableMemory s -> EVM t s (Expr Buf)
freezeMemory memory =
  ConcreteBuf . BS.pack . VUnboxed.toList <$> VUnboxed.freeze memory


instance VMOps Symbolic where
  burn' _ continue = continue
  burnExp _ continue = continue
  burnSha3 _ continue = continue
  burnCalldatacopy _ continue = continue
  burnCodecopy _ continue = continue
  burnExtcodecopy _ _ continue = continue
  burnReturndatacopy _ continue = continue
  burnLog _ _ continue = continue

  initialGas = ()
  ensureGas _ continue = continue
  gasTryFrom _ = Right ()
  costOfCreate _ _ _ _ = ((), ())
  costOfCall _ _ _ _ _ _ continue = continue 0 0
  reclaimRemainingGasAllowance _ = pure ()
  payRefunds = pure ()
  pushGas = do
    modifying (#env % #freshGasVals) (+ 1)
    n <- use (#env % #freshGasVals)
    pushSym $ Expr.Gas n
  enoughGas _ _ = True
  subGas _ _ = ()
  toGas _ = ()
  whenSymbolicElse a _ = a

  partial e = assign #result (Just (Unfinished e))
  branch cond continue = do
    loc <- codeloc
    pathconds <- use #constraints
    query $ PleaseAskSMT cond pathconds (choosePath loc)
    where
      condSimp = Expr.simplify cond
      condSimpConc = Expr.concKeccakSimpExpr condSimp
      choosePath loc (Case v) = do
        assign #result Nothing
        pushTo #constraints $ if v then Expr.simplifyProp (condSimpConc ./= Lit 0)
                                   else Expr.simplifyProp (condSimpConc .== Lit 0)
        (iteration, _) <- use (#iterations % at loc % non (0,[]))
        stack <- use (#state % #stack)
        assign (#cache % #path % at (loc, iteration)) (Just v)
        assign (#iterations % at loc) (Just (iteration + 1, stack))
        continue v
      -- Both paths are possible; we ask for more input
      choosePath loc Unknown =
        choose . PleaseChoosePath condSimp $ choosePath loc . Case

instance VMOps Concrete where
  burn' n continue = do
    available <- use (#state % #gas)
    if n <= available
      then do
        #state % #gas %= (subtract n)
        #burned %= (+ n)
        continue
      else
        vmError (OutOfGas available n)

  burnExp (forceLit -> exponent) continue = do
    FeeSchedule {..} <- gets (.block.schedule)
    let cost = if exponent == 0
          then g_exp
          else g_exp + g_expbyte * unsafeInto (ceilDiv (1 + log2 exponent) 8)
    burn cost continue

  burnSha3 (forceLit -> xSize) continue = do
    FeeSchedule {..} <- gets (.block.schedule)
    burn (g_sha3 + g_sha3word * ceilDiv (unsafeInto xSize) 32) continue

  burnCalldatacopy (forceLit -> xSize) continue = do
    FeeSchedule {..} <- gets (.block.schedule)
    burn (g_verylow + g_copy * ceilDiv (unsafeInto xSize) 32) continue

  burnCodecopy (forceLit -> n) continue =
    case tryFrom n of
      Left _ -> vmError IllegalOverflow
      Right n' -> do
        FeeSchedule {..} <- gets (.block.schedule)
        if n' <= ( (maxBound :: Word64) - g_verylow ) `div` g_copy * 32 then
          burn (g_verylow + g_copy * ceilDiv (unsafeInto n) 32) continue
        else vmError IllegalOverflow

  burnExtcodecopy extAccount (forceLit -> codeSize) continue = do
    FeeSchedule {..} <- gets (.block.schedule)
    acc <- accessAccountForGas extAccount
    let cost = if acc then g_warm_storage_read else g_cold_account_access
    burn (cost + g_copy * ceilDiv (unsafeInto codeSize) 32) continue

  burnReturndatacopy (forceLit -> xSize) continue = do
    FeeSchedule {..} <- gets (.block.schedule)
    burn (g_verylow + g_copy * ceilDiv (unsafeInto xSize) 32) continue

  burnLog (forceLit -> xSize) n continue = do
    case tryFrom xSize of
      Right sz -> do
        FeeSchedule {..} <- gets (.block.schedule)
        burn (g_log + g_logdata * sz + (into n) * g_logtopic) continue
      _ -> vmError IllegalOverflow

  initialGas = 0

  ensureGas amount continue = do
    availableGas <- use (#state % #gas)

    if availableGas <= amount then
      vmError (OutOfGas availableGas amount)
    else continue

  gasTryFrom (forceLit -> w256) =
    case tryFrom w256 of
      Left _ -> Left ()
      Right a -> Right a

  -- Gas cost of create, including hash cost if needed
  costOfCreate (FeeSchedule {..}) availableGas size hashNeeded = (createCost, initGas)
    where
      byteCost   = if hashNeeded then g_sha3word + g_initcodeword else g_initcodeword
      createCost = g_create + codeCost
      codeCost   = byteCost * (ceilDiv (unsafeInto (forceLit size)) 32)
      initGas    = allButOne64th (availableGas - createCost)

  -- Gas cost function for CALL, transliterated from the Yellow Paper.
  costOfCall (FeeSchedule {..}) recipientExists (forceLit -> xValue) availableGas xGas target continue = do
    acc <- accessAccountForGas target
    let call_base_gas = if acc then g_warm_storage_read else g_cold_account_access
        c_new = if not recipientExists && xValue /= 0
              then g_newaccount
              else 0
        c_xfer = if xValue /= 0  then g_callvalue else 0
        c_extra = call_base_gas + c_xfer + c_new
        c_gascap =  if availableGas >= c_extra
                    then min xGas (allButOne64th (availableGas - c_extra))
                    else xGas
        c_callgas = if xValue /= 0 then c_gascap + g_callstipend else c_gascap
    let (cost, gas') = (c_gascap + c_extra, c_callgas)
    continue cost gas'

  -- When entering a call, the gas allowance is counted as burned
  -- in advance; this unburns the remainder and adds it to the
  -- parent frame.
  reclaimRemainingGasAllowance oldVm = do
    let remainingGas = oldVm.state.gas
    modifying #burned (subtract remainingGas)
    modifying (#state % #gas) (+ remainingGas)

  payRefunds = do
    -- compute and pay the refund to the caller and the
    -- corresponding payment to the miner
    block        <- use #block
    tx           <- use #tx
    gasRemaining <- use (#state % #gas)

    let
      sumRefunds   = sum (snd <$> tx.subState.refunds)
      gasUsed      = tx.gaslimit - gasRemaining
      cappedRefund = min (quot gasUsed 5) sumRefunds
      originPay    = (into $ gasRemaining + cappedRefund) * tx.gasprice
      minerPay     = tx.priorityFee * (into gasUsed)

    modifying (#env % #contracts)
       (Map.adjust (over #balance (Expr.add (Lit originPay))) tx.origin)
    modifying (#env % #contracts)
       (Map.adjust (over #balance (Expr.add (Lit minerPay))) block.coinbase)

  pushGas = do
    vm <- get
    push (into vm.state.gas)

  enoughGas cost gasCap = cost <= gasCap

  subGas gasCap cost = gasCap - cost

  toGas = id

  whenSymbolicElse _ a = a

  partial _ = internalError "won't happen during concrete exec"

  branch (forceLit -> cond) continue = continue (cond > 0)

-- Create symbolic VM from concrete VM
symbolify :: VM Concrete s -> VM Symbolic s
symbolify vm =
  vm { result = symbolifyResult <$> vm.result
     , state  = symbolifyFrameState vm.state
     , frames = symbolifyFrame <$> vm.frames
     , burned = ()
     }

symbolifyFrameState :: FrameState Concrete s -> FrameState Symbolic s
symbolifyFrameState state = state { gas = () }

symbolifyFrame :: Frame Concrete s -> Frame Symbolic s
symbolifyFrame frame = frame { state = symbolifyFrameState frame.state }

symbolifyResult :: VMResult Concrete s -> VMResult Symbolic s
symbolifyResult result =
  case result of
    HandleEffect _ -> internalError "shouldn't happen"
    VMFailure e -> VMFailure e
    VMSuccess b -> VMSuccess b

forceLit :: Expr EWord -> W256
forceLit (Lit w) = w
forceLit _ = internalError "concrete vm, shouldn't ever happen"

burn :: VMOps t => Word64 -> EVM t s () -> EVM t s ()
burn = burn' . toGas
