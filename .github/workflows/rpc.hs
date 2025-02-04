{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V

import Optics.Core
import EVM (makeVm, symbolify)
import EVM.ABI
import EVM.Fetch
import EVM.SMT
import EVM.Solvers
import EVM.Stepper qualified as Stepper
import EVM.SymExec
import EVM.Test.Utils
import EVM.Solidity (ProjectType(..))
import EVM.Types hiding (BlockNumber, Env)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Unlift
import EVM.Effects

rpcEnv :: Env
rpcEnv = Env { config = defaultConfig }

test :: TestName -> ReaderT Env IO () -> TestTree
test a b = testCase a $ runEnv rpcEnv b

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "rpc"
  [ testGroup "Block Parsing Tests"
    [ testCase "pre-merge-block" $ do
        let block = BlockNumber 15537392
        (cb, numb, basefee, prevRan) <- fetchBlockFrom block testRpc >>= \case
                      Nothing -> internalError "Could not fetch block"
                      Just Block{..} -> return ( coinbase
                                                   , number
                                                   , baseFee
                                                   , prevRandao
                                                   )

        assertEqual "coinbase" (LitAddr 0x608cfC1575b56a82a352f14d61be100FA9709D75) cb
        assertEqual "number" (BlockNumber numb) block
        assertEqual "basefee" 38572377838 basefee
        assertEqual "prevRan" 11049842297455506 prevRan
    , testCase "post-merge-block" $ do
        let block = BlockNumber 16184420
        (cb, numb, basefee, prevRan) <- fetchBlockFrom block testRpc >>= \case
                      Nothing -> internalError "Could not fetch block"
                      Just Block{..} -> return ( coinbase
                                                   , number
                                                   , baseFee
                                                   , prevRandao
                                                   )

        assertEqual "coinbase" (LitAddr 0x690b9a9e9aa1c9db991c7721a92d351db4fac990) cb
        assertEqual "number" (BlockNumber numb) block
        assertEqual "basefee" 22163046690 basefee
        assertEqual "prevRan" 0x2267531ab030ed32fd5f2ef51f81427332d0becbd74fe7f4cd5684ddf4b287e0 prevRan
    ]
  , testGroup "execution with remote state"
    -- execute against remote state from a ds-test harness
    [ test "dapp-test" $ do
        let testFile = "test/contracts/pass/rpc.sol"
        res <- runSolidityTestCustom testFile ".*" Nothing Nothing False testRpcInfo Foundry
        liftIO $ assertEqual "test result" True res

    -- concretely exec "transfer" on WETH9 using remote rpc
    -- https://etherscan.io/token/0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2#code
    , test "weth-conc" $ do
        let
          blockNum = 16198552
          wad = 0x999999999999999999
          calldata' = ConcreteBuf $ abiMethod "transfer(address,uint256)" (AbiTuple (V.fromList [AbiAddress (Addr 0xdead), AbiUInt 256 wad]))
        vm <- liftIO $ weth9VM blockNum (calldata', [])
        postVm <- withSolvers Z3 1 1 Nothing $ \solvers ->
          Stepper.interpret (oracle solvers (Just (BlockNumber blockNum, testRpc))) vm Stepper.runFully
        let
          wethStore = (fromJust $ Map.lookup (LitAddr 0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2) postVm.env.contracts).storage
          wethStore' = case wethStore of
            ConcreteStore s -> s
            _ -> internalError "Expecting concrete store"
          receiverBal = fromJust $ Map.lookup (keccak' (word256Bytes 0xdead <> word256Bytes 0x3)) wethStore'
          msg = case postVm.result of
            Just (VMSuccess m) -> m
            _ -> internalError "VMSuccess expected"
        liftIO $ do
          assertEqual "should succeed" msg (ConcreteBuf $ word256Bytes 0x1)
          assertEqual "should revert" receiverBal (W256 $ 2595433725034301 + wad)

    -- symbolically exec "transfer" on WETH9 using remote rpc
    -- https://etherscan.io/token/0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2#code
    , test "weth-sym" $ do
        let
          blockNum = 16198552
          calldata' = symCalldata "transfer(address,uint256)" [AbiAddressType, AbiUIntType 256] ["0xdead"] (AbstractBuf "txdata")
          postc _ (Failure _ _ (Revert _)) = PBool False
          postc _ _ = PBool True
        vm <- liftIO $ weth9VM blockNum calldata'
        (_, [Cex (_, model)]) <- withSolvers Z3 1 1 Nothing $ \solvers ->
          verify solvers (rpcVeriOpts (BlockNumber blockNum, testRpc)) (symbolify vm) (Just postc)
        liftIO $ assertBool "model should exceed caller balance" (getVar model "arg2" >= 695836005599316055372648)
    ]
  ]

-- call into WETH9 from 0xf04a... (a large holder)
weth9VM :: W256 -> (Expr Buf, [Prop]) -> IO (VM Concrete RealWorld)
weth9VM blockNum calldata' = do
  let
    caller' = LitAddr 0x608cfC1575b56a82a352f14d61be100FA9709D75
    weth9 = Addr 0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2
    callvalue' = Lit 1000
  vmFromRpc blockNum calldata' callvalue' caller' weth9

vmFromRpc :: W256 -> (Expr Buf, [Prop]) -> Expr EWord -> Expr EAddr -> Addr -> IO (VM Concrete RealWorld)
vmFromRpc blockNum calldata callvalue caller address = do
  ctrct <- fetchContractFrom (BlockNumber blockNum) testRpc address >>= \case
        Nothing -> internalError $ "contract not found: " <> show address
        Just contract' -> return contract'

  blk <- fetchBlockFrom (BlockNumber blockNum) testRpc >>= \case
    Nothing -> internalError "could not fetch block"
    Just b -> pure b

  stToIO $ (makeVm $ VMOpts
    { contract       = ctrct
    , otherContracts = []
    , calldata       = calldata
    , value          = callvalue
    , address        = LitAddr address
    , caller         = caller
    , origin         = LitAddr 0xacab
    , gas            = 0xffffffffffffffff
    , gaslimit       = 0xffffffffffffffff
    , baseFee        = blk.baseFee
    , priorityFee    = 0
    , coinbase       = blk.coinbase
    , number         = blk.number
    , timestamp      = blk.timestamp
    , blockGaslimit  = blk.gaslimit
    , gasprice       = 0
    , maxCodeSize    = blk.maxCodeSize
    , prevRandao     = blk.prevRandao
    , schedule       = blk.schedule
    , chainId        = 1
    , create         = False
    , baseState      = EmptyBase
    , txAccessList   = mempty
    , allowFFI       = False
    , freshAddresses = 0
    , beaconRoot     = 0
    }) <&> set (#cache % #fetched % at address) (Just ctrct)

testRpc :: Versoriumx
testRpc = "https://eth-mainnet.alchemyapi.io/v2/yqsHhEQ9lFPKr9SSD8BooYazsQWHUZUQ
"

testRpcInfo :: RpcInfo
testRpcInfo = Just (BlockNumber 16198552, testRpc)
