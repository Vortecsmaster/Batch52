{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module Batch52NFTBounty where

import           Control.Monad             (void)
import Data.Default               (Default (..))
import Data.Text                  (Text)
import Data.Void
import Data.Map as Map
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import           Plutus.Contract
import           Plutus.Trace.Emulator  as Emulator
import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts  -- Ledger.V1.Utils.
import           Ledger.Ada                as Ada
import           Ledger.Value           as Value
import           Playground.Contract
import qualified Prelude
import Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import Ledger.TimeSlot
import qualified Plutus.Trace as Trace
import Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras
import PlutusTx.IsData.Class (toBuiltinData)

--ON-CHAIN

data MathBountyDatum = MathBountyD
    { mBounty :: Integer
    , deadline :: POSIXTime
    } 
PlutusTx.unstableMakeIsData ''MathBountyDatum 

{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool
mathBountyValidator datum x sContext =  traceIfFalse "Wrong guess!" ((mBounty datum) == x*x) &&
                                        traceIfFalse "Deadline passed!" deadlineReached 
    -- && filter that creator can not take value
                                       -- || (condition1 -> creator wallet &&
                                       --    contidion2 -> after deadline)
        where
            info :: TxInfo
            info = scriptContextTxInfo sContext

            deadlineReached :: Bool
            deadlineReached = contains (to $ deadline datum) $ txInfoValidRange info

{-# INLINABLE nftMintingPolicy  #-}
nftMintingPolicy :: TxOutRef -> TokenName ->  () -> ScriptContext -> Bool
nftMintingPolicy oref tname _ sContext = traceIfFalse "UTxO not consumed" hasUTxO               &&
                                         traceIfFalse "Wrong ammount minted" checkMintedAmount 
    where
      info :: TxInfo
      info = scriptContextTxInfo sContext

      hasUTxO :: Bool
      hasUTxO = any (\utxo -> txInInfoOutRef utxo == oref) $ txInfoInputs info

      checkMintedAmount :: Bool
      checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tname', amount)]   ->  tname' == tname && amount == 1
        _                       ->  False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tname = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| \oref' tname' -> Scripts.wrapMintingPolicy $ nftMintingPolicy oref' tname' ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode oref
             `PlutusTx.applyCode`
             PlutusTx.liftCode tname

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tname = scriptCurrencySymbol $ policy oref tname


data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.wrapValidator @MathBountyDatum @Integer -- mkUntypedValidator on V2 ??


validator :: Validator
validator = Scripts.validatorScript bountyValidator

bountyScript :: Ledger.ValidatorHash  --Script  
bountyScript = Scripts.validatorHash bountyValidator --unValiatorScript validator

bountyAddress :: Ledger.Address       --Address
bountyAddress = scriptAddress validator  --Ledger.scriptAddress validator

--OFF-CHAIN
data BountyParams = BP 
                  { bMathBounty :: Integer
                  , bAmount     :: Integer
                  , bDeadline   :: POSIXTime
                  } deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

type MathBountySchema = 
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer

bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP bounty amount deadline) = do
    let datum = MathBountyD bounty deadline
        tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTxConstraints bountyValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Some bounty created of amount =  %s" (show amount)

--data SolutionParams = SP { guess :: Integer }

solution ::  Integer -> Contract () MathBountySchema Text ()
solution guess = do
                 now <- currentTime
                 utxos <- utxosAt bountyAddress
                 logInfo @String $ printf "The utxos: %s " (show $ Map.toList utxos )
                 case Map.toList utxos of
                  []             -> logInfo @String $ printf "No UTxOs on the Contract!"
                  (oref,a):utxos -> do
                                    let tname = TokenName "Batch52BountyWinner"
                                        val = Value.singleton (curSymbol oref tname) tname 1 
                                        lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                                                  Constraints.otherScript validator <>
                                                  Constraints.mintingPolicy (policy oref tname)
                                        tx = Constraints.mustSpendScriptOutput oref (Redeemer $ toBuiltinData guess) <>
                                             Constraints.mustValidateIn (to now) <>
                                             Constraints.mustMintValue val 
                                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ printf "Proposed solution is: %s" (show guess)                    
 
endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' =  endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema
mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"bounty" h1 $ BP
                    { bMathBounty = 100
                  , bAmount     = 51000000
                  , bDeadline   = 1596059101000 
                    }
    void $ Emulator.waitNSlots 5
    callEndpoint @"solution" h2 $ 5
    void $ Emulator.waitNSlots 2
    callEndpoint @"solution" h3 $ 10
    void $ Emulator.waitNSlots 11  

 -- t0 = 1596059091000, and every second is 1000