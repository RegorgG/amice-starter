{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Aoe where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude
import           Data.Void                 (Void)


newtype GameId = GameId {
    id :: Integer
} deriving (Show, Generic, FromJSON, ToJSON) -- TODO add deriving

newtype WinnerPubKeyHashBs = WinnerPubKeyHashBs {
    winner :: TokenName
} deriving (Show, Generic, FromJSON, ToJSON) -- TODO add deriving

{-# INLINABLE mkPolicy #-}
mkPolicy :: GameId -> ScriptContext -> Bool
mkPolicy gameId ctx = traceIfFalse "not signed by oracle owner"   isSigned           &&
                       traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oraclePubKeyHash = undefined
    isSigned = txSignedBy info oraclePubKeyHash

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && amt == 1
        _                -> False

policy :: GameId -> Scripts.MonetaryPolicy
policy gameId = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \gameId' -> Scripts.wrapMonetaryPolicy $ mkPolicy gameId' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode gameId


curSymbol :: GameId -> CurrencySymbol
curSymbol gameId = scriptCurrencySymbol $ policy gameId

type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" (GameId, WinnerPubKeyHashBs)

transform :: WinnerPubKeyHashBs -> TokenName
transform :: undefined

mint :: (GameId, WinnerPubKeyHashBs) -> Contract w NFTSchema Text ()
mint (gameId,winnerPkh) = do
    let val     = Value.singleton (curSymbol gameId) winnerTn 1
        lookups = Constraints.monetaryPolicy (policy gameId)
        tx      = Constraints.mustForgeValue val 
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

    where
        winnerTn :: TokenName
        winnerTn = transform winnerPkh



-- data Oracle = Oracle
--     { oSymbol   :: !CurrencySymbol
--     , oOperator :: !PubKeyHash
--     , oFee      :: !Integer
--     , oAsset    :: !AssetClass
--     } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

-- data OracleRedeemer = UseResult
--     deriving Show

-- data Oracling
-- instance Scripts.ScriptType Oracling where
--     type instance DatumType Oracling = Integer
--     type instance RedeemerType Oracling = OracleRedeemer

-- oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
-- oracleInst oracle = Scripts.validator @Oracling
--     ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @Integer @OracleRedeemer

-- oracleValidator :: Oracle -> Validator
-- oracleValidator = Scripts.validatorScript . oracleInst

-- oracleAddress :: Oracle -> Ledger.Address
-- oracleAddress = scriptAddress . oracleValidator

-- data OracleParams = OracleParams
--     { opFees   :: !Integer
--     , opSymbol :: !CurrencySymbol
--     , opToken  :: !TokenName
--     } deriving (Show, Generic, FromJSON, ToJSON)

-- startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
-- startOracle op = do
--     pkh <- pubKeyHash <$> Contract.ownPubKey
--     osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
--     let cs     = Currency.currencySymbol osc
--         oracle = Oracle
--             { oSymbol   = cs
--             , oOperator = pkh
--             , oFee      = opFees op
--             , oAsset    = AssetClass (opSymbol op, opToken op)
--             }
--     logInfo @String $ "started oracle " ++ show oracle
--     return oracle