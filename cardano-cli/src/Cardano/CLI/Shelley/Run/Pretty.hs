{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Shelley.Run.Pretty (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra),
                   ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   ShelleyEra, TxBody, serialiseToBech32)
import           Cardano.Api.Byron (TxBody (ByronTxBody))
import           Cardano.Api.Shelley (TxBody (ShelleyTxBody), fromShelleyAddr)
import           Cardano.Binary (Annotated)
import           Cardano.CLI.Helpers (textShow)
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Shelley as Ledger (ShelleyEra)
import           Cardano.Ledger.ShelleyMA (MaryOrAllegra (Allegra, Mary), ShelleyMAEra)
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMA
import           Data.Aeson (Object, Value (..), object, toJSON, (.=))
import qualified Data.HashMap.Strict as HashMap
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import           Shelley.Spec.Ledger.API (Addr (..), TxOut (TxOut))
import qualified Shelley.Spec.Ledger.API as Shelley

friendlyTxBodyBS :: Api.TxBody era -> ByteString
friendlyTxBodyBS =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody

friendlyTxBody :: Api.TxBody era -> Value
friendlyTxBody = \case
  ByronTxBody tx ->
    case friendlyTxBodyByron tx of
      Object obj -> objectWithEra "Byron" obj
      value      -> object ["era" .= String "Byron", "transaction" .= value]
  ShelleyTxBody ShelleyBasedEraShelley body aux ->
    objectWithEra "Shelley" $ addAuxData aux $ friendlyTxBodyShelley body
  ShelleyTxBody ShelleyBasedEraAllegra body aux ->
    objectWithEra "Allegra" $ addAuxData aux $ friendlyTxBodyAllegra body
  ShelleyTxBody ShelleyBasedEraMary body aux ->
    objectWithEra "Mary" $ addAuxData aux $ friendlyTxBodyMary body

objectWithEra :: Text -> Object -> Value
objectWithEra era = Object . HashMap.insert "era" (String era)

addAuxData :: Show a => Maybe a -> Object -> Object
addAuxData = HashMap.insert "auxiliary data" . maybe Null (toJSON . textShow)

friendlyTxBodyByron :: Annotated Byron.Tx ByteString -> Value
friendlyTxBodyByron = toJSON

friendlyTxBodyShelley
  :: Shelley.TxBody (Ledger.ShelleyEra StandardCrypto) -> Object
friendlyTxBodyShelley body =
  HashMap.fromList
    [ "inputs" .= Shelley._inputs body
    , "outputs" .= fmap friendlyTxOutShelley (Shelley._outputs body)
    , "certificates" .= fmap textShow (Shelley._certs body)
    , "withdrawals" .= Shelley.unWdrl (Shelley._wdrls body)
    , "fee" .= Shelley._txfee body
    , "time to live" .= Shelley._ttl body
    , "update" .= fmap textShow (Shelley._txUpdate body)
    , "metadata hash" .= fmap textShow (Shelley._mdHash body)
    ]

friendlyTxBodyAllegra
  :: ShelleyMA.TxBody (ShelleyMAEra 'Allegra StandardCrypto) -> Object
friendlyTxBodyAllegra
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    mint) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutAllegra outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyTxBodyMary
  :: ShelleyMA.TxBody (ShelleyMAEra 'Mary StandardCrypto) -> Object
friendlyTxBodyMary
  (ShelleyMA.TxBody
    inputs
    outputs
    certificates
    (Shelley.Wdrl withdrawals)
    txfee
    validity
    update
    adHash
    mint) =
  HashMap.fromList
    [ "inputs" .= inputs
    , "outputs" .= fmap friendlyTxOutMary outputs
    , "certificates" .= fmap textShow certificates
    , "withdrawals" .= withdrawals
    , "fee" .= txfee
    , "validity interval" .= friendlyValidityInterval validity
    , "update" .= fmap textShow update
    , "auxiliary data hash" .= fmap textShow adHash
    , "mint" .= mint
    ]

friendlyValidityInterval :: ShelleyMA.ValidityInterval -> Value
friendlyValidityInterval
  ShelleyMA.ValidityInterval{invalidBefore, invalidHereafter} =
    object
      [ "invalid before" .= invalidBefore
      , "invalid hereafter" .= invalidHereafter
      ]

friendlyTxOutShelley :: TxOut (Ledger.ShelleyEra StandardCrypto) -> Value
friendlyTxOutShelley (TxOut addr amount) =
  Object $ HashMap.insert "amount" (toJSON amount) $ friendlyAddress addr

friendlyAddress :: Addr StandardCrypto -> Object
friendlyAddress addr =
  HashMap.fromList $
    case addr of
      Addr net cred ref ->
        [ ( "address"
          , object
              [ "network" .= net
              , "credential" .= cred
              , "stake reference" .= textShow ref
              , "Bech32" .= addressBech32
              ]
          )
        ]
      AddrBootstrap _ ->
        [("bootstrap address", object ["Bech32" .= String addressBech32])]
  where
    addressBech32 =
      case fromShelleyAddr @Api.ShelleyEra addr of
        AddressInEra (ShelleyAddressInEra _) a -> serialiseToBech32 a
        AddressInEra ByronAddressInAnyEra _ -> panic "expected Shelley address"

friendlyTxOutAllegra :: TxOut (ShelleyMAEra 'Allegra StandardCrypto) -> Value
friendlyTxOutAllegra = toJSON

friendlyTxOutMary :: TxOut (ShelleyMAEra 'Mary StandardCrypto) -> Value
friendlyTxOutMary = toJSON
