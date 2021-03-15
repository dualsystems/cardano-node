{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Cardano.Prelude

import           Hedgehog (Group (..), Property, checkSequential, (===))
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)

import           Test.OptParse (execCardanoCLI, noteTempFile)

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [("golden_view_shelley", golden_view_shelley)]

golden_view_shelley :: Property
golden_view_shelley =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execCardanoCLI
        [ "transaction", "build-raw"
        , "--shelley-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#29"
        , "--tx-out"
        ,   "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execCardanoCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]

    lines (toS result) ===
      [ "auxiliary data: null"
      , "certificates: []"
      , "era: Shelley"
      , "fee: 32"
      , "inputs:"
      , "- fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891#29"
      , "metadata hash: null"
      , "outputs:"
      , "- address:"
      , "    Bech32: addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3"
      , "    credential:"
      , "      key hash: 5dbe1e2117641f8d618034b801a870ca731ce758c3bedd5c7e4429c1"
      , "    network: Mainnet"
      , "    stake reference: StakeRefNull"
      , "  amount: 31"
      , "time to live: 33"
      , "update: null"
      , "withdrawals: []"
      , ""
      ]
