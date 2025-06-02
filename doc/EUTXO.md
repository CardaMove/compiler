---
marp: true
theme: gaia
---

# EUTXO

* UTXO + Validator Script + Payload

---

## ScriptContext

```hs
data ScriptContext
    = ScriptContext
    { scriptContextTxInfo   :: TxInfo
    , scriptContextPurpose  :: ScriptPurpose
    }
```

---

## ScriptPurpose

```hs
data ScriptPurpose
   = Minting CurrencySymbol
   | Spending TxOutRef -- We are interested in this one
   | Rewarding StakingCredential
   | Certifying DCert
```

---

## TxInfo

```hs
data TxInfo 
    = TxInfo
        { txInfoInputs      :: [TxInInfo]   -- ^ Transaction inputs
        , txInfoOutputs     :: [TxOut]      -- ^ Transaction outputs
        , txInfoFee         :: Value        -- ^ The fee paid by this transaction.
        , txInfoForge       :: Value        -- ^ The 'Value' forged by this transaction.
        , txInfoDCert       :: [DCert]      -- ^ Digests of certificates included in this transaction
        , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
        , txInfoValidRange  :: SlotRange    -- ^ The valid range for the transaction.
        , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
        , txInfoData        :: [(DatumHash, Datum)]
        , txInfoId          :: TxId
        -- ^ Hash of the pending transaction (excluding witnesses)
        } deriving (Generic)
```

---

## What happens if the transaction fails?

[Nothing.](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week3.html#txinfovalidrange)