# Specific Swap
The `SpecificSwap` can be configured to only allow swaps of NFTs minted with a specific policy. The contract logic checks the swap transaction to see that it only swaps what is allowed and prohibits swapping of other NFTs than it was set up to swap.

## Prerequisites
- Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md)
- Local copy of this repository, built according to [building instructions](building-the-basic-smart-contracts-repo.md)
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)

## Brief
The following instructions are for deploying the built SpecificSwap to Cardano blockchain.
Instructions are the same for all of the Cardano Blockchains, but you need to change the
"Network Magic" parameter according to your environment
Use one of the following Network Magics according to which network you want to work
| Network | Magic | Command |
| --- | --- | --- |
| Preview | `--testnet-magic 2` | `export MAGIC="--testnet-magic 2"` |
| Pre-Production | `--testnet-magic 1` | `export MAGIC="--testnet-magic 1"` |
| Mainnet | `--mainnet` | `export MAGIC="--mainnet"` |

The Cardano CLI commands are compatible with version 1.35.3
```
~  : cardano-cli --version
cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
git rev 950c4e222086fed5ca53564e642434ce9307b0b9
```

## Configure the Specific Swap
One wallet owns the swap pool. The owner is the only wallet allowed to perform cleanups and eventually emptying the contract. To set a wallet as the owner of the contract, you need to get hold of the payment pub key hash and also specify the policy id that is allowed to be swapped

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file owner-wallet.vkey --signing-key-file owner-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file owner-wallet.vkey $MAGIC --out-file owner-wallet.addr
```

### Fetch Payment pub key hash for owner address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file owner-wallet.vkey --out-file owner-wallet.pkh
```

The contents of your owner-wallet.pkh should now be a 56 byte hex, similar to but not identical to `c6575a86d6864038828bd3be29afc435247cb81af2e4ee2d5d8e861a`. 
All wallets have their own unique key hash

## Serialize the plutus script
Time has come to build your unique swap pool contract. This is accomplished with the following command.
This works in the way that the `specific-swap` executable compiles your contract using three parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/specificswap-0-6.plutus` |
| 2 | owner wallet payment pub key hash | `c6575a86d6864038828bd3be29afc435247cb81af2e4ee2d5d8e861a` |
| 3 | policy id of NFTs that are allowed to swap | `49eb4b489f32e50dd566b54db4a6f687a6438a4904df7e3d2dd88fd6`

```
[nix-shell:~/basic-smart-contracts]$ cabal exec specific-swap -- plutus-scripts/specificswap-0-6.plutus c6575a86d6864038828bd3be29afc435247cb81af2e4ee2d5d8e861a 49eb4b489f32e50dd566b54db4a6f687a6438a4904df7e3d2dd88fd6
_______________________________________________
 Contract saved to file     : plutus-scripts/specificswap-0-6.plutus
 PubKeyHash of owner wallet : c6575a86d6864038828bd3be29afc435247cb81af2e4ee2d5d8e861a
 Desired Policy ID          : 49eb4b489f32e50dd566b54db4a6f687a6438a4904df7e3d2dd88fd6
 Parameter to contract      : ContractParam {contractOwner = c6575a86d6864038828bd3be29afc435247cb81af2e4ee2d5d8e861a, desiredPolicyID = 49eb4b489f32e50dd566b54db4a6f687a6438a4904df7e3d2dd88fd6}
 contractOwner    (obj type): PaymentPubKeyHash
 desiredPolicyID  (obj type): CurrencySymbol
_______________________________________________


[nix-shell:~/basic-smart-contracts]$ 
```
The contents of your contract plutus script file should now look similar to
```
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "5910da5910d701000033232332233223232333222323332223233333333222222223233322232333322223232332232333222323332223232
    <shortened for readability>
    81af2e4ee2d5d8e861a0048811c49eb4b489f32e50dd566b54db4a6f687a6438a4904df7e3d2dd88fd60022123300100300220011"
}

```

## Generating contract script address
The final step is to generate the contract script address. This address is used when you interact with it

```
~/smart-contracts  : cardano-cli address build --payment-script-file specificswap-0-6.plutus $MAGIC --out-file specificswap-0-6.addr
~/smart-contracts  : specificswap-0-6.addr 
addr_test1wq2pc9t9kh42l39f3p8mv0jdtykazjce6vp2dmm49l4256quhfhfy
~/smart-contracts  : 
```
Once again, the address `addr_test1wq2pc9t9kh42l39f3p8mv0jdtykazjce6vp2dmm49l4256quhfhfy` is only an example of how your address should look.
You are now ready for interacting with your swap pool
