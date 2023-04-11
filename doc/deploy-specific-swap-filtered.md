# Specific Swap Filtered
The `SpecificSwapFiltered` can be configured to only allow swaps of NFTs minted with a specific policy and an optional list of NFT names allowed to swap. The contract logic checks the swap transaction to see that it only swaps what is allowed and prohibits swapping of other NFTs than it was set up to swap.
If the list of TokenNames is empty, all NFTs from a specific policy can be swapped. If it on the other hand contains any TokenNames, only the listed NFTs can be swapped. This version therefore enables to set up a swap pool for only a specific selection of NFTs so that for example only "epic" NFTs from a collection is swappable.

## Prerequisites
- Plutus and nix-shell installed on your computer according to [instructions](installing-plutus.md)
- Local copy of this repository, built according to [building instructions](building-the-basic-smart-contracts-repo.md)
- Cardano node and cli setup according to instructions on [cardano-node repository](https://github.com/input-output-hk/cardano-node)

## Brief
The following instructions are for deploying the built SpecificSwapFiltered to Cardano blockchain.
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

## Configure the Specific Swap Filtered contract
One wallet owns the swap pool. The owner is the only wallet allowed to perform cleanups and eventually emptying the contract. To set a wallet as the owner of the contract, you need to get hold of the payment pub key hash. Also needed is a policy id and a list of NFT names that are allowed to be swapped.
Note: If you do not want to specify a list of NFT names, use the contract SpecificSwap instead.

### Generate payment address for owner
```
~/wallets  : cardano-cli address key-gen --normal-key --verification-key-file owner-wallet.vkey --signing-key-file owner-wallet.skey
~/wallets  : cardano-cli address build --payment-verification-key-file owner-wallet.vkey $MAGIC --out-file owner-wallet.addr
```

### Fetch Payment pub key hash for owner address
```
~/wallets  : cardano-cli address key-hash --payment-verification-key-file owner-wallet.vkey --out-file owner-wallet.pkh
```

The contents of your owner-wallet.pkh should now be a 56 byte hex, similar to but not identical to `11444c05a756813e49973f5e60a4ba8bea0ebcd4f98bd5724f395f78`. 
All wallets have their own unique key hash

## Serialize the plutus script
Time has come to build your unique swap pool contract. This is accomplished with the following command.
This works in the way that the `specific-swap-filtered` executable compiles your contract using four parameters (following --)
| Parameter | Description | Example |
| --- | --- | --- |
| 1 | filename to save your plutus script as | `plutus-scripts/specificswap-0-12-2.plutus` |
| 2 | owner wallet payment pub key hash | `11444c05a756813e49973f5e60a4ba8bea0ebcd4f98bd5724f395f78` |
| 3 | policy id of NFTs that are allowed to swap | `949d2a36bcd1a2398e9b72cd5e822d851b0d50b4b48465676bd2c2a7` |
| 4 | list of NFT names that are allowed to swap | `4d657263757279,56656e7573,4561727468,4d617273` |


In this example, we have a policy containing all planets of the solar system, but we want to set up the swap pool to only allow swaps of the four innermost planets. We therefore supply the list of NFT names separated by comma (,)
```
[nix-shell:~/NFT-Swap-Infrastructure-Templates/smart-contracts]$ cabal exec specific-swap-filtered -- plutus-scripts/specificswap-0-12-2.plutus 11444c05a756813e49973f5e60a4ba8bea0ebcd4f98bd5724f395f78 949d2a36bcd1a2398e9b72cd5e822d851b0d50b4b48465676bd2c2a7 4d657263757279,56656e7573,4561727468,4d617273
_______________________________________________
 Contract saved to file        : plutus-scripts/specificswap-0-12-2.plutus
 PubKeyHash of owner wallet    : 11444c05a756813e49973f5e60a4ba8bea0ebcd4f98bd5724f395f78
 Desired Policy ID             : 949d2a36bcd1a2398e9b72cd5e822d851b0d50b4b48465676bd2c2a7
 Parameter to contract         : ContractParam {contractOwner = 11444c05a756813e49973f5e60a4ba8bea0ebcd4f98bd5724f395f78, desiredPolicyID = 949d2a36bcd1a2398e9b72cd5e822d851b0d50b4b48465676bd2c2a7, tokensAllowedToSwap = ["Mercury","Venus","Earth","Mars"]}
 contractOwner       (obj type): PaymentPubKeyHash
 desiredPolicyID     (obj type): CurrencySymbol
 tokensAllowedToSwap (obj type): [TokenName]
_______________________________________________

[nix-shell:~/NFT-Swap-Infrastructure-Templates/smart-contracts]$ 
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
~/smart-contracts  : cardano-cli address build --payment-script-file smart-contracts/specificswap-0-12-2.plutus $MAGIC --out-file smart-contracts/specificswap-0-12-2.addr
~/smart-contracts  : cat  smart-contracts/specificswap-0-12-2.addr
addr_test1wqcc0xrqz2xmm0f3v7ykgvdx2sq9xs6u2zawlz2smtumxvgme3gjt
~/smart-contracts  : 
```
Once again, the address `addr_test1wqcc0xrqz2xmm0f3v7ykgvdx2sq9xs6u2zawlz2smtumxvgme3gjt` is only an example of how your address should look.
You are now ready for configuring the dapp web interface to allow interaction with your swap pool
