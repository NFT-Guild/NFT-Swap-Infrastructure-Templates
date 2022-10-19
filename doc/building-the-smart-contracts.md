# Building the smart contracts

## Prerequisites
Plutus and nix-shell installed on your computer

## Compiling the contracts
Change directory into the folder of your local clone of the plutus-apps repository
```bash
cd plutus-apps
nix-shell
```

When nix-shell is running, change directory into the smart-contracts folder of the NFT swap pool templates repository
```bash
cd <directory of nft swap pools directory>/smart-contracts
cabal build
```

## Assigning an owner-wallet to your contract
Before you can use your contract, a wallet must be assigned to play the role of owning the contract and its contents.
<instructions on how to serialize and configure the contract here...>


## Generate contract address
<instructions on how to determine contract address...>
