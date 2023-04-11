# NFT-Swap-Infrastructure-Templates
Open-source NFT swapping Cardano smart contract templates and matching off-chain code and front-end templates.
Easily adaptable and implementable by any NFT project in the Cardano space.

# Overview of the repository
!!Repository is work in progress!!
The smart contract provided are at this point not a full implementation. Feel free to use as inspiration for your own contracts, but do not useon Cardano mainnet at this time.

* README          - This file
* LICENSE         - License information
* doc             - Documentation about how to build and configure the NFT Swap Infrastructure
* smart-contracts - All code related to the smart contracts to guard your swap pool contents
* web             - Example web site you can customize to allow users to interact with your swap pool

# Prerequesites
We have made effort to keep this repoisitory easy to use. Code has comments to give insight into what is going on.
Documentation has also been added to aid understanding of functionality, to inspire you on how to make changes.
All this, will not take away the fact that to make changes or customizations to the code, you should consult a developer.
Non-developers should follow the instructions to configure the platform for use with standard settings.

## Getting started
The supplied code can be used in your existing development setup if you already have one.
If you do not have a running environment you can use the following instructions to set it up
- [Install Plutus](doc/installing-plutus.md)
- [Building the smart contracts](doc/building-the-smart-contracts.md)

## The contracts
| Contract | Deployment instructions |
| --- | --- |
| [Specific Swap](smart-contracts/src/SpecificSwap.hs) | [Instructions](doc/deploy-specific-swap.md) |
| [Filtered Specific Swap](smart-contracts/src/SpecificSwapFiltered.hs) | [Instructions](doc/deploy-specific-swap-filtered.md) |
