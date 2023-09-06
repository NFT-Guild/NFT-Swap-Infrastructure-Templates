# NFT-Swap-Infrastructure-Templates
Open-source NFT swapping Cardano smart contract templates and matching off-chain code and front-end templates.
Easily adaptable and implementable by any NFT project in the Cardano space.

# Overview of the repository
* README          - This file
* LICENSE         - License information
* smart-contracts - Swap pool smart contracts that validates that swaps are done according to the rules
* dapp             - Example web site you can customize to allow users to interact with your swap pool

# Prerequesites
We have made effort to keep this repoisitory easy to use. Code has comments to give insight into what is going on.
Documentation has also been added to aid understanding of functionality, to inspire you on how to make changes.
All this, will not take away the fact that to make changes or customizations to the code, you should consult a developer.
Non-developers should follow the instructions to configure the platform for use with standard settings.

## Getting started
The supplied code can be used in your existing development setup if you already have one.
Dapp code is based on nodejs and pug templates.

Start the dapp on you web server and navigate to the page https://<your-domain:port>/admin to get started.
**We recommend to always do a first test of the platform in the preproduction environment so you get aquainted with setting up the dapp and to see how it works.**

### Policy wide swap pool
To set up a swap pool that allows swaps between all NFTs of a certain policy, insert the policy id into the field and click the CREATE POOL button. You are then presented with a dialog containing the generated smart contract code and contract address. Copy these values into the appointed constants in the files specific_swap.js and wallet-integration.js and reload the dapp to publish your changes.
This is a short [demo](https://www.youtube.com/watch?v=VK_v6FuDGCg&list=PL9yobT1b_0gxoW81cavVgqYZw-Fi8yf-q&index=1) of the steps

### Explicitly named NFTs swap pool (filtered)
To set up a filtered swap pool that allows swaps between explicitly named NFTs of a policy, insert the policy id into the field and click the LOAD button. All NFTs of the policy are then loaded. Then select all the NFTs that the swap pool should allow. When you are done, click the CREATE FILTERED POOL button. You are then presented with a dialog containing the generated smart contract code, the list of allowed NFT names and contract address. Copy these values into the appointed constants in the files specific_swap.js and wallet-integration.js and reload the dapp to publish your changes.
To see how this is done, you can watch this short [demo](https://www.youtube.com/watch?v=2UCtS4nEUP4&list=PL9yobT1b_0gxoW81cavVgqYZw-Fi8yf-q&index=5) of the steps

### Rule Based NFT swap pool
To set up a rule based swap pool you need to define the rule set for which NFTs that are allowed to swap. This is particularly powerful when you want to allow swaping of NFTs from numbered ranges, for example 'NFTName00100' to 'NFTName04500'. The only requirement is that all of the NFTs belong to the same policy and that they have the same naming structure and a number is part of the name. To get started, navigate to the Admin module and click the "CREATE RULE POOL" button. You are then presented with a dialog where you need to input the rules for the NFTs to allow. Copy/paste the policy id and provide the prefix that all nfts have in common ('NFTName' from our example above). The dialog then suggests the position of the number based on the length of the prefix. This can be changed if there are some following part of the name that is not constant for all NFTs. You will in that case see 'ABCD...Z' up until the number position is reached. ABCD.. is only an indication that this is a random text. Then you need to set the length of the digit. In the example above, we have 00100 to 04500, so we would then need to specify 5 as the length for this digit. Then specify the value 100 as DIGIT START RANGE and 4500 as DIGIT END RANGE.
When you have specified this, you can see example NFT name according to your set rules on the bottom of the dialog. If this example looks ok, check the "I have verified the rule and example to be OK". This will perform a check that all fields have a correct value and if so, you can click the "CREATE RULE POOL" botton to confirm the creation.
You are then presented with a dialog containing the generated smart contract code, the rules of the allowed NFTs and contract address. Copy these values into the appointed constants in the files specific_swap.js and wallet-integration.js and reload the dapp to publish your changes.
(We will soon publish a video tutorial that shows this process...)


### Recommended step before adding NFTs to your swap pool
To verify that the generated smart contract is working as expected, verify this by doing test deposits of ADA to the contract with the TEST DEPOSIT button and then update the code of the TEST WITHDRAWAL button to withdraw the ADA deposit UTxO you just did. If this is successful (meaning you withdraw about the same amount you deposited) you have verified that the contract recognize you as the owner of the swap pool and hence can remove NFTs from the swap pool. 

## The contracts
| Contract | Deployment instructions |
| --- | --- |
| [Specific Swap](smart-contracts/SpecificSwap.hs) | The base contract used for swap pools allowing swaps of all policy NFTs |
| [Filtered Specific Swap](smart-contracts/SpecificSwapFiltered.hs) | The base contract of swap pools allowing swap of explicitly named NFTs of a policy |
| [Rule Based Specific Swap](smart-contracts/SpecificSwapTokenNameRule.hs) | The base contract of swap pools allowing swap of NFTs of a policy with names starting with a defined text and specified number series |
