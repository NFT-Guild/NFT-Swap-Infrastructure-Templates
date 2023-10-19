# Required system platform tools

The dapp is a standard [nodejs](https://nodejs.org/en/download/package-manager) project that can be installed on your server.
We also like using [PM2 process manager](https://pm2.keymetrics.io/) to manage the nodejs applications

# Installation instructions
Make sure you have nodejs running on your server before continuing. Then continue with the instructions below to install the dapp.

## Clone the GitHub project.
Typical command for this can be
```bash
git clone https://github.com/NFT-Guild/NFT-Swap-Infrastructure-Templates.git
```
## Add HTTPS certificate
Add your HTTPS certificate to the folder dapp/keys. The current setup requires that you provide two files (swappool.key and swappool.pem). 

## Configure the dapp (specific_swap.js)
Open the file dapp/specific_swap.js and make the appropriate settings
* Set the correct name for the certificate files if they are named anything else than swappool.key and swappool.pem
* Port of the HTTPS server. (Also make sure to open this port in your firewall if it is closed)

## wallet_integration.js
The file wallet_integration.js contains the required settings and code for the off-chain wallet connection code. This code makes us of the Spacebudz Lucid library which in turn requires Blockfrost. You will need a [Blockfrost API](https://blockfrost.io/#pricing) user account. For running one project you will only need one API key which is free. Select the plan that fit your needs

When you have the api key, open the file dapp/public/javascript/wallet_integration.js and set the appropriate values for the environment and key constants at the top of the file to reflect your environment
* blockfrost_api_url
* blockfrost_api_key
* blockfrost_api_env
* tx_explorer_url

## Verify the config of javascript file helpers.js
Open the file helpers.js and set the correct KOIOS API environment. If you want to use pre-production environment, simply leave the default value as is.

## Build project and install required nodejs library
In a command line, navigate to the dapp folder and run the following commands (instructions for the NPM package manager)
```bash
npm rebuild
npm install
```
## Add the swap contracts
Using the Admin interface of the dapp, you get a smart contract built for you. Add these contracts to the appropriate sections of the files specific_swap.js and wallet_integration.js. Please validate and verify that you use the correct format and order the smart contracts correctly as this is vital for correctly linking the smart contract and dapp pages.

## Optional renaming of specific_swap.js
It can be useful to rename the file dapp/specific_swap.js to include your nft project name or Cardano network environment if you run multiple swap pools on your server. In this way the individual projects are easily distinguishable in PM2.

## Start the dapp
The dapp should now be good to go. Start it using either nodejs start commands or PM2
```bash
pm2 start specific_swap.js
```
