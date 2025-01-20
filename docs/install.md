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

## Additional steps needed for Random Swap Pools
The Swapper server job creates Cardano transactions using the Cardano node CLI. We are looking at doing a full Plu-ts implementation, but a bug inhibited us from doing this at this time. Unfortunately the Random Swap functionality then has a requirement on a Cardano Node until we have a plu-ts implementation in place.

### Download Cardano-CLI from Intersect GitHub
There are prebuilt cli execuables available for download directly from the [Intersect GitHub](https://github.com/IntersectMBO/cardano-cli) repository. The swap pool templates project is tested with version 10.1.0.0, but you should normally be able to use the latest version avaiable from the GitHub repository. Please follow the installation documentation on the GitHub site and download the correct version for your server operating system.

### Create wallet keys for the Swapper server job
To sign a Cardano transaction, the Swapper server job will need to control a Cardano wallet. You can reuse an already existing wallet if you want, but it must be on the format that is understood by the Cardano CLI, namely a file for the public key (.vkey) and a file containing the private signing key (.skey). We do recommend to create a new wallet solely intended for this service as this wallet should not contain any significant amounts of ADA. The transactions will be paid by the swap requests and not by the swapper wallet.
To generate a new wallet address, please use the Cardano CLI commands according to for example this [Cardano Developers Generate Wallet page](https://developers.cardano.org/docs/operate-a-stake-pool/generating-wallet-keys/). Please configure the dApp with by adding the vkey and skey files into the dapp/wallets folder. After this, configure the swapper server job with the name of the wallet and you are ready.

### Create Random Swap contracts using the Public Key Hash of the wallet
When you create the Random Swap contracts you provide the policy id of the desired NFT collection in the policy id field. In addition you provide the public key hash of the wallet. This wallet key is then supplied as a parameter into both contracts to ensure that swap contract operations can only be performed by the swapper server job.

### Start the Swapper server job
The swapper server job is a regular nodejs scheduled job. To start it simply start it using either nodejs start command or PM2
```bash
pm2 start swapper_job.js
```