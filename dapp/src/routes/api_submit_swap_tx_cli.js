const express = require('express');
const { exec } = require('child_process');
const util = require('util');
const fs = require('fs');
const crypto = require('crypto');
const path = require('path');
const { 
  Address,
  PrivateKey,
  PublicKey,
  Hash28,
  Script,
  ScriptType,
  Credential,
  CredentialType,
  DataConstr,
  TxBuilder,
  Hash32
} = require("@harmoniclabs/plu-ts");
const { BlockfrostPluts } = require("@harmoniclabs/blockfrost-pluts");

const execPromise = util.promisify(exec);
const router = express.Router();

const { sharedSmartContracts } = require('../../public/javascripts/shared-resources');

async function calculateNewRandomHex(oracleHex, slot) {
  // Convert slot to hexadecimal and pad to 8 characters
  const slotHex = slot.toString(16).padStart(8, '0');
  
  // Concatenate slotHex and oracleHex
  const combinedHex = slotHex + oracleHex;
  
  // Convert the combined hex string to a Uint8Array
  const combinedBytes = new Uint8Array(combinedHex.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));
  
  // Calculate SHA-256 hash using Web Crypto API
  const hashBuffer = await crypto.subtle.digest('SHA-256', combinedBytes);
  
  // Convert the hash buffer to a hex string
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  
  const newRandomHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
  
  return newRandomHex;
}

// Helper function to execute cardano-cli commands
async function executeCardanoCli(command) {
  try {
    const { stdout, stderr } = await execPromise(command);
    if (stderr) {
      console.error('Command stderr:', stderr);
    }
    return stdout.trim();
  } catch (error) {
    console.error('Error executing cardano-cli command:', error);
    throw error;
  }
}

router.post('/', async function (req, res, next) {
  try {
    const txId = await createAndSubmitRandomSwapTransaction();
    if(txId != "") {
      console.log('Smart contract transaction submitted successfully. Transaction ID:', txId);
      res.json({ success: true, txId });
    } else {
      console.error('Failed to create and submit smart contract transaction');
      res.status(500).json({ success: false, error: 'Failed to create and submit smart contract transaction' });
    }
  } catch (error) {
    console.error('Failed to create and submit smart contract transaction:', error);
    res.status(500).json({ success: false, error: error.message });
  }
});

function getOracleData(oracle) {
  // get random number from oracle datum
  const datum_fields = oracle.resolved.datum.fields;
  const block_data = datum_fields[2];

  const oracleData = {};
  oracleData.block_no = block_data.fields[0].int;
  oracleData.block_hash = block_data.fields[1].bytes;
  oracleData.steak_slot = block_data.fields[2].int;
  return oracleData;
}

async function createAndSubmitRandomSwapTransaction() {
  try {
    // MOVE THESE TO CONFIG
    const cli = 'cardano-cli-1010';
    const walletSigningKeyFileName = 'test-1';
    const networkMagic = 2; // PREVIEW
    const environment = "testnet";
    const oracleAddress = 'addr_test1wqe0guqnnp463avr82jrscaq2e8q5xhc47636lmjncgs5lc9232s3';
    const policyId = "344c2a3c40c39e76cc22d0855b445ea60a891fc57afafd4081cf2947"; // Atrium policy id
    
    // Setup paths
    const walletDir = path.resolve(__dirname, '../../wallets');
    const tempDir = path.resolve('../tmp/cardano-tx');
    const socketPath = process.env.CARDANO_NODE_SOCKET_PATH || '/ipc/node.socket';

    // function to ensure sorted key order in case this affects the hashing of the datum
    const keySorter = (key, value) =>
      value instanceof Object && !(value instanceof Array) ?
        Object.keys(value)
          .sort()
          .reduce((sorted, key) => {
            sorted[key] = value[key];
            return sorted
          }, {}) :
        value;

    // Create temp directory if it doesn't exist
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }

    // MOVE THESE TO CONFIG
    // Define all file paths
    const protocolParamsFile = path.join(tempDir, 'protocol.json');
    const txRawFile = path.join(tempDir, 'tx.raw');
    const txSignedFile = path.join(tempDir, 'tx.signed');
    const queueScriptFile = path.join(tempDir, 'queue_script.plutus');
    const swapScriptFile = path.join(tempDir, 'swap_script.plutus');
    const queueDatumFile = path.join(tempDir, 'datum.json');
    const queueRedeemerFile = path.join(tempDir, 'queue_redeemer.json');
    const swapRedeemerFile = path.join(tempDir, 'swap_redeemer.json');
    const swapDatumFile = path.join(tempDir, 'unit.json');

    // Initialize BlockFrost and setup wallet
    const bf = new BlockfrostPluts({
      projectId: "previewXagICmH9YWjQmRrPU9dGM6WSGBow5UjP"
    });

    // Setup wallet and addresses
    const privateKeyFile = fs.readFileSync(path.join(walletDir, `${walletSigningKeyFileName}.skey`), 'utf8');
    const privateKey = PrivateKey.fromCbor(JSON.parse(privateKeyFile).cborHex);
    const publicKey = privateKey.derivePublicKey();
    const walletAddress = new Address(environment, new Credential(CredentialType.KeyHash, new Hash28(publicKey.hash)));
    
    // --- MAKE THESE DYNAMIC INSTEAD OF HAVING HARD-CODED INDEXES ------
    // the random swap contract
    const randomSwapContract = sharedSmartContracts[sharedSmartContracts.length - 2];

    // the random swap queue
    const randomQueueContract = sharedSmartContracts[sharedSmartContracts.length - 1];
    // ---------------------------------------------------

    const randomSwapContractScript = Script.fromCbor(randomSwapContract.script, ScriptType.PlutusV2);
    const randomSwapAddress = new Address(environment, new Credential(CredentialType.Script, new Hash28(randomSwapContractScript.hash)));
    
    const randomQueueContractScript = Script.fromCbor(randomQueueContract.script, ScriptType.PlutusV2);
    const randomQueueAddress = new Address(environment, new Credential(CredentialType.Script, new Hash28(randomQueueContractScript.hash)));
    
    // Find UTXOs using plu-ts and BlockFrost
    const bfWalletUTxOs = await bf.addressUtxos(walletAddress);
    if (bfWalletUTxOs.length === 0) {
      throw new Error('No UTXOs found for the wallet address');
    }

    // Find collateral using txBuilder
    const txBuilder = new TxBuilder(await bf.epochsLatestParameters());
    const collateralUTxOs = txBuilder.findCollaterals(bfWalletUTxOs, 5000000n);
    
    const collateralUTxO = collateralUTxOs[0];
    if (!collateralUTxO) {
      throw new Error("No suitable collateral UTXO found");
    }
    
    // Find queue script UTXO
    const pScriptUTxOs = await bf.addressUtxos(randomQueueAddress, {order:"desc"});
    
    var pScriptUTxO = pScriptUTxOs[pScriptUTxOs.length - 1];
    
    if (!pScriptUTxO) {
      throw new Error("No spendable UTXO found in the script address");
    }
    
    const nftToOfferCLI = pScriptUTxO.resolved.value.toString(false);
    
    // use queue script utxo as payment utxo
    const paymentUTxO = pScriptUTxO;

    var swapInitiatorAddress;
    var pDatum = pScriptUTxO.resolved.datum;
    if (pDatum != undefined) {
      // there is a datum on the utxo. This is required in final version of the code to calculate the swap initiator.
      pDatum = pDatum.toJson();

      // rename properties to match names used by CLI
      pDatum.constructor = pDatum.constr;
      delete pDatum.constr;

      fs.writeFileSync(queueDatumFile, JSON.stringify(pDatum, keySorter));
      
      swapInitiatorAddress = new Address(environment, new Credential(CredentialType.KeyHash, new Hash28(pDatum.fields[0].bytes)));
      console.log('Address of swap initiator:', swapInitiatorAddress.toString());

    }
    else {
      console.log('we want to test the contract and hence create our own receiver address, not based on any datum');
      swapInitiatorAddress = walletAddress;
    }
    
    // Save files needed for cardano-cli
    const queueScriptJson = {
      type: "PlutusScriptV2",
      description: "",
      cborHex: randomQueueContract.script  // or smartContract.cborHex depending on your contract structure
    };
    fs.writeFileSync(queueScriptFile, JSON.stringify(queueScriptJson, null, 2));

    const swapScriptJson = {
      type: "PlutusScriptV2",
      description: "",
      cborHex: randomSwapContract.script  // or smartContract.cborHex depending on your contract structure
    };
    fs.writeFileSync(swapScriptFile, JSON.stringify(swapScriptJson, null, 2));

    // Create redeemer file for Queue 
    const queueRedeemer = {
      "constructor": 0,
      "fields": [
        {
          "int": 0
        }
      ]
    };
    fs.writeFileSync(queueRedeemerFile, JSON.stringify(queueRedeemer));

    const oracleUTxOs = await bf.addressUtxos(oracleAddress, {order:"desc"});
    const oracleUTxO = oracleUTxOs[0]
    const oracleData = getOracleData(oracleUTxO);
  
    const blockInfo = await bf.getChainTip();

    const slot = oracleData.steak_slot + BigInt(blockInfo.slot);

    // calculating the hash of the oracle / slot combo
    const newRandom = await calculateNewRandomHex(oracleData.block_hash, slot);

    // getting the random integer from the first 4 bytes (8 characters) of the random hex
    const randomIntegerHex = newRandom.slice(0, 8);

    const randomInteger = BigInt(`0x${randomIntegerHex}`);

    // calculating the target value of closest match for this oracle / slot combination
    const targetValue = Number(randomInteger % 65536n);
    
    const swap_pool_utxos = await bf.addressUtxos(randomSwapAddress.toString());

    // looping through NFTs to find closest match
    var closestMatch = Number.MAX_SAFE_INTEGER;
    var closestName, closestMatchFields, closestUTXOID, closestUTXOIndex;
    var swapPoolUTxOToSpend;


    for (const utxo of swap_pool_utxos) {

      // Get the value from the resolved UTXO
      const valueMap = utxo.resolved.value.map;

      // Loop through each policy entry in the value map
      for (const policyEntry of valueMap) {
        // Check if this policy matches our target using toString()

        if (policyEntry.policy && policyEntry.policy.toString() === policyId) {
        
          // Found matching policy, now we can process its assets
          for (const asset of policyEntry.assets) {
            // Your custom logic here
            console.log("Found matching asset:", {
              policyId: policyEntry.policy.toString(),
              name: Buffer.from(asset.name).toString('hex'),
              quantity: asset.quantity
            });

            // calculating indexes of characters to use from the NFT name
            const nftName = Buffer.from(asset.name).toString('hex');
            const nameLength = nftName.length;

            const position1 = nameLength - 1 - Number(randomInteger % 8n);
            const position2 = nameLength - 1 - Number((randomInteger / 256n) % 8n);

            const byte1 = parseInt(nftName.substr(position1, 2), 16);
            const byte2 = parseInt(nftName.substr(position2, 2), 16);

            const selectedValue = (byte1 * 256) + byte2;

            const diff = Math.abs(selectedValue - targetValue)
            if (diff < closestMatch) {
              // diff is smaller than current closest match. Save this NFT as closest match
              closestMatch = diff;
              closestName = nftName;
              closestUTXOID = utxo.utxoRef.id.toString();
              closestUTXOIndex = utxo.utxoRef.index;
              swapPoolUTxOToSpend = utxo;

              // save info about current closest match in a list
              closestMatchFields = [
                { "bytes": newRandom },
                { "bytes": randomIntegerHex },
                { "int": targetValue },
                { "int": parseInt(slot) },
                { "int": position1 },
                { "int": position2 },
                { "int": selectedValue },
                { "int": diff },
                { "int": 0 } // action = 0 - validate random swap selection
              ]
            }
          }
        }
      }
    }

    // Create redeemer file for Swap using the closest match data
    const swapRedeemer = {
      "constructor": 0,
      "fields": closestMatchFields
    };
    fs.writeFileSync(swapRedeemerFile, JSON.stringify(swapRedeemer));

    const swapDatum = {
      "constructor": 0,
      "fields":[]
    };
    fs.writeFileSync(swapDatumFile,JSON.stringify(swapDatum));

    console.log(`utxo ${closestUTXOID}#${closestUTXOIndex}`);

    // Get protocol parameters
    await executeCardanoCli(
      `${cli} conway query protocol-parameters \
      --socket-path ${socketPath} \
      ${environment == 'mainnet' ? '--mainnet' : `--testnet-magic ${networkMagic}`} \
      --out-file ${protocolParamsFile}`
    );

    const nftToReceiveCLI = `1 ${policyId}.${closestName}`

    // Build cli transaction
    const buildCommand = `${cli} conway transaction build \
      --socket-path ${socketPath} \
      ${environment == 'mainnet' ? '--mainnet' : `--testnet-magic ${networkMagic}`} \
      --tx-in-collateral ${collateralUTxO.utxoRef.id}#${collateralUTxO.utxoRef.index} \
      --tx-in ${paymentUTxO.utxoRef.id}#${paymentUTxO.utxoRef.index} \
      --tx-in-script-file ${queueScriptFile} \
      --tx-in-${pScriptUTxO.resolved.datum.constructor.name === 'DataConstr' ? 'inline-datum-present' : 'datum-file'} ${pScriptUTxO.resolved.datum.constructor.name === 'DataConstr' ? '' : queueDatumFile} \
      --tx-in-redeemer-file ${queueRedeemerFile} \
      --tx-out ${randomSwapAddress.toString()}+1500000+${nftToOfferCLI} \
      --tx-out-inline-datum-file ${swapDatumFile} \
      --tx-in ${closestUTXOID}#${closestUTXOIndex} \
      --tx-in-script-file ${swapScriptFile} \
      --tx-in-${swapPoolUTxOToSpend.resolved.datum.constructor.name === 'DataConstr' ? 'inline-datum-present' : 'datum-file'} ${swapPoolUTxOToSpend.resolved.datum.constructor.name === 'DataConstr' ? '' : swapDatumFile} \
      --tx-in-redeemer-file ${swapRedeemerFile} \
      --read-only-tx-in-reference ${oracleUTxO.utxoRef.id.toString()}#${oracleUTxO.utxoRef.index} \
      --tx-out ${swapInitiatorAddress.toString()}+1500000+"${nftToReceiveCLI}" \
      --required-signer-hash ${publicKey.hash.toString()} \
      --change-address ${walletAddress.toString()} \
      --out-file ${txRawFile}`;
  
    console.log('Build command:', buildCommand);

    await executeCardanoCli(buildCommand);
    
    // Sign transaction
    const signCommand = `${cli} conway transaction sign \
      --tx-body-file ${txRawFile} \
      --signing-key-file ${path.join(walletDir, `${walletSigningKeyFileName}.skey`)} \
      ${environment == 'mainnet' ? '--mainnet' : `--testnet-magic ${networkMagic}`} \
      --out-file ${txSignedFile}`;

    await executeCardanoCli(signCommand);

    // Submit transaction
    const txId = await executeCardanoCli(
      `${cli} conway transaction submit \
      --socket-path ${socketPath} \
      ${environment == 'mainnet' ? '--mainnet' : `--testnet-magic ${networkMagic}`} \
      --tx-file ${txSignedFile}`
    );

    // Clean up temp files
    fs.unlinkSync(txRawFile);
    fs.unlinkSync(txSignedFile);
    fs.unlinkSync(queueScriptFile);
    fs.unlinkSync(queueDatumFile);
    fs.unlinkSync(queueRedeemerFile);
    fs.unlinkSync(swapScriptFile);
    fs.unlinkSync(swapRedeemerFile);
    fs.unlinkSync(protocolParamsFile);

    return txId;

  } catch (error) {
    console.error('Error in createAndSubmitRandomSwapTransaction:', error);
    throw error;
  }
}

module.exports = router;