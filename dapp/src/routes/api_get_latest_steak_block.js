var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.post('/', function (req, res, next) {
    
    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const addresses = req.body._addresses;
    
    const koiosquery = `${koios_api_url}/address_txs`;
    const koiosparams = `{"_addresses":${JSON.stringify(addresses)}}`;
    
    // ask koios for all transactions performed by the randomness orcle address
    var xhrTxs = new XMLHttpRequest();
    xhrTxs.onload = function () {

        if (xhrTxs.status === 200) {

            const txs = JSON.parse(xhrTxs.responseText);

            const tx_hashes = [];
            
            // extract the hashes of the last 10 transactions
            for(var i = 0; i < txs.length && i < 10; i++) {
                tx_hashes.push(txs[i].tx_hash);
            }
            
            const koiosquery = `${koios_api_url}/tx_info`;
            const koiosparams = `{"_tx_hashes":${JSON.stringify(tx_hashes)}, "_metadata":true, "_scripts":true,"_inputs":true}`;
            
            // ask koios for extended information about the 10 latest transactions of the randomness oracle address
            var xhrTxInfo = new XMLHttpRequest();
            xhrTxInfo.onload = function () {

                if (xhrTxInfo.status === 200) {

                    // loop through transactions until the first (latest) randomness block is found
                    const txInfos = JSON.parse(xhrTxInfo.responseText);
                    for(var i = txInfos.length - 1; i >= 0; i--) {
                        if(txInfos[i].metadata == null) continue;
                        if(txInfos[i].metadata[674] == null) continue;
                        if(txInfos[i].metadata[674].msg == null) continue;
                        if(txInfos[i].metadata[674].msg[0] == null) continue;
                        if(txInfos[i].metadata[674].msg[0] != 'Mine Block') continue;
                        
                        // break out of loop and return the randomness block
                        res.json(txInfos[i]);
                        break;
                    }
                }
                else {
                    console.log('xhrTxInfo request unsuccessful', xhrTxInfo.responseText);
                }
            };

            xhrTxInfo.open('POST', koiosquery, true);
            xhrTxInfo.setRequestHeader('accept', 'application/json');
            xhrTxInfo.setRequestHeader('content-type', 'application/json');
            xhrTxInfo.send(koiosparams);
        }
        else {
            console.log('xhrTxs request unsuccessful', JSON.stringify(xhrTxs));
        }
    };

    xhrTxs.open('POST', koiosquery, true);
    xhrTxs.setRequestHeader('accept', 'application/json');
    xhrTxs.setRequestHeader('content-type', 'application/json');
    xhrTxs.send(koiosparams);
});

module.exports = router;