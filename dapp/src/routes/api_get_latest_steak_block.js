var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.post('/', function (req, res, next) {
    
    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const addresses = req.body._addresses;
    
    const koiosquery = `${koios_api_url}/address_txs`;
    const koiosparams = `{"_addresses":${JSON.stringify(addresses)}}`;
    
    var xhrTxs = new XMLHttpRequest();
    xhrTxs.onload = function () {

        if (xhrTxs.status === 200) {

            const txs = JSON.parse(xhrTxs.responseText);

            const tx_hashes = [];
            // extract tx hashes
            console.log('number of transactions in STEAK contract', txs.length);
            for(var i = 0; i < txs.length && i < 10; i++) {
            // for(var i = txs.length - 100; i > 0 && i > txs.length - 200; i--) {
                tx_hashes.push(txs[i].tx_hash);
            }
            console.log('tx_hashes', tx_hashes);
            const koiosquery = `${koios_api_url}/tx_info`;
            const koiosparams = `{"_tx_hashes":${JSON.stringify(tx_hashes)}, "_metadata":true, "_scripts":true,"_inputs":true}`;
            
            var xhrTxInfo = new XMLHttpRequest();
            xhrTxInfo.onload = function () {

                if (xhrTxInfo.status === 200) {

                    const txInfos = JSON.parse(xhrTxInfo.responseText);
                    for(var i = txInfos.length - 1; i >= 0; i--) {
                        if(txInfos[i].metadata == null) continue;
                        if(txInfos[i].metadata[674] == null) continue;
                        if(txInfos[i].metadata[674].msg == null) continue;
                        if(txInfos[i].metadata[674].msg[0] == null) continue;
                        if(txInfos[i].metadata[674].msg[0] != 'Mine Block') continue;
                        
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