var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.post('/', function (req, res, next) {
    
    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const xhrTip = new XMLHttpRequest();
    xhrTip.onload = function () {

        if (xhrTip.status === 200) {

            const tipJson = JSON.parse(xhrTip.responseText);
            console.log('tip', xhrTip.responseText);

            const addresses = req.body._addresses;
            
            const koiosquery = `${koios_api_url}/address_txs`;
            // , _after_block_height=${tipJson[0].block_no - 200}
            const koiosparams = `{"_addresses":${JSON.stringify(addresses)}}`;
            
            var xhrTxs = new XMLHttpRequest();
            xhrTxs.onload = function () {

                if (xhrTxs.status === 200) {

                    const txs = JSON.parse(xhrTxs.responseText);

                    const tx_hashes = [];
                    // extract tx hashes
                    console.log('number of transactions in STEAK contract', txs.length);
                    // for(var i = 0; i < txs.length; i++) {
                    for(var i = txs.length - 100; i > 0 && i > txs.length - 200; i--) {
                        tx_hashes.push(txs[i].tx_hash);
                    }
                    
                    const koiosquery = `${koios_api_url}/tx_info`;
                    const koiosparams = `{"_tx_hashes":${JSON.stringify(tx_hashes)}}`;
                    
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
                                
                                var output, inline_datum_value, datum_fields, block_data;
                                for(var j = 0; j < txInfos[i].outputs.length; j++) {
                                    output = txInfos[i].outputs[j];
                                    if(output.inline_datum == null) continue;

                                    inline_datum_value = output.inline_datum.value;
                                    if(inline_datum_value == null) continue;

                                    console.log(`utxo: ${output.tx_hash}#${output.tx_index}`);

                                    datum_fields = inline_datum_value.fields;
                                    
                                    if(datum_fields.length == 3) {
                                        // steak pool datum...use to get mining pool info
                                        pool_data = datum_fields[0];

                                        if (pool_data.fields.length != 4) continue;

                                        // console.log('block_data.fields.length', pool_data.fields.length);
                                        const steak_pool_name_hash = Buffer.from(pool_data.fields[1].bytes, 'hex');
                                        console.log('pool name', steak_pool_name_hash.toString("utf-8"));
                                    }
                                    else if(datum_fields.length == 6) {
                                        // steak chain datum...use to get random number info
                                        block_data = datum_fields[2];

                                        if (block_data.fields.length != 3) continue;

                                        // we found the STEAK data
                                        const block_no = block_data.fields[0].int;
                                        const block_hash = block_data.fields[1].bytes;
                                        const steak_slot = block_data.fields[2].int;

                                        console.log('block_no', block_no);
                                        console.log('block_hash', block_hash);
                                        console.log('steak_slot', steak_slot);
                                    }
                                }
                                console.log('----');
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
        }
    };

    const koiosquery = `${koios_api_url}/tip`;
    xhrTip.open('GET', koiosquery, true);
    xhrTip.setRequestHeader('accept', 'application/json');
    xhrTip.send();
    



});

module.exports = router;