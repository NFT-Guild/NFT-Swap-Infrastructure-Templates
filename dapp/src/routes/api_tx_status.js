var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.post('/', function (req, res, next) {
    
    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const xhr = new XMLHttpRequest();
    
    var tx_hashes = req.body._tx_hashes;
    
    const koiosparams = `{"_tx_hashes":${JSON.stringify(tx_hashes)}}`;
   
    xhr.open('POST', `${koios_api_url}/tx_status`, true);

    xhr.onload = function () {
        res.json(JSON.parse(xhr.response));
    }

    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(koiosparams);
});

module.exports = router;