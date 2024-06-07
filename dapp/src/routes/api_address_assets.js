var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.post('/', function (req, res, next) {
    
    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const xhr = new XMLHttpRequest();
    var koiosquery, koiosparams = '';

    var addresses = req.body._addresses;
    var stake_addresses = req.body._stake_addresses;

    if(typeof addresses !== 'undefined') {
        koiosquery = `${koios_api_url}/address_assets`;
        koiosparams = `{"_addresses":${JSON.stringify(addresses)}}`;
    }
    else {
        koiosquery = `${koios_api_url}/account_assets`;
        koiosparams = `{"_stake_addresses":${JSON.stringify(stake_addresses)}}`
    }
   
    xhr.open('POST', koiosquery, true);

    xhr.onload = function () {
        res.json(JSON.parse(xhr.response));
    }

    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(koiosparams);
});

module.exports = router;