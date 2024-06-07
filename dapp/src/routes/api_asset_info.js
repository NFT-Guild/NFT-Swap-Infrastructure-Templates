var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.get('/', function (req, res, next) {

    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const assetpolicy = req.query.policy_id;
    const assetname = req.query.asset_name;

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {
        if (xhr.status === 200) {
            res.json(JSON.parse(xhr.responseText));
        }
    }

    const koiosquery = `${koios_api_url}/asset_info?_asset_policy=${assetpolicy}&_asset_name=${assetname}`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();

});

module.exports = router;