var express = require('express');
var router = express.Router();
var XMLHttpRequest = require('xhr2');

router.get('/', function (req, res, next) {

    const app = req.app;
    const koios_api_url = app.get('koios_api_url');

    const assetpolicy = req.query.policy_id;
    const order = req.query.order;
    const filter = req.query.filter;
    var offset = req.query.offset;
    var limit = req.query.limit;

    if(offset == undefined) offset = 0;   // set default if undefined
    if(limit == undefined) limit = 50;    // set default if undefined
    var offsetParam = `&offset=${offset}`;
    var limitParam = `&limit=${limit}`;
    
    if(filter != '') {
        offsetParam = '';
        limitParam = '';       // leave out parameters if filter is applied. Do offset and limiting after fetch to ensure non-empty pages
    }

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {
        if (xhr.status === 200) {
            res.json(JSON.parse(xhr.responseText));
        }
    }

    const koiosquery = `${koios_api_url}/policy_asset_info?_asset_policy=${assetpolicy}&order=${order}${offsetParam}${limitParam}`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();

});

module.exports = router;