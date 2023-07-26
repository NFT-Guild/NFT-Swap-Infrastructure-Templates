var express = require('express');
var router = express.Router();

router.get('/', function (req, res, next) {
    const app = req.app;
    var theme = app.get(`swap_pool_theme`);
    if (theme != 'dark-mode') {
        theme = '';
    }

    var poolIndexParam = parseInt(req.query.poolIndex);
    if(isNaN(poolIndexParam) ||
        poolIndexParam == undefined || 
        poolIndexParam < 0 || 
        (poolIndexParam > app.get('swap_pool_names').length -1)) poolIndexParam = 0; 

    res.render('index', {
        currentPoolIndex: poolIndexParam,
        poolName: app.get('swap_pool_names'),
        poolPolicyId: app.get('swap_pool_policy_id'),
        poolNFTNames: app.get('swap_pool_nft_names'),
        poolAddress: app.get('swap_pool_address'),
        navWebpage: app.get('navWebpage'),
        navTwitter: app.get('navTwitter'),
        navDiscord: app.get('navDiscord'),
        themeclass: theme
    });
});

module.exports = router;