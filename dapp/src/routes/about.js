var express = require('express');
var router = express.Router();

router.get('/', function (req, res, next) {
    const app = req.app;
    var theme = app.get(`swap_pool_theme`);
    if (theme != 'dark-mode') {
        theme = '';
    }

    res.render('about', {
        /*poolPolicyId: app.get('swap_pool_policy_id'),
        poolNFTNames: app.get('swap_pool_nft_names'),
        nftPerPage: app.get('nft_per_page'),*/
        themeclass: theme
    });
});

module.exports = router;