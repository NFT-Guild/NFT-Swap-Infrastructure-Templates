var express = require('express');
var router = express.Router();

function getCookie(cookieString, cookiename) {

    if(cookieString == null) {
        // no cookie currently set
        return '';
    }

    const indexStart = cookieString.indexOf(cookiename);
    if (indexStart == -1) {
        // no cookie found with that name
        return '';
    }
    else {
        // cookie found..check value
        const indexEnd = cookieString.indexOf(';',indexStart);
        var cookieValue;
        if(indexEnd == -1) {
            cookieValue = cookieString.substring(indexStart);
        }
        else {
            cookieValue = cookieString.substring(indexStart, indexEnd);
        }

        cookieValue = cookieValue.substring(cookieValue.indexOf('=') + 1);

        
        return cookieValue;
    }
}

router.get('/', function (req, res, next) {
    const app = req.app;
    
    // cookie start
    const themeCookie = getCookie(req.headers.cookie, 'swap_pool_theme');
    var theme;
    if(themeCookie == '') {
        // no cookie set in users browser cookies...use server theme
        theme = app.get(`swap_pool_theme`);   
    }
    else {
        // user browser cookie set. Use this
        theme = themeCookie;
    }
    
    res.cookie('swap_pool_theme', theme);
    if (theme != 'dark-mode') {
        theme = '';
    }
    // cookie end

    res.render('admin', {
        poolName: app.get('swap_pool_names'),
        poolPolicyId: app.get('swap_pool_policy_id'),
        poolNFTNames: app.get('swap_pool_nft_names'),
        poolAddress: app.get('swap_pool_address'),
        nftPerPage: app.get('nft_per_page'),
        themeclass: theme
    });
});

module.exports = router;