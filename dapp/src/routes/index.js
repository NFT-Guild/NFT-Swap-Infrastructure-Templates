var express = require('express');
var router = express.Router();

function getCookie(cookieString, cookiename) {
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
        nftPerPage: app.get('nft_per_page'),
        navWebpage: app.get('navWebpage'),
        navTwitter: app.get('navTwitter'),
        navDiscord: app.get('navDiscord'),
        themeclass: theme
    });
});

module.exports = router;