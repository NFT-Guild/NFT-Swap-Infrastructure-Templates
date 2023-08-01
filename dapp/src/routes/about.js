var express = require('express');
var router = express.Router();

router.get('/', function (req, res, next) {
    const app = req.app;
    var theme = app.get(`swap_pool_theme`);
    if (theme != 'dark-mode') {
        theme = '';
    }

    res.render('about', {
        navWebpage: app.get('navWebpage'),
        navTwitter: app.get('navTwitter'),
        navDiscord: app.get('navDiscord'),
        themeclass: theme
    });
});

module.exports = router;