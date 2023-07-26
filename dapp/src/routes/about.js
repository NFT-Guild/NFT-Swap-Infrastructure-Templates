var express = require('express');
var router = express.Router();

router.get('/', function (req, res, next) {
    const app = req.app;
    var theme = app.get(`swap_pool_theme`);
    if (theme != 'dark-mode') {
        theme = '';
    }

    res.render('about', {
        themeclass: theme
    });
});

module.exports = router;