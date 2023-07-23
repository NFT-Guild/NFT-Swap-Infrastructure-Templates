var createError = require('http-errors');
const express = require('express');
var path = require('path');
var fs = require('fs');
var https = require('https');
var privateKey  = fs.readFileSync('./keys/swappool.key', 'utf8');
var certificate = fs.readFileSync('./keys/swappool.pem', 'utf8');

var credentials = {key: privateKey, cert: certificate};

var indexRouter = require('./src/routes/index');
var adminRouter = require('./src/routes/admin');
var aboutRouter = require('./src/routes/about');

const app = express()

var httpsServer = https.createServer(credentials, app);

httpsServer.listen(13998);

app.set('view engine', 'pug')
app.set('views', path.join(__dirname, 'src/views'));
app.use(express.static(path.join(__dirname, 'public')));
app.use('/', indexRouter);
app.use('/admin', adminRouter);
app.use('/about', aboutRouter);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
    next(createError(404));
});
  
// error handler
app.use(function(err, req, res, next) {
    // set locals, only providing error in development
    res.locals.message = err.message;
    res.locals.error = req.app.get('env') === 'development' ? err : {};
  
    // render the error page
    res.status(err.status || 500);
    res.render('error');
});

////////////////////////////////////////////
// LIGHT / DARK MODE SETTINGS
app.set('swap_pool_theme', 'light-mode')
//app.set('swap_pool_theme', 'dark-mode')
////////////////////////////////////////////


////////////////////////////////////////////
// SWAP POOL SETTINGS
app.set('swap_pool_policy_id', 'INSERT PROJECT POLICY ID');

app.set('swap_pool_nft_names', []);

app.set('nft_per_page', 50);

app.set('swap_pool_address', 'INSERT SWAP POOL SMART CONTRACT ADDRESS');

// example format and values for swap pool settings
/*
app.set('swap_pool_policy_id', '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e');

app.set('swap_pool_nft_names', ["54727962626c65735365726965734f6e65456d6f6b6f3139323130","54727962626c65735365726965734f6e65456d6f6b6f3230303130","54727962626c65735365726965734f6e65456d6f6b6f3230343130","54727962626c65735365726965734f6e65456d6f6b6f3230353130","54727962626c65735365726965734f6e65456d6f6b6f3231363130","54727962626c65735365726965734f6e65456d6f6b6f3231323031","54727962626c65735365726965734f6e65456d6f6b6f3230393031","54727962626c65735365726965734f6e65456d6f6b6f3230373031","54727962626c65735365726965734f6e65456d6f6b6f3231333031","54727962626c65735365726965734f6e65456d6f6b6f3231343031","54727962626c65735365726965734f6e65456d6f6b6f3231353130","54727962626c65735365726965734f6e65456d6f6b6f3232333039"]);

app.set('nft_per_page', 50);

app.set('swap_pool_address', 'addr_test1wp2aypn7y90m5dqks2va960mhentssv626z4g2er64zfnsq9dqr33');
*/
//////////////////////////////////////////////

app.set('navWebpage', '#'); // change to the full address of your web site, example: https://github.com/cent-development
app.set('navTwitter', '#'); // change to the full address of your twitter page, https://twitter.com/yourprofile
app.set('navDiscord', '#'); // change to the full address of your Discord server, https://discord.gg/serverid

module.exports = app;
