var createError = require('http-errors');
const express = require('express');
var path = require('path');
var fs = require('fs');
var https = require('https');
var bodyParser = require('body-parser');

var privateKey  = fs.readFileSync(require.resolve('./keys/swappool.key'), 'utf8');
var certificate = fs.readFileSync(require.resolve('./keys/swappool.pem'), 'utf8');

var credentials = {key: privateKey, cert: certificate};

var indexRouter = require('./src/routes/index');
var adminRouter = require('./src/routes/admin');
var aboutRouter = require('./src/routes/about');
var apiPolicyAssetRouter = require('./src/routes/api_policy_asset_info');
var apiAssetInfoRouter = require('./src/routes/api_asset_info');
var apiAddressAssetsRouter = require('./src/routes/api_address_assets');
var apiTxStatusRouter = require('./src/routes/api_tx_status');
var apiAddressTxsRouter = require('./src/routes/api_address_txs');
var apiLatestSTEAKBlockRouter = require('./src/routes/api_get_latest_steak_block');

var randomRouter = require('./src/routes/random');
var apiSubmitRandomSwapCLIRouter = require('./src/routes/api_submit_swap_tx_cli');

const app = express()
app.use(bodyParser.json());

var httpsServer = https.createServer(credentials, app);

httpsServer.listen(13998);

app.set('view engine', 'pug')
app.set('views', path.join(__dirname, 'src/views'));
app.use(express.static(path.join(__dirname, 'public')));
app.use('/', indexRouter);
app.use('/admin', adminRouter);
app.use('/about', aboutRouter);
app.use('/api_policy_asset_info', apiPolicyAssetRouter);
app.use('/api_asset_info', apiAssetInfoRouter);
app.use('/api_address_assets', apiAddressAssetsRouter);
app.use('/api_tx_status', apiTxStatusRouter);
app.use('/api_address_txs', apiAddressTxsRouter);
app.use('/api_get_latest_steak_block', apiLatestSTEAKBlockRouter);

app.use('/random', randomRouter);
app.use('/api_submit_swap_tx_cli', apiSubmitRandomSwapCLIRouter);


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
app.set('swap_pool_names', ['COLLECTION NAME 1', 'COLLECTION NAME 2', 'COLLECTION NAME 3']);

app.set('swap_pool_policy_id', ['COLLECTION POLICY ID 1','COLLECTION POLICY ID 2', 'COLLECTION POLICY ID 3']);

app.set('swap_pool_nft_names', [[],['LIST OF HEX NFT NAMES'], []]);

app.set('swap_pool_rules',  [{},{}, {'OBJECT WITH': 'CONTRACT RULES'}]);

app.set('swap_pool_address', ['COLLECTION 1 SMART CONTRACT ADDRESS', 'COLLECTION 2 SMART CONTRACT ADDRESS', 'COLLECTION 3 SMART CONTRACT ADDRESS']);

app.set('nft_per_page', 50);

/*
// example format and values for swap pool settings

app.set('swap_pool_names',  [
                            'Common (policy-wide)', 
                            'Rare',
                            '040 - 049',
                            '050 - 059',
                            '070 - 075',
                            '039 - 075',
                            '030 - 100',
                            'Random',
                            'Queue'
                            ]);

app.set('swap_pool_policy_id',  [
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e', 
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e', 
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e', 
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e',
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e',
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e',
                                '141efecf55e4e6c91399e1d4561f9845481369a6f200209d4085ae9e',
                                '344c2a3c40c39e76cc22d0855b445ea60a891fc57afafd4081cf2947',
                                '344c2a3c40c39e76cc22d0855b445ea60a891fc57afafd4081cf2947'
                                ]);

app.set('swap_pool_nft_names',  [
                                [],
                                ["54727962626c65735365726965734f6e65456d6f6b6f3139323130","54727962626c65735365726965734f6e65456d6f6b6f3139333130","54727962626c65735365726965734f6e65456d6f6b6f3139343039","54727962626c65735365726965734f6e65456d6f6b6f3139353130","54727962626c65735365726965734f6e65456d6f6b6f3139363130","54727962626c65735365726965734f6e65456d6f6b6f3139373130","54727962626c65735365726965734f6e65456d6f6b6f3139383039","54727962626c65735365726965734f6e65456d6f6b6f3139393130","54727962626c65735365726965734f6e65456d6f6b6f3230303130","54727962626c65735365726965734f6e65456d6f6b6f3230313130","54727962626c65735365726965734f6e65456d6f6b6f3230323035","54727962626c65735365726965734f6e65456d6f6b6f3230333035","54727962626c65735365726965734f6e65456d6f6b6f3230343130","54727962626c65735365726965734f6e65456d6f6b6f3230353130","54727962626c65735365726965734f6e65456d6f6b6f3230363035","54727962626c65735365726965734f6e65456d6f6b6f3230373031","54727962626c65735365726965734f6e65456d6f6b6f3230383032","54727962626c65735365726965734f6e65456d6f6b6f3230393031","54727962626c65735365726965734f6e65456d6f6b6f3231303035","54727962626c65735365726965734f6e65456d6f6b6f3231313031","54727962626c65735365726965734f6e65456d6f6b6f3231323031","54727962626c65735365726965734f6e65456d6f6b6f3231333031","54727962626c65735365726965734f6e65456d6f6b6f3231343031","54727962626c65735365726965734f6e65456d6f6b6f3231353130","54727962626c65735365726965734f6e65456d6f6b6f3231363130","54727962626c65735365726965734f6e65456d6f6b6f3231393033","54727962626c65735365726965734f6e65456d6f6b6f3232333039"], 
                                [],
                                [],
                                [],
                                [],
                                [],
                                [],
                                []
                                ]);

app.set('swap_pool_rules',  [
                            {},
                            {},
                            {"nftNamePrefix":"TrybblesSeriesOneEmoko","digitIndexStart":22,"digitLength":3,"digitRangeFirst":40,"digitRangeLast":49},
                            {"nftNamePrefix":"TrybblesSeriesOneEmoko","digitIndexStart":22,"digitLength":3,"digitRangeFirst":50,"digitRangeLast":59},
                            {"nftNamePrefix":"TrybblesSeriesOneEmoko","digitIndexStart":22,"digitLength":3,"digitRangeFirst":70,"digitRangeLast":75},
                            {"nftNamePrefix":"TrybblesSeriesOneEmoko","digitIndexStart":22,"digitLength":3,"digitRangeFirst":39,"digitRangeLast":75},
                            {"nftNamePrefix":"TrybblesSeriesOneEmoko","digitIndexStart":22,"digitLength":3,"digitRangeFirst":30,"digitRangeLast":100},
                            {},
                            {}
                            ]);

app.set('swap_pool_address',    [
                                'addr_test1wz73v5de6tklh5d7aycxavvmq6jrux6nk7xynxeu7sm5zlg28frg7',
                                'addr_test1wzw0w5s880wy3nneejcfzla3ra73v4fk3e9fv6mtx866vucw57mw7', 
                                'addr_test1wz2yclhpsn6vzvs2ydx0vehz6th25k2fy4fy4kyjqyktx2cav6azl', 
                                'addr_test1wz4uptd902qvhj9ngsam5sxtyhtmcgj9d2qw7schs2dm8lcrpjzhx',
                                'addr_test1wp5a47skywwhwj6l3fc6suljkfr60kz49wpfktduezyud6q6sw8vw',
                                'addr_test1wpr66ej7jgladhpp7dhy25ys49zc2602m0jddksktg3xm0gujpf6r',
                                'addr_test1wz5c460rfg3290ff2jsfj7w4uqlhfw3d489navg0ta9h85c7nw7vc',
                                'addr_test1wqlmqyj32c4pgf8kchuva96jzjlseulqcs50ztdfxzfh5eq8kf3ge',
                                'addr_test1wrwfl0rhr4f4msaemr854u7zq6f7d5hlpmwjwwd5u6tq9xszkuyf4'
                                ]);

app.set('nft_per_page', 20);

app.set('view_dropdown_options',  [
    'TEXT LIST', 
    'SMALL THUMBNAILS',
    'LARGE THUMBNAILS'
    ]);

app.set('sort_dropdown_options',  [
    'MINT DATE', 
    'NAME',
    'RARITY',
    'TRAITS'
    ]);

app.set('rarity_dropdown_options',  [
    'COMMON', 
    'UNCOMMON',
    'RARE',
    'SUPER RARE',
    'ULTRA RARE',
]);

app.set('trait_dropdown_options', [
    'ANGRY',
    'CUTE',
    'HAPPY',
    'IGNORED',
    'IGNORING',
    'PROUD',
    'NEUTRAL',
    'PLAYFUL',
    'SAD',
    'SCARED',
    'SLEEPY',
    'WORRIED'
]);

app.set('filter_dropdown_options',  [
    'COMMON', 
    'UNCOMMON',
    'RARE',
    'SUPER RARE',
    'ULTRA RARE',
    'ANGRY', 
    'CUTE',
    'HAPPY',
    'IGNORED',
    'IGNORING',
    'PROUD',
    'NEUTRAL',
    'PLAYFUL',
    'SAD',
    'SCARED',
    'SLEEPY',
    'WORRIED'
    ]);
*/

//////////////////////////////////////////////
app.set('nftProjectName', 'Trybbles'); // the NFT project name to be displayed on the "GET ..." button, example: Trybbles
app.set('getNFTsMarketplaceURL', 'https://www.jpg.store/collection/trybbles?tab=items'); // link to marketplace where your NFTs can be bought. The target of the "GET ..." button, example: https://www.jpg.store/collection/trybbles?tab=items
app.set('getNFTsIconURL', 'images/Reveal-Animation-T-s.gif'); // change to full address of a icon that symbolises your NFT collection, displayed on the "GET NFTs" button, example: https://cdn.discordapp.com/attachments/1007576669558677504/1148908192378662912/Reveal-Animation-T-s.gif
app.set('navWebpage', '#'); // change to the full address of your web site, example: https://github.com/cent-development
app.set('navTwitter', '#'); // change to the full address of your twitter page, https://twitter.com/yourprofile
app.set('navDiscord', '#'); // change to the full address of your Discord server, https://discord.gg/serverid
app.set('navSupport', '#'); // change to the full address of your Support system, for example Discord server, https://discord.gg/serverid

////////////////////////////////////////////
// KOIOS NETWORK ENVIRONMENT SETTING - CHANGE TO YOUR DESIRED ENVIRONMENT
// const koios_api_url = 'https://api.koios.rest/api/v1'; // mainnet
// const koios_api_url = 'https://preprod.koios.rest/api/v1'; // preproduction
const koios_api_url = 'https://preview.koios.rest/api/v1'; // preview
app.set('koios_api_url', koios_api_url);

// environment variable to hold the path of the cardano node socket
process.env.CARDANO_NODE_SOCKET_PATH = '/.../preview/db-relay/node.socket';

module.exports = app;
