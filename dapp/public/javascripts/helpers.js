var connectedWallet = undefined;
var connectedWalletExtName = '';
var connectorDropdown = undefined;
var connectorButton = undefined;
var selectedPoolNFTs = [];
var selectedWalletNFTs = [];
var numSelectedPoolNFTs = 0;
var numSelectedWalletNFTs = 0;
var selectedFilterNFTNameMap = new Map();

// KOIOS MAINNET / PREPROD SETTING - CHANGE TO YOUR DESIRED ENVIRONMENT
//const koios_api_url = 'https://api.koios.rest/api/v0'; // mainnet
const koios_api_url = 'https://preprod.koios.rest/api/v0'; // preproduction

function addEnterKeyListener(sourceElem, clickElem) {
    sourceElem.addEventListener("keypress", function(event) {
        if (event.key === "Enter") {
          // cancel the default action, if needed
          event.preventDefault();
          // trigger onclick event of element 
          document.getElementById(clickElem.id).click();
        }
    });
}

function toggleSelectedPoolNFTs(theme) {
    for (var i = 0; i < selectedPoolNFTs.length; i++) {
        togglePoolNFTSelectionViewOnly(`${selectedPoolNFTs[i]}`, theme);
    }
}

function updateWalletSelectionLabel() {
    var numSelectedFromWalletLabel = document.getElementById('selectNFTsDialogLabel');
    if(numSelectedFromWalletLabel.innerText.indexOf('NFTs to swap') > 0) {
        numSelectedFromWalletLabel.innerText = `Select ${numSelectedPoolNFTs - numSelectedWalletNFTs} NFTs to swap`;
    }
}

function updatePoolSelectionLabel() {
    var numSelectedFromPoolLabelTop = document.getElementById('num_selected');
    numSelectedFromPoolLabelTop.innerText = `${numSelectedPoolNFTs} SELECTED`;
}

function resetSelectedWalletNFTs() {
    numSelectedWalletNFTs = 0;
    selectedWalletNFTs = [];
    updateWalletSelectionLabel();
}

function togglePoolNFTSelectionViewOnly(nftimageid, theme) {
    var nftimage = document.getElementById(nftimageid+'');
    if(nftimage == null) {
        // nftimage is not currently in view...skip toggle of selection in UI
        return;
    }
    
    var nameDiv = document.getElementById(nftimageid + "_namediv");

    if (nftimageid.indexOf('pool_') == 0) {
        if (selectedPoolNFTs.indexOf(nftimageid) > -1) {
            // NFT is selected. set style according to active selection
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
        }
    }
    else {
        if (selectedWalletNFTs.indexOf(nftimageid) > -1) {
            // NFT is selected. set style according to active selection
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
        }
    }
}

function togglePoolNFTSelection(nftimage, theme) {
    var selectButton = document.getElementById('selectPoolNFTButton');
    var confirmButton = document.getElementById('confirmSwapButton')
    var nameDiv = document.getElementById(nftimage.id + "_namediv");

    if (nftimage.id.indexOf('pool_nft_list_') == 0) {
        if (selectedPoolNFTs.indexOf(nftimage.id) > -1) {
            selectedPoolNFTs = selectedPoolNFTs.filter(e => e !== nftimage.id);
            nftimage.classList.remove("selected" + theme);
            nftimage.classList.add("not-selected" + theme);
            nameDiv.classList.remove("selected-no-border" + theme);
            nameDiv.classList.add("not-selected" + theme);
            numSelectedPoolNFTs = selectedPoolNFTs.length;
        }
        else {
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
            numSelectedPoolNFTs = selectedPoolNFTs.push(nftimage.id);
        }
        updatePoolSelectionLabel();
    }
    else {
        if (selectedWalletNFTs.indexOf(nftimage.id) > -1) {
            selectedWalletNFTs = selectedWalletNFTs.filter(e => e !== nftimage.id);
            nftimage.classList.remove("selected" + theme);
            nftimage.classList.add("not-selected" + theme);
            nameDiv.classList.remove("selected-no-border" + theme);
            nameDiv.classList.add("not-selected" + theme);
            numSelectedWalletNFTs = selectedWalletNFTs.length;
        }
        else {
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
            numSelectedWalletNFTs = selectedWalletNFTs.push(nftimage.id);
        }
        updateWalletSelectionLabel();
    }

    if(selectButton != null) {
        if (numSelectedPoolNFTs > 0) {
            selectButton.classList.remove("disabled");
        }
        else {
            selectButton.classList.add("disabled");
        }
    }

    if(confirmButton != null) {
        if (numSelectedPoolNFTs != numSelectedWalletNFTs) {
            confirmButton.classList.add("disabled");
        }
        else {
            confirmButton.classList.remove("disabled");
        }
    }

}

function setInnerText(elemid, text) {
    document.getElementById(elemid).innerText = text;
}

function showElem(elemid) {
    document.getElementById(elemid).style.display = '';
}

function hideElem(elemid) {
    document.getElementById(elemid).style.display = 'none';
}

function togglePoolFilterSelection(nftimage, nftnamehex, theme) {
    var createScriptButton = document.getElementById('generateFilteredPoolButton');
    var nameDiv = document.getElementById(nftimage.id + "_namediv");

    if (selectedPoolNFTs.indexOf(nftimage.id) > -1) {
        selectedPoolNFTs = selectedPoolNFTs.filter(e => e !== nftimage.id);
        nftimage.classList.remove("selected" + theme);
        nftimage.classList.add("not-selected" + theme);
        nameDiv.classList.remove("selected-no-border" + theme);
        nameDiv.classList.add("not-selected" + theme);
        numSelectedPoolNFTs = selectedPoolNFTs.length;
        selectedFilterNFTNameMap.delete(nftimage.id);
    }
    else {
        nftimage.classList.add("selected" + theme);
        nftimage.classList.remove("not-selected" + theme);
        nameDiv.classList.add("selected-no-border" + theme);
        nameDiv.classList.remove("not-selected" + theme);
        numSelectedPoolNFTs = selectedPoolNFTs.push(nftimage.id);
        selectedFilterNFTNameMap.set(nftimage.id, nftnamehex);
    }


    if (numSelectedPoolNFTs > 0) {
        createScriptButton.classList.remove("disabled");
    }
    else {
        createScriptButton.classList.add("disabled");
    }

    var numSelectedFromPoolLabelTop = document.getElementById('num_selected');
    numSelectedFromPoolLabelTop.innerText = `${numSelectedPoolNFTs} SELECTED`;

}


async function getAddressAssets(address) {
    
    var xhr = new XMLHttpRequest();

    const koiosquery = `${koios_api_url}/address_assets`;
    xhr.open('POST', koiosquery, false);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(`{"_addresses":["${address}"]}`);

    if (xhr.status === 200) {
        addressAssetsJSON = JSON.parse(xhr.response);
                    
        if (addressAssetsJSON[0]['asset_list']) {
            return addressAssetsJSON[0]['asset_list'];
        }
        else {
            console.log('no asset_list');
        }
    }
    return {};
}

async function getAddressAssetFingerprints(address) {
    var xhr = new XMLHttpRequest();

    const koiosquery = `${koios_api_url}/address_assets`;
    
    xhr.open('POST', koiosquery, false);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(`{"_addresses":["${address}"]}`);

    if (xhr.status === 200) {
        addressAssetsJSON = JSON.parse(xhr.response);
        var asset_list = addressAssetsJSON[0]['asset_list'];
        var asset_fingerprints = [];
        
        if (asset_list) {
            for(var i = 0; i < asset_list.length; i++) {
                asset_fingerprints.push(asset_list[i]['fingerprint']);           
            }
            return asset_fingerprints;
        }
        else {
            console.log('no asset_list');
        }
    }
    return [];
}

async function getAssetInfo(assetpolicy, fingerprints, list_html_element, filter, theme, pOffset, pLimit) {

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {

        if (xhr.status === 200) {

            var assetJson = JSON.parse(xhr.responseText);
            var htmlList = '';
            var numberOfResults = 0;
            for (let i = 0; i < assetJson.length; i++) {
                let asset = assetJson[i];
                
                if (!fingerprints.includes(`${asset['fingerprint']}`)) { continue; }

                if (asset.minting_tx_metadata) {
                    if (!asset.minting_tx_metadata['721']) { continue; }
                    const asset_policy = asset.minting_tx_metadata['721'][assetpolicy];

                    const metadata = asset_policy[asset.asset_name_ascii];

                    if (filter.trim() != '') {
                        // a filter criterion is applied. Skip if criterion is not satisfied or result item is outside offset/limit range
                        const metadataString = JSON.stringify(metadata);
                        if (metadataString.toLowerCase().indexOf(filter.toLowerCase()) == -1) { continue; }
                    }

                    numberOfResults++;
                    if(numberOfResults <= pOffset) { continue; } // result item is before the start of range. Continue to next result
                    else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop

                    const image = metadata['image'];
                    var ipfsID = "";
                    var imageURL = "";

                    try {
                        var ipfsIndex = image.indexOf('Qm');
                        if (ipfsIndex > -1) {
                            ipfsID = image.substring(image.indexOf('Qm'));
                            imageURL = `https://image-optimizer.jpgstoreapis.com/${ipfsID}`;
                        }
                    }
                    catch (error) { console.log(error); }

                    
                    // no filter applied. Offset and limiting done in query. Simply output the result item
                    htmlList += `<div class="not-selected${theme} padding" id="pool_nft_list_${assetpolicy}${asset.asset_name}"><img id="pool_nft_list_${assetpolicy}${asset.asset_name}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(document.getElementById('pool_nft_list_${assetpolicy}${asset.asset_name}'), '${theme}');" class="show-hover-pointer padding" src="${imageURL}"></img><div id="pool_nft_list_${assetpolicy}${asset.asset_name}_namediv" class="nft-name-display not-selected${theme} align-bottom">${metadata['name']}</div></div>`;
                }
                else if (asset.token_registry_metadata) {

                    //TODO: IMPLEMENT 

                    //console.log("koios onchain image");
                    //document.getElementById(elem_prefix + assetinfo).innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo});" class="show-hover-pointer padding" src='data:image/jpeg;base64,${assetJson.metadata.logo}'><div id="${elem_prefix + assetinfo}_namediv" class="not-selected align-bottom">${assetJson.metadata.name}</div>`;
                }
            }

            htmlList += '</div>';
            document.getElementById(list_html_element).innerHTML = htmlList;

        }
        else {
            document.getElementById(list_html_element).innerHTML = xhr.responseText;
        }

        
        if(document.querySelectorAll('[id$="_namediv"]').length < (pLimit - 1)) {
            // we are now displaying fewer results than the pLimit. There are no more results to display. Disable paging
            disableHigherNavPages();
        }
        else {
            enableHigherNavPages();
        }

        toggleSelectedPoolNFTs(`${theme}`)

    };
  
    const koiosquery = `${koios_api_url}/policy_asset_info?_asset_policy=${assetpolicy}&order=asset_name_ascii.asc`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();
}


async function loadPolicyAssets(assetpolicy, list_html_element, filter, theme, pOffset, pLimit) {

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {

        if (xhr.status === 200) {

            var assetJson = JSON.parse(xhr.responseText);

            var htmlList = '<div class="nft-list light"><div class="flex-row d-flex mb-3 flex-wrap padding">';
            var numberOfResults = 0;
            for (let i = 0; i < assetJson.length; i++) {
                let asset = assetJson[i];

                if (asset.minting_tx_metadata) {
                    if (!asset.minting_tx_metadata['721']) { continue; }
                    const asset_policy = asset.minting_tx_metadata['721'][assetpolicy];

                    const metadata = asset_policy[asset.asset_name_ascii];

                    if (filter.trim() != '') {
                        // a filter criterion is applied. Skip if criterion is not satisfied or result item is outside offset/limit range
                        const metadataString = JSON.stringify(metadata);
                        if (metadataString.toLowerCase().indexOf(filter.toLowerCase()) == -1) { continue; }

                        numberOfResults++;
                        if(numberOfResults < pOffset) { continue; } // result item is before the start of range. Continue to next result
                        else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop
                        //else {
                        //    result item is within the offset/limit range we want to display
                        //}
                    }

                    const image = metadata['image'];
                    var ipfsID = "";
                    var imageURL = "";

                    try {
                        var ipfsIndex = image.indexOf('Qm');
                        if (ipfsIndex > -1) {
                            ipfsID = image.substring(image.indexOf('Qm'));
                            imageURL = `https://image-optimizer.jpgstoreapis.com/${ipfsID}`;
                        }
                    }
                    catch (error) { console.log(error); }

                    
                    // no filter applied. Offset and limiting done in query. Simply output the result item
                    htmlList += `<div class="not-selected${theme} padding" id="pool_filter_nft_list_${asset.fingerprint}"><img id="pool_filter_nft_list_${asset.fingerprint}_img" loading="lazy" height="200" onclick="togglePoolFilterSelection(pool_filter_nft_list_${asset.fingerprint}, '${asset.asset_name}', '${theme}');" class="show-hover-pointer padding" src="${imageURL}"></img><div id="pool_filter_nft_list_${asset.fingerprint}_namediv" class="nft-name-display not-selected${theme} align-bottom">${metadata['name']}</div></div>`;
                }
                else if (asset.token_registry_metadata) {

                    //TODO: IMPLEMENT 

                    //console.log("koios onchain image");
                    //document.getElementById(elem_prefix + assetinfo).innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo});" class="show-hover-pointer padding" src='data:image/jpeg;base64,${assetJson.metadata.logo}'><div id="${elem_prefix + assetinfo}_namediv" class="not-selected align-bottom">${assetJson.metadata.name}</div>`;
                }
            }

            htmlList += '</div>';
            document.getElementById(list_html_element).innerHTML = htmlList;

        }
        else {
            document.getElementById(list_html_element).innerHTML = xhr.responseText;
        }

        if(document.querySelectorAll('[id$="_namediv"]').length < (pLimit - 1)) {
            // we are now displaying fewer results than the pLimit. There are no more results to display. Disable paging
            disableHigherNavPages();
        }
        else {
            enableHigherNavPages();
        }

        toggleSelectedPoolNFTs(`${theme}`)

    };


    if(pOffset == undefined) pOffset = 0;   // set default if undefined
    if(pLimit == undefined) pLimit = 50;    // set default if undefined
    var offsetParam = `&offset=${pOffset}`;
    if(pOffset == 0) offsetParam = '';      // leave out parameter if offset is 0
    var limitParam = `&limit=${pLimit}`;
    if(filter != '') {
        offsetParam = '';
        limitParam = '';       // leave out parameters if filter is applied. Do offset and limiting after fetch to ensure non-empty pages
    }

    
    const koiosquery = `${koios_api_url}/policy_asset_info?_asset_policy=${assetpolicy}&order=asset_name_ascii.asc${offsetParam}${limitParam}`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();
}

function stepPage(increment) {

    // update page number of the 3 numbered pages
    var navPageElem, navPageNumOld;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`page${i}`);
        navPageNumOld = parseInt(navPageElem.innerText);
        navPageElem.innerText = navPageNumOld + increment;
    }

    // enable / disable PREVIOUS
    navPageElem = document.getElementById(`page1`);
    if(parseInt(navPageElem.innerText) > 1) {
        document.getElementById('pageprevious').parentElement.classList.remove('disabled');
    }
    else {
        document.getElementById('pageprevious').parentElement.classList.add('disabled');
    }
}

function setActivePage(pNum) {
    var navPageElem;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`page${i}`);
        if(i == pNum) {
            navPageElem.classList.add('active');
        }
        else {
            navPageElem.classList.remove('active');
        }
    }
}

function disableHigherNavPages() {
    var navPageElem;

    // disable next pager
    navPageElem = document.getElementById(`pagenext`);
    navPageElem.classList.add('disabled');

    // loop through pages from the top and disable until active page is found
    for(var i = 3; i >= 1; i--) {
        navPageElem = document.getElementById(`page${i}`);
        if(navPageElem.classList.contains('active')) break;

        navPageElem.classList.add('disabled');
    }
}

function enableHigherNavPages() {
    var navPageElem;

    // enable next pager
    navPageElem = document.getElementById(`pagenext`);
    navPageElem.classList.remove('disabled');

    // loop through pages from the top and enable until active page is found
    for(var i = 3; i >= 1; i--) {
        navPageElem = document.getElementById(`page${i}`);
        if(navPageElem.classList.contains('active')) break;

        navPageElem.classList.remove('disabled');
    }
}

function hex_to_ascii(str1) {
    var hex = str1.toString();
    var str = '';
    for (var n = 0; n < hex.length; n += 2) {
        str += String.fromCharCode(parseInt(hex.substr(n, 2), 16));
    }
    return str;
}

function loadNFTInfoKoios(elem_prefix, assetinfo, theme) {
    const assetpolicy = assetinfo.substring(0, 56)
    const assetname = assetinfo.substring(56);

    const assetNameAscii = hex_to_ascii(assetname);

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {

        if (xhr.status === 200) {
            var assetJson = JSON.parse(xhr.responseText);

            if (assetJson[0].minting_tx_metadata) {
                const koios_policy = assetJson[0].minting_tx_metadata['721'][assetpolicy];

                const asset = koios_policy[assetNameAscii];

                const image = asset['image'];
                const ipfsID = image.substring(image.indexOf('Qm'));
                const imageURL = `https://image-optimizer.jpgstoreapis.com/${ipfsID}`

                document.getElementById(elem_prefix + assetinfo).innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo}, '${theme}');" class="show-hover-pointer padding" src='${imageURL}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>`;

            }
            else if (assetJson[0].token_registry_metadata) {
                //console.log("koios onchain image");
                document.getElementById(elem_prefix + assetinfo).innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo}, '${theme}');" class="show-hover-pointer padding" src='data:image/jpeg;base64,${assetJson.metadata.logo}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${assetJson.metadata.name}</div>`;
            }
            else {
                console.log("koios no metadata for nft")
            }
        }
        else {
            document.getElementById(elem_prefix + assetinfo).innerHTML = xhr.responseText;
        }
    };

    const koiosquery = `${koios_api_url}/asset_info?_asset_policy=${assetpolicy}&_asset_name=${assetname}`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();
}

function listNFTs(nftList, nftListHTMLElement, htmlprefix, theme, pool_policy_id, pool_nft_names) {
    var html = '';
    if (htmlprefix == '')
        htmlprefix = 'wallet';

    var doFiltering = false;
    if (pool_policy_id != undefined) {
        doFiltering = true;
    }

    if (nftList) {
        for (var i = 0; i < nftList.length; i++) {
            if (doFiltering && nftList[i].policy_id != pool_policy_id) { continue; }
            if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(nftList[i].asset_name) == -1) { continue; }
            html += `<div id='${htmlprefix}_nft_list_${nftList[i].policy_id}${nftList[i].asset_name}' class='not-selected${theme} padding'></div>`
        }
    }

    nftListHTMLElement.innerHTML = html;

    if (nftList) {
        for (var i = 0; i < nftList.length; i++) {

            if (doFiltering && nftList[i].policy_id != pool_policy_id) { continue; }
            if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(nftList[i].asset_name) == -1) { continue; }
            loadNFTInfoKoios(`${htmlprefix}_nft_list_`, `${nftList[i].policy_id}${nftList[i].asset_name}`, theme);

        }
    }

    updateWalletSelectionLabel();
}


function displayMBoxScriptFields() {
    hideElem('message-box-content');
    showElem('message-box-pool-script-row');
    hideElem('message-box-filter-row');
    showElem('message-box-pool-addr-row');
}

function displayMBoxFilteredScriptFields() {
    displayMBoxScriptFields()
    showElem('message-box-filter-row');
}

function displayMBoxMessageFieldOnly() {
    showElem('message-box-content');
    hideElem('message-box-pool-script-row');
    hideElem('message-box-filter-row');
    hideElem('message-box-pool-addr-row');
    hideElem('message-box-filter-row');
}

async function copyToClipboard(textToCopy) {
    try {
        await navigator.clipboard.writeText(textToCopy);
        console.log('Text copied to clipboard');
        /* Resolved - text copied to clipboard successfully */
    } catch (err) {
        console.error('Failed to copy: ', err);
        /* Rejected - text failed to copy to the clipboard */
    }
}

function resetOnClick(element) {
    if(element == undefined) {
        // no element provided. If add button is visible, reset add button onclick script. If not, reset remove button
        element = document.getElementById('confirmAddNFTsButton');
        if(element.style.display == 'none') {
            element = document.getElementById('confirmRemoveNFTsButton');
        }
    }

    var onclickJS = element.getAttribute('onclick');
    onclickJS = onclickJS.substring(onclickJS.indexOf('.'))
    element.setAttribute('onclick', onclickJS);
}

function loadAddNFTDropdown(dropdown, theme, swapPoolNames, poolPolicyIds, poolNFTNames) {

    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex" data-bs-toggle="modal" data-bs-target="#selectNFTsDialog"><a class="dropdown-item ${theme}" href="#" onclick="const confButton = document.getElementById('confirmAddNFTsButton'); confButton.setAttribute('onclick', 'addNFTsToPool(${i})'+ confButton.getAttribute('onclick')); setInnerText('selectNFTsDialogLabel', 'Select NFTs to add to swap pool'); showElem('confirmAddNFTsButton'); hideElem('confirmRemoveNFTsButton'); getWalletAddress().then((addr) => { getAddressAssets(addr).then((assets) => { listNFTs(assets, document.getElementById('selectable_nfts'), 'wallet', '${theme}', '${poolPolicyIds[i]}', '${poolNFTNames[i]}') } ) } ).catch((reason => console.log('error: '+ reason.message)));">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;

}

function loadRemoveNFTDropdown(dropdown, theme, swapPoolNames) {

    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex" data-bs-toggle="modal" data-bs-target="#selectNFTsDialog"><a class="dropdown-item ${theme}" href="#" onclick="const confButton = document.getElementById('confirmRemoveNFTsButton'); confButton.setAttribute('onclick', 'removeNFTsFromPool(${i})'+ confButton.getAttribute('onclick')); setInnerText('selectNFTsDialogLabel', 'Select NFTs to remove from swap pool'); showElem('confirmRemoveNFTsButton'); hideElem('confirmAddNFTsButton'); getSwapPoolAddress(${i}).then((addr) => { getAddressAssets(addr).then((assets) => { listNFTs(assets, document.getElementById('selectable_nfts'), 'pool', '${theme}') } ) } ).catch((reason => console.log('error: '+ reason.message)));">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;

}

function loadWithdrawalDropdown(dropdown, theme, swapPoolNames) {
    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="#" onclick="getSwapPoolUTxO(${i}).then(utxo => {withdrawFromPool(utxo[0].txHash,utxo[0].outputIndex, ${i})}).then(message => { message = JSON.stringify(message); const mBoxTitle = document.getElementById('messageBoxLabel'); if(message.indexOf('Redeemer') > -1 || message.indexOf('Error') > -1) { mBoxTitle.innerText='Something went wrong'; } else { mBoxTitle.innerText='Deposit successful'; } document.getElementById('message-box-content').innerHTML=message; const messageBox = new bootstrap.Modal('#messageBox', { keyboard: false }); messageBox.show();});">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;
}

function loadDepositDropdown(dropdown, theme, swapPoolNames) {
    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="#" onclick="depositLovelace(3000000, ${i}).then(message => { message = JSON.stringify(message); const mBoxTitle = document.getElementById('messageBoxLabel'); if(message.indexOf('Redeemer') > -1 || message.indexOf('Error') > -1) { mBoxTitle.innerText='Something went wrong'; } else { mBoxTitle.innerText='Deposit successful'; } document.getElementById('message-box-content').innerHTML=message; const messageBox = new bootstrap.Modal('#messageBox', { keyboard: false }); messageBox.show();});">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;
}

function loadSwapPoolDropdown(dropdown, button, theme, swapPoolNames, selectedIndex) {

    var swapPoolListHtml = '';

    if(isNaN(selectedIndex) ||
        selectedIndex == undefined || 
        selectedIndex < 0 || 
        (selectedIndex > swapPoolNames.length -1)) selectedIndex = 0; 

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            
            if(selectedIndex == i) {
                swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="/?poolIndex=${i}">${swapPoolNames[i].trim()}</a><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16"><path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/></svg></div></li>`;
                button.innerHTML = `<a class="connect-button${theme}" href="#">${swapPoolNames[i].trim()}</a>`;
            }
            else {
                swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="/?poolIndex=${i}">${swapPoolNames[i].trim()}</a></div></li>`;
            }
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;

}

function delay(time) {
    return new Promise(resolve => setTimeout(resolve, time));
}

function getCookie(cookiename) {
    var cookieString = document.cookie;
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

function setCookie(cookiename, value) {
    const expirationDate = new Date();
    expirationDate.setMonth(expirationDate.getMonth()+1);
    
    document.cookie = `${cookiename}=${value}; expires=${expirationDate};`;
}

async function loadWalletConnector(dropdown, button, theme) {

    const nonWalletNames = ['enable', 'isEnabled', 'getBalance', 'signData', 'signTx', 'submitTx', 'getUtxos', 'getCollateral', 'getUsedAddresses', 'getUnusedAddresses', 'getChangeAddress', 'getRewardAddress', 'getNetworkId', 'onAccountChange', 'onNetworkChange', 'off', '_events'];

    connectorDropdown = dropdown;
    connectorButton = button;

    const cardanowallets = window.cardano;

    var walletListHtml = '';
    
    connectedWalletExtName = getCookie('connectedwallet');

    console.log('connecting to '+ connectedWalletExtName);

    for(var i = 0; i < Object.keys(cardanowallets).length; i++) {
        
        currentWalletName = Object.keys(cardanowallets)[i];
        if(nonWalletNames.includes(currentWalletName)) {continue;}
        wallet = cardanowallets[currentWalletName];
        if (connectedWalletExtName == currentWalletName) {
            
            const api = await wallet.enable();
            console.log(`${wallet.name} enabled`);

            if (wallet.name && wallet.icon) {

                if (await wallet.isEnabled()) {
                    try {
                        
                        var lucid;
                        var retries = 0;
                        while(lucid == null && retries < 3) {
                            try {
                                // some wallets time out...retry before giving up
                                lucid = await connectToLucid();
                            }
                            catch(e) { 
                                console.log(`Connection to ${wallet.name} failed. Retrying...`);
                                retries++; 
                                await delay(500); 
                            }
                        }
                        if(lucid == null) {
                            // unable to connect to wallet. Display warning icon
                            console.log(`unable to initialize ${wallet.name}`);
                            walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-exclamation-circle" viewBox="0 0 16 16">
                            <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
                            <path d="M7.002 11a1 1 0 1 1 2 0 1 1 0 0 1-2 0zM7.1 4.995a.905.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 4.995z"/>
                            </svg>&nbsp;<a class="dropdown-item ${theme}" href="#">${wallet.name}</a></div></li>`
                            button.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-exclamation-circle" viewBox="0 0 16 16">
                            <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
                            <path d="M7.002 11a1 1 0 1 1 2 0 1 1 0 0 1-2 0zM7.1 4.995a.905.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 4.995z"/>
                            </svg>&nbsp;<a class="connect-button${theme}" href="#">${wallet.name}</a>`
                            continue;
                        }
                        
                        const utxos = await lucid.wallet.getUtxos();
                        const lovelace = utxos.reduce((acc, utxo) => acc + utxo.assets.lovelace, 0n);
                        const adaBalance = lovelace / 1000000n;
                        walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><img src="${wallet.icon}" width="30" height="30"/><a class="dropdown-item ${theme}" href="#">${adaBalance} ADA</a><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16"><path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/></svg></div></li>`
                        button.innerHTML = `<img src="${wallet.icon}" width="30" height="30"/>&nbsp;<a class="connect-button${theme}" href="#">${adaBalance} ADA</a>`
                        connectedWallet = wallet;
                        
                        // typhon must be changed to typhoncip30 to be compliant with the dapp connector api
                        if (connectedWalletExtName == 'typhon') {
                            connectedWalletExtName = 'typhoncip30';
                        }
                    }
                    catch (err) {
                        console.error(err)
                    }
                }
            }
        }
        else {
            walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><img src="${wallet.icon}" width="30" height="30"/><a class="dropdown-item ${theme}" href="#" onclick="connectwallet('${currentWalletName}')">${wallet.name.toUpperCase()}</a></div></li>`
        }
    }

    dropdown.innerHTML = walletListHtml;

    if (connectedWallet == null) {
        button.innerHTML = "CONNECT WALLET";
    }
}

async function connectwallet(name) {
    const api = await window.cardano[name].enable();
    setCookie('connectedwallet', name);
    location.reload();
}