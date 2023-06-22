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

function togglePoolNFTSelection(nftimage, theme) {
    var selectButton = document.getElementById('selectPoolNFTButton');
    var confirmButton = document.getElementById('confirmSwapButton')
    var nameDiv = document.getElementById(nftimage.id + "_namediv");

    if (nftimage.id.indexOf('pool_nft_list_') == 0) {
        if (selectedPoolNFTs.indexOf(nftimage.id) > -1) {
            console.log(`${nftimage.id} found in selected NFTs`);
            selectedPoolNFTs = selectedPoolNFTs.filter(e => e !== nftimage.id);
            nftimage.classList.remove("selected" + theme);
            nftimage.classList.add("not-selected" + theme);
            nameDiv.classList.remove("selected-no-border" + theme);
            nameDiv.classList.add("not-selected" + theme);
            numSelectedPoolNFTs = selectedPoolNFTs.length;
        }
        else {
            console.log(`${nftimage.id} not found in selected NFTs`);
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
            numSelectedPoolNFTs = selectedPoolNFTs.push(nftimage.id);
        }
    }
    else {
        if (selectedWalletNFTs.indexOf(nftimage.id) > -1) {
            console.log(`${nftimage.id} found in selected NFTs`);
            selectedWalletNFTs = selectedWalletNFTs.filter(e => e !== nftimage.id);
            nftimage.classList.remove("selected" + theme);
            nftimage.classList.add("not-selected" + theme);
            nameDiv.classList.remove("selected-no-border" + theme);
            nameDiv.classList.add("not-selected" + theme);
            numSelectedWalletNFTs = selectedWalletNFTs.length;
        }
        else {
            console.log(`${nftimage.id} not found in selected NFTs`);
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            nameDiv.classList.add("selected-no-border" + theme);
            nameDiv.classList.remove("not-selected" + theme);
            numSelectedWalletNFTs = selectedWalletNFTs.push(nftimage.id);
        }
    }

    if (numSelectedPoolNFTs > 0) {
        selectButton.classList.remove("disabled");
    }
    else {
        selectButton.classList.add("disabled");
    }

    if (numSelectedPoolNFTs != numSelectedWalletNFTs) {
        confirmButton.classList.add("disabled");
    }
    else {
        confirmButton.classList.remove("disabled");
    }

    var numSelectedFromPoolLabelTop = document.getElementById('num_selected');
    numSelectedFromPoolLabelTop.innerText = `${numSelectedPoolNFTs} SELECTED`;

    var numSelectedFromWalletLabel = document.getElementById('selectNFTsDialogLabel');
    numSelectedFromWalletLabel.innerText = `Select ${numSelectedPoolNFTs - numSelectedWalletNFTs} NFTs to swap`;
    console.log(`${numSelectedPoolNFTs} Pool NFTs SELECTED`)
    console.log(`${numSelectedWalletNFTs} Wallet NFTs SELECTED`)

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
        console.log(`${nftimage.id} found in selected NFTs`);
        selectedPoolNFTs = selectedPoolNFTs.filter(e => e !== nftimage.id);
        nftimage.classList.remove("selected" + theme);
        nftimage.classList.add("not-selected" + theme);
        nameDiv.classList.remove("selected-no-border" + theme);
        nameDiv.classList.add("not-selected" + theme);
        numSelectedPoolNFTs = selectedPoolNFTs.length;
        selectedFilterNFTNameMap.delete(nftimage.id);
    }
    else {
        console.log(`${nftimage.id} not found in selected NFTs`);
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
    console.log('getAddressAssets - koios()');

    var xhr = new XMLHttpRequest();

    const koiosquery = `${koios_api_url}/address_assets`;
    console.log('koiosquery', koiosquery);
    xhr.open('POST', koiosquery, false);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(`{"_addresses":["${address}"]}`);

    console.log(`koios xhr.status ${xhr.status}`)
    if (xhr.status === 200) {
        addressAssetsJSON = JSON.parse(xhr.response);
        console.log("addressAssetsJSON", addressAssetsJSON);

        if (addressAssetsJSON[0]['asset_list']) {
            console.log('asset_list', addressAssetsJSON[0]['asset_list']);
            return addressAssetsJSON[0]['asset_list'];
        }
        else {
            console.log('no asset_list');
        }
    }
    return {};
}

async function loadPolicyAssets(assetpolicy, list_html_element, filter, theme) {

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {

        if (xhr.status === 200) {

            var assetJson = JSON.parse(xhr.responseText);

            var htmlList = '<div class="nft-list light"><div class="light" style="margin-bottom: 10px">Select the NFTs you want to allow to swap in the swap pool</div><div class="flex-row d-flex mb-3 flex-wrap padding">';
            for (let i = 0; i < assetJson.length; i++) {
                let asset = assetJson[i];

                if (asset.minting_tx_metadata) {
                    if (!asset.minting_tx_metadata['721']) { continue; }
                    const asset_policy = asset.minting_tx_metadata['721'][assetpolicy];

                    const metadata = asset_policy[asset.asset_name_ascii];

                    if (filter.trim() != '') {
                        // a filter criterion was provided. Skip if criterion is not satisfied
                        const metadataString = JSON.stringify(metadata);
                        if (metadataString.toLowerCase().indexOf(filter.toLowerCase()) == -1) { continue; }
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
    };

    const koiosquery = `${koios_api_url}/policy_asset_info?_asset_policy=${assetpolicy}`;
    xhr.open('GET', koiosquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();
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

    console.log("listNFTs - nftList", nftList);


    var doFiltering = false;
    if (pool_policy_id != undefined) {
        doFiltering = true;
    }

    if (doFiltering) {
        console.log(`policy id desired by the pool ${pool_policy_id}`);
        console.log(`NFT names desired by the pool ${pool_nft_names}`);
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

async function loadWalletConnector(dropdown, button, theme) {

    connectorDropdown = dropdown;
    connectorButton = button;

    const cardanowallets = window.cardano;

    console.log('cardanowallets', Object.keys(cardanowallets));


    var walletListHtml = '';
    var connectedWalletHtml = '';

    //for(var i = 0; i < Object.keys(cardanowallets).length; i++) {
    //wallet = cardanowallets[Object.keys(cardanowallets)[i]];
    wallet = cardanowallets['nami'];
    const api = await wallet.enable();
    console.log('nami enabled', api);

    if (wallet.name && wallet.icon) {

        if (await wallet.isEnabled()) {
            try {
                console.log('wallet', wallet);

                adaBalance = 0;
                walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><img src="${wallet.icon}" width="30" height="30"/><a class="dropdown-item${theme}" href="#">${adaBalance} ADA</a><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16"><path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/></svg></div></li>`
                //button.innerHTML = `${wallet.name.toUpperCase()} CONNECTED`;
                button.innerHTML = `<img src="${wallet.icon}" width="30" height="30"/>&nbsp;<a class="connect-button${theme}" href="#">${adaBalance} ADA</a>`
                connectedWallet = wallet;
                //connectedWalletExtName = Object.keys(cardanowallets)[i];
                connectedWalletExtName = 'nami'

                // typhon must be changed to typhoncip30 to be compliant with the dapp connector api
                if (connectedWalletExtName == 'typhon') {
                    connectedWalletExtName = 'typhoncip30';
                }


            }
            catch (err) {
                console.error(err)
            }

            //i = Object.keys(cardanowallets).length; // break loop...found connected wallet
        }
        else {
            walletListHtml += `<li><div class="dropdown-item d-flex"><img src="${wallet.icon}" width="30" height="30"/><a class="dropdown-item" href="#" onclick="connectwallet('${Object.keys(cardanowallets)[i]}')">${wallet.name.toUpperCase()}</a></div></li>`
        }
    }
    //}

    dropdown.innerHTML = walletListHtml;

    if (connectedWallet == null) {

        button.innerHTML = "CONNECT WALLET";
    }


}

async function connectwallet(name) {
    console.log("connect wallet called");
    const api = await window.cardano[name].enable();
}