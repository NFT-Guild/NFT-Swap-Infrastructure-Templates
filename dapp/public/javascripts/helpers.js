var connectedWallet = undefined;
var connectedWalletExtName = '';
var connectorDropdown = undefined;
var connectorButton = undefined;
var selectedPoolNFTs = [];
var selectedWalletNFTs = [];
var numSelectedPoolNFTs = 0;
var numSelectedWalletNFTs = 0;
var selectedFilterNFTNameMap = new Map();

var currentPolicyId, currentNFTNames, currentSwapPoolIndex, currentRules;
var LOADED_NFT_LIST = [];
var LOADED_POOL_NFTS = [];
var LOADED_WALLET_NFTS = [];

var VIEW_OPTIONS;

const listTypes = [
    'TEXT LIST', 
    'SMALL THUMBNAILS',
    'LARGE THUMBNAILS'
];

// object with settings determining how the NFT lists look and what NFTs they contain
const VIEW_OPTIONS_POOL = {
    disableSelection: false,
    viewAs: listTypes[2],
    viewRarity: [],
    viewTrait: [],
    searchCriterion: ''
};

const VIEW_OPTIONS_WALLET = {
    disableSelection: false,
    viewAs: listTypes[2],
    viewRarity: [],
    viewTrait: [],
    searchCriterion: ''
};

function setDefaultViewOptions(type) {
    if(type == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(type == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    VIEW_OPTIONS.disableSelection = false;
    VIEW_OPTIONS.viewAs = listTypes[2];
    VIEW_OPTIONS.viewRarity = [];
    VIEW_OPTIONS.viewTrait = [];
    VIEW_OPTIONS.searchCriterion = '';

    document.getElementById('search_field_input_'+type).value = '';
}

function toggleViewOptionsViewAs(view, addrType) {
    const viewIndex = listTypes.indexOf(view);
    
    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    if(viewIndex > -1) {
        // view is valid...set as current view as
        VIEW_OPTIONS.viewAs = listTypes[viewIndex];
    }
    else {
        // view invalid...reset to default
        VIEW_OPTIONS.viewAs = listTypes[2];
    }
}

function toggleViewOptionsRarity(rarity, addrType) {

    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    const rarityIndex = VIEW_OPTIONS.viewRarity.indexOf(rarity);
    if(rarityIndex > -1) {
        // rarity exists...remove rarity from view options
        VIEW_OPTIONS.viewRarity = VIEW_OPTIONS.viewRarity.filter(r => r != rarity);
    }
    else {
        // rarity doesn't exist...add to view options
        VIEW_OPTIONS.viewRarity.push(rarity);
    }
}

function toggleViewOptionsTrait(trait, addrType) {

    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    const traitIndex = VIEW_OPTIONS.viewTrait.indexOf(trait);
    if(traitIndex > -1) {
        // trait exists...remove trait from view options
        VIEW_OPTIONS.viewTrait = VIEW_OPTIONS.viewTrait.filter(t => t != trait);
    }
    else {
        // trait doesn't exist...add to view options
        VIEW_OPTIONS.viewTrait.push(trait);
    }
}

function updateDropdownSelection(prefix, type) {

    var addrType = '';
    if(prefix.indexOf('_pool') > -1) {
        addrType = 'pool';
    }
    else {
        addrType = 'wallet';
    }
    
    var optionElement, checkElement;
    const options = document.querySelectorAll(`[id^=${prefix}_${type}]`);
    
    for(var j = 0; j < options.length; j++) {
        optionElement = options[j];
        
        optionName = optionElement.id.split("_");

        partOfFilter = isSelected(optionName[4], type, addrType);
        
        if(partOfFilter) {
            // rarity is part of filter
            checkElement = getCheckIfExists(optionName[4], type, addrType);

            // rarity is selected. Add check if not present
            if(checkElement == null) {
                optionElement.innerHTML = optionElement.innerHTML + `<svg id="check_${addrType}_${type}_${optionName[4]}_" xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16"><path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z" /></svg>`;
            }
        }
        else {
            // rarity is not part of filter
            checkElement = getCheckIfExists(optionName[4], type, addrType);

            if(checkElement != null) {
                optionElement.removeChild(checkElement);
            }
        }
    }
}

function isSelected(name, type, addrType) {
    if(type == 'rarity') {
        return isSelectedRarity(name, addrType);
    }
    else if(type == 'trait') {
        return isSelectedTrait(name, addrType);
    }
    else if(type == 'view') {
        return isSelectedViewAs(name, addrType);
    }
    console.error('unsupported dropdown type '+ type);
    return false;
}

function getCheckIfExists(name, type, addrType) {
    const checkElements = document.querySelectorAll(`[id^='check_${addrType}_${type}_']`);
    var checkElement = null;

    if(checkElements.length > 0) {
        for(var c = 0; c < checkElements.length; c++) {
            if(checkElements[c].id == `check_${addrType}_${type}_${name}_`) {
                checkElement = checkElements[c];
            }
        }
    }
    return checkElement;
}

function isSelectedViewAs(view, addrType) {
    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }
    return VIEW_OPTIONS.viewAs == view
}

function isSelectedRarity(rarity, addrType) {
    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }
    return VIEW_OPTIONS.viewRarity.indexOf(rarity) > -1
}

function isSelectedTrait(trait, addrType) {
    if(addrType == 'pool') {
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else if(addrType == 'wallet') {
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }
    return VIEW_OPTIONS.viewTrait.indexOf(trait) > -1
}

function removeTechnicalGibberish(message) {
    if(message == null || message == undefined) { return ''; }

    if(typeof message === 'object') { message = JSON.stringify(message); }

    message = message.toLowerCase();

    if(message.indexOf('redeemer') > -1) {

        if(message.indexOf('over budget') > -1) {
            // example: 
            // Redeemer (Spend, 1): Over budget mem: -1036 & cpu: 6310129259 ExBudget { mem: -1036, cpu: 6310129259,}
            message = 'Tx too large to be processed by the swap pool smart contract.<br>Please remove one NFT and try again.';
        }
        else {
            // example:
            // Redeemer (Spend, 2): The provided Plutus code called 'error'. ExBudget { mem: 12740384, cpu: 9611748796, } CLEANUP FAILED:
            message = message.substring(message.indexOf('}'));
            
            // example:
            // } CLEANUP FAILED: Operation can only be performed by contract owner PT5
            message = message.replace('}','');
            message = message.replace('pt5', '');
            message.trim();
        }
    }
    else if(message.indexOf('"info":') > -1) {
        // message is a JSON string with an info object. Return content of info
        const msgJSON = JSON.parse(message);
        message = msgJSON['info']; 
    }
    else if(message.indexOf('inputsexhaustederror') > -1) {
        message = 'Please verify that your wallet has more than 5 ADA.<br>Refresh page and try again.';
    }

    return toProperCase(message);
}

function toProperCase(msg) {

    if(msg.trim() == '') return '';

    const words = msg.trim().replace(/\s+/g, ' ').split(' ');

    for (let i = 0; i < words.length; i++) {
        words[i] = words[i][0].toUpperCase() + words[i].substr(1);
    }

    return words.join(" ");
}

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

function updateSettingsVisualized(ruleDisplayElem, exampleDisplayElem) {

    var nftTokenNameRuleVisualized = ''; 
    // nft name starts with
    const nftNameStart = document.getElementById('dlgNamePrefixInput').value;
    if(nftNameStart == "") { 
        document.getElementById(ruleDisplayElem).innerText = '';
        return;
    }

    nftTokenNameRuleVisualized = nftNameStart;

    // nft digit start position
    const nftDigitPosStartString = document.getElementById('dlgDigitStartInput').value;
    var nftDigitPosStart = -1;
    try {
        nftDigitPosStart = parseInt(nftDigitPosStartString);
    }
    catch(err) {
        nftDigitPosStart = nftNameStart.length;
    }

    var charCode = 65;
    for(var i = nftNameStart.length; i < nftDigitPosStart; i++) {
        // add ABCD... until the digit start position is reached
        nftTokenNameRuleVisualized += String.fromCharCode(charCode);
        if(charCode < 90) {
            charCode++;
        }
        else {
            charCode = 65;
        }
    }

    // nft digit length
    const nftDigitLengthString = document.getElementById('dlgDigitLengthInput').value;
    var nftDigitLength = -1;
    try {
        nftDigitLength = parseInt(nftDigitLengthString);
    }
    catch(err) {
        nftDigitLength = nftNameStart.length;
    }

    var nftTokenNameExampleVisualized = nftTokenNameRuleVisualized;
 
    for(var i = 0; i < nftDigitLength; i++) {
        // add 0000... until the digit length is reached
        nftTokenNameRuleVisualized += '0';
    }

    // nft digit range start
    const nftDigitRangeStartString = document.getElementById('dlgDigitRangeStartInput').value;
    var startDigitInRange = -1;
    try {
        startDigitInRange = parseInt(nftDigitRangeStartString);
    }
    catch(err) {}

    // nft digit range end
    const nftDigitRangeEndString = document.getElementById('dlgDigitRangeEndInput').value;
    var endDigitInRange = -1;
    try {
        endDigitInRange = parseInt(nftDigitRangeEndString);
    }
    catch(err) {}

    // calculating an example nft name
    if(startDigitInRange > -1 && endDigitInRange > -1 && endDigitInRange > startDigitInRange) {
        randomNumInRange = randomIntFromInterval(startDigitInRange, endDigitInRange);
        document.getElementById(exampleDisplayElem).innerText = nftTokenNameExampleVisualized + intToStringOfLength(randomNumInRange, nftDigitLength);
        setActive('verifyRulesCheck');
    }
    else {
        document.getElementById(exampleDisplayElem).innerText = 'More info needed';
        setDisabled('verifyRulesCheck');
    }

    document.getElementById(ruleDisplayElem).innerText = nftTokenNameRuleVisualized;
}

function verifyDlgFields() {
    const dlgInputs = document.querySelectorAll("input[id^='dlg']");
    for(var i = 0; i < dlgInputs.length; i++) {
        if(dlgInputs.item(i).value.trim() == '') {
            return false;
        }
    }
    return true;
}

function clearRuleContractBuilderDialog() {
    const dlgInputs = document.querySelectorAll("input[id^='dlg']");
    for(var i = 0; i < dlgInputs.length; i++) {
        dlgInputs.item(i).value= '';
    }

    document.getElementById('settingsVisualized').innerText = '';
    document.getElementById('exampleVisualized').innerText = '';
    document.getElementById('verifyRulesCheck').checked = false;
    setDisabled('confirmCreateRuleButton');
}

function getInputValue(inputId) {
    var inputValue = "";
    try {
        inputValue = document.getElementById(inputId).value
    }
    catch(error) {}
    return inputValue;
}
 
function setActive(idToActivate) {
    const elemToActivate = document.getElementById(idToActivate);
    elemToActivate.removeAttribute('disabled');
}

function setDisabled(idToDisable) {
    const elemToDisable = document.getElementById(idToDisable);
    elemToDisable.setAttribute('disabled','disabled');
}

function randomIntFromInterval(min, max) { // min and max included 
    return Math.floor(Math.random() * (max - min + 1) + min)
}

function intToStringOfLength(intValue, length) {
    var intValueString = intValue.toString();
    for(var i = intValueString.length; i < length; i++) {
        // prepend 0 until the desired length is reached
        intValueString = '0'+intValueString;
    }
    return intValueString;
}

function toggleSelectedNFTs(theme) {
    for (var i = 0; i < selectedPoolNFTs.length; i++) {
        toggleNFTSelectionViewOnly(`${selectedPoolNFTs[i]}`, theme);
    }
    for (var i = 0; i < selectedWalletNFTs.length; i++) {
        toggleNFTSelectionViewOnly(`${selectedWalletNFTs[i]}`, theme);
    }
}

function updateWalletSelectionLabel() {

    var numSelectedFromWalletLabel = document.getElementById('selectNFTsDialogLabel');
    if(numSelectedPoolNFTs == 0) {
        numSelectedFromWalletLabel.innerText = `Select NFTs to swap`;
    }
    else if(numSelectedFromWalletLabel.innerText.indexOf('NFTs to swap') > 0) {
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

function toggleNFTSelectionViewOnly(nftimageid, theme) {
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
            if(nameDiv != null) {
                nameDiv.classList.add("selected-no-border" + theme);
                nameDiv.classList.remove("not-selected" + theme);
            }
        }
    }
    else {
        if (selectedWalletNFTs.indexOf(nftimageid) > -1) {
            // NFT is selected. set style according to active selection
            nftimage.classList.add("selected" + theme);
            nftimage.classList.remove("not-selected" + theme);
            if(nameDiv != null) {
                nameDiv.classList.add("selected-no-border" + theme);
                nameDiv.classList.remove("not-selected" + theme);
            }
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
        if(numSelectedPoolNFTs > 0) {
            // at least 1 pool NFT has been selected
            if (numSelectedPoolNFTs != numSelectedWalletNFTs) {
                confirmButton.classList.add("disabled");
            }
            else {
                confirmButton.classList.remove("disabled");
            }
        }
        else if(numSelectedPoolNFTs == 0) {
            // no pool NFT has been selected
            if(numSelectedWalletNFTs > 0) {
                confirmButton.classList.remove("disabled");
            }
            else {
                confirmButton.classList.add("disabled");
            }
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

    var numSelectedFromPoolLabelTop = document.getElementById('num_selected');
    numSelectedFromPoolLabelTop.innerText = `${numSelectedPoolNFTs} SELECTED`;

}


async function getAddressTxs(address) {
    
    var xhr = new XMLHttpRequest();
    var koiosparams = '';

    const apiquery = '/api_address_txs';

    if(typeof address === 'string' || address instanceof String) {
        koiosparams = `{"_addresses":["${address}"]}`;
    }
    else {
        return {};
    }

    xhr.open('POST', apiquery, false);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(koiosparams);

    if (xhr.status === 200) {
        addressAssetsJSON = JSON.parse(xhr.response);
        return addressAssetsJSON; 
    }
    return {};
}


async function getAddressAssets(address) {
    
    var xhr = new XMLHttpRequest();
    var koiosparams = '';

    const apiquery = '/api_address_assets';

    if(typeof address === 'string' || address instanceof String) {
        koiosparams = `{"_addresses":["${address}"]}`;
    }
    else {
        koiosparams = `{"_stake_addresses":${JSON.stringify(address)}}`
    }

    xhr.open('POST', apiquery, false);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.setRequestHeader('content-type', 'application/json');
    xhr.send(koiosparams);

    if (xhr.status === 200) {
        addressAssetsJSON = JSON.parse(xhr.response);
        return addressAssetsJSON; 
    }
    return {};
}

async function loadPolicyAssets(assetpolicy, list_html_element, filter, theme, pOffset, pLimit) {

    var xhr = new XMLHttpRequest();
    xhr.onload = function () {

        if (xhr.status === 200) {

            var assetJson = JSON.parse(xhr.responseText);

            var htmlList = '<div class="nft-list light"><div class="flex-row d-flex mb-3 flex-wrap padding">';
            var numberOfResults = 0;
            var nftsOnThePage = 0;
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
                            // We are making use of a free IPFS image provider. Feel free to change it to
                            // your favorite image provider
                            imageURL = `https://ipfs.blockfrost.dev/ipfs/${ipfsID}`;
                        }
                    }
                    catch (error) { console.log(error); }

                    nftsOnThePage++;
                    // no filter applied. Offset and limiting done in query. Simply output the result item
                    htmlList += `<div class="not-selected${theme} padding" id="pool_filter_nft_list_${asset.fingerprint}"><img id="pool_filter_nft_list_${asset.fingerprint}_img" loading="lazy" height="200" onclick="togglePoolFilterSelection(pool_filter_nft_list_${asset.fingerprint}, '${asset.asset_name}', '${theme}');" class="show-hover-pointer padding" src="${imageURL}"></img><div id="pool_filter_nft_list_${asset.fingerprint}_namediv" class="nft-name-display not-selected${theme} align-bottom">${metadata['name']}</div></div>`;
                }
                else if (asset.token_registry_metadata) {
                    //NOT IMPLEMENTED. MAIN USE CASE IS NFTs AT THIS POINT 
                }
            }

            htmlList += '</div>';
            document.getElementById(list_html_element).innerHTML = htmlList;

        }
        else {
            document.getElementById(list_html_element).innerHTML = xhr.responseText;
        }

        if(nftsOnThePage < pLimit) {
            // we are now displaying fewer results than the pLimit. There are no more results to display. Disable paging
            disableHigherNavPages('policy');
        }
        else {
            enableHigherNavPages('policy');
        }
    
        toggleSelectedNFTs(`${theme}`)

    };

    if(pOffset == undefined) pOffset = 0;   // set default if undefined
    if(pLimit == undefined) pLimit = 50;    // set default if undefined
    var offsetParam = `&offset=${pOffset+1}`; // increase with 1 because of how Koios interprets the value
    var limitParam = `&limit=${pLimit}`;
    
    if(filter != '') {
        offsetParam = '';
        limitParam = '';       // leave out parameters if filter is applied. Do offset and limiting after fetch to ensure non-empty pages
    }

    const apiquery = `/api_policy_asset_info?policy_id=${assetpolicy}&filter=${filter}&order=asset_name_ascii.asc${offsetParam}${limitParam}`;
    xhr.open('GET', apiquery, true);
    xhr.setRequestHeader('accept', 'application/json');
    xhr.send();
}

function stepPageNext(prefix) {
    const currentPage = getActivePage(prefix);
    var navPageElem = document.getElementById(`${prefix}page3`);
    if(parseInt(navPageElem.innerText) == currentPage) {
        // we are on the last page of the set...navigate to first page of next set
        stepPageSetNext(prefix);
    }
    else {
        setActivePage(currentPage + 1, prefix);
    }
}

function stepPagePrev(prefix) {
    const currentPage = getActivePage(prefix);
    var navPageElem = document.getElementById(`${prefix}page1`);
    if(parseInt(navPageElem.innerText) == currentPage) {
        // we are on the first page of the set...navigate to last page of previous set
        stepPageSetPrev(prefix);
    }
    else {
        setActivePage(currentPage - 1, prefix);
    }
}

function updateStepPrev(prefix) {
    // enable / disable step prev page
    if(getActivePage(prefix) > 1) {
        document.getElementById(`${prefix}pageprevious`).parentElement.classList.remove('disabled');
    }
    else {
        document.getElementById(`${prefix}pageprevious`).parentElement.classList.add('disabled');
        document.getElementById(`${prefix}pagesetprevious`).parentElement.classList.add('disabled');
    }

    var navPageElem = document.getElementById(`${prefix}page1`);
    if(parseInt(navPageElem.innerText) == 1) {
        document.getElementById(`${prefix}pagesetprevious`).parentElement.classList.add('disabled');
    }
    else {
        document.getElementById(`${prefix}pagesetprevious`).parentElement.classList.remove('disabled');
    }
}

function stepPageSetNext(prefix) {

    // update page number of the 3 numbered pages and clear active state of all pages
    var navPageElem, navPageNumOld;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
        navPageNumOld = parseInt(navPageElem.innerText);
        navPageElem.innerText = navPageNumOld + 3;
        navPageElem.classList.remove('active');
        navPageElem.classList.remove('disabled');
    }

    // set first page in set as active page 
    navPageElem = document.getElementById(`${prefix}page1`);
    setActivePage(parseInt(navPageElem.innerText), prefix);

    // enable / disable PREVIOUS
    updateStepPrev(prefix);
}

function stepPageSetPrev(prefix) {

    // update page number of the 3 numbered pages and clear active state of all pages
    var navPageElem, navPageNumOld;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
        navPageNumOld = parseInt(navPageElem.innerText);
        navPageElem.innerText = navPageNumOld - 3;
        navPageElem.classList.remove('active');
        navPageElem.classList.remove('disabled');
    }

    // set last page in set active page
    navPageElem = document.getElementById(`${prefix}page3`);
    setActivePage(parseInt(navPageElem.innerText), prefix);

    // enable / disable PREVIOUS
    updateStepPrev(prefix);
}

function setActivePage(pNum, prefix) {

    var navPageElem;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
        
        if(parseInt(navPageElem.innerText) == pNum) {
            navPageElem.classList.add('active');
        }
        else {
            navPageElem.classList.remove('active');
        }
    }
    updateStepPrev(prefix);
}

function getActivePage(prefix) {
    var navPageElem;
    for(var i = 1; i <= 3; i++) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
        if(navPageElem.classList.contains('active')) {
            return parseInt(navPageElem.innerText);
        }
    }
    // if this point is reached, no page is active
    return -1;
}

function disableHigherNavPages(prefix) {
    var navPageElem;
    
    // disable next pager
    navPageElem = document.getElementById(`${prefix}pagenext`);
    navPageElem.classList.add('disabled');
    // disable next set pager
    navPageElem = document.getElementById(`${prefix}pagesetnext`);
    navPageElem.classList.add('disabled');

    // loop through pages from the top and disable until active page is found
    for(var i = 3; i >= 1; i--) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
        if(navPageElem.classList.contains('active')) break;

        navPageElem.classList.add('disabled');
    }
}

function enableHigherNavPages(prefix) {
    var navPageElem;
    
    // enable next pager
    navPageElem = document.getElementById(`${prefix}pagenext`);
    navPageElem.classList.remove('disabled');

    // enable next set pager
    navPageElem = document.getElementById(`${prefix}pagesetnext`);
    navPageElem.classList.remove('disabled');

    // loop through pages from the top and enable until active page is found
    for(var i = 3; i >= 1; i--) {
        navPageElem = document.getElementById(`${prefix}page${i}`);
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

function addExtendedInfo(elem_prefix, assetinfo, theme, nftIndex) {

    const assetpolicy = assetinfo.substring(0, 56)
    const assetname = assetinfo.substring(56);

    const assetNameAscii = hex_to_ascii(assetname);
    const nft_html_elem = document.getElementById(elem_prefix + assetinfo);

    if(elem_prefix.indexOf('pool_') == 0) {
        LOADED_NFT_LIST = LOADED_POOL_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else {
        LOADED_NFT_LIST = LOADED_WALLET_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }
       
    const extendedInfo = LOADED_NFT_LIST[nftIndex].extended; 

    if (extendedInfo.minting_tx_metadata) {
        const koios_policy = extendedInfo.minting_tx_metadata['721'][assetpolicy];

        const asset = koios_policy[assetNameAscii];

        const image = asset['image'];
        const ipfsID = image.substring(image.indexOf('Qm'));
        // We are making use of a free IPFS image provider. Feel free to change it to
        // your favorite image provider
        const imageURL = `https://ipfs.blockfrost.dev/ipfs/${ipfsID}`;

        var imgSize = 100;
        if(VIEW_OPTIONS.viewAs == listTypes[2]) {
            // display large images
            imgSize = 200;
        }

        if(VIEW_OPTIONS.viewAs != listTypes[0]) {
            if(!VIEW_OPTIONS.disableSelection) {
                nft_html_elem.innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="${imgSize}" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo}, '${theme}');" class="show-hover-pointer padding" src='${imageURL}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>`;
            }
            else {
                nft_html_elem.innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="${imgSize}" class="padding" src='${imageURL}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>`;
            }
        }
        else {
            // Displaying nfts as a text list. Adding some more properties
            if(!VIEW_OPTIONS.disableSelection) {
                nft_html_elem.innerHTML = `
                <img id="${elem_prefix + assetinfo}_img" loading="lazy" height="${imgSize}" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo}, '${theme}');" class="show-hover-pointer padding" src='${imageURL}'>
                <div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>
                <div id="${elem_prefix + assetinfo}_descriptiondiv" class="nft-name-display not-selected${theme} align-bottom">${asset['description']}</div>
                <div id="${elem_prefix + assetinfo}_raritydiv" class="nft-name-display not-selected${theme} align-bottom">${asset['rarity']}</div>
                `;
            }
            else {
                nft_html_elem.innerHTML = `
                <img id="${elem_prefix + assetinfo}_img" loading="lazy" height="${imgSize}" class="padding" src='${imageURL}'>
                <div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>
                <div id="${elem_prefix + assetinfo}_descriptiondiv" class="nft-name-display not-selected${theme} align-bottom">${asset['description']}</div>
                <div id="${elem_prefix + assetinfo}_raritydiv" class="nft-name-display not-selected${theme} align-bottom">${asset['rarity']}</div>
                `;
            }
        }
    }
    else {
        console.log("koios no metadata for nft")
    }
}

async function getAssetMetadata(assetpolicy, assetname) {
    return new Promise((resolve, reject) => {
        var xhr = new XMLHttpRequest();
        xhr.onload = function () {
            if (xhr.status === 200) {
                resolve(xhr.responseText);
            } else {
                resolve('Unable to fetch metadata for asset');
            }
        };
        xhr.onerror = function() {
            resolve('Unable to fetch metadata for asset');
        };

        const apiquery = `/api_asset_info?policy_id=${assetpolicy}&asset_name=${assetname}`;
        xhr.open('GET', apiquery, true);
        xhr.setRequestHeader('accept', 'application/json');
        xhr.send();
    });
}

async function loadNFTInfoKoios(elem_prefix, assetinfo, theme, nftIndex) {
    const assetpolicy = assetinfo.substring(0, 56);
    const assetname = assetinfo.substring(56);
    const assetNameAscii = hex_to_ascii(assetname);

    if(elem_prefix.indexOf('pool_') == 0) {
        LOADED_NFT_LIST = LOADED_POOL_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else {
        LOADED_NFT_LIST = LOADED_WALLET_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    const nft_html_elem = document.getElementById(elem_prefix + assetinfo);
    
    try {
        const response = await getAssetMetadata(assetpolicy, assetname);
        
        if (response !== 'Unable to fetch metadata for asset') {
            var assetJson = JSON.parse(response);
            LOADED_NFT_LIST[nftIndex].extended = assetJson[0];

            if (assetJson[0].minting_tx_metadata) {
                const koios_policy = assetJson[0].minting_tx_metadata['721'][assetpolicy];
                const asset = koios_policy[assetNameAscii];

                const image = asset['image'];
                const ipfsID = image.substring(image.indexOf('Qm'));
                const imageURL = `https://ipfs.blockfrost.dev/ipfs/${ipfsID}`;

                if(!VIEW_OPTIONS.disableSelection) {
                    nft_html_elem.innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" onclick="togglePoolNFTSelection(${elem_prefix + assetinfo}, '${theme}');" class="show-hover-pointer padding" src='${imageURL}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>`;
                }
                else {
                    nft_html_elem.innerHTML = `<img id="${elem_prefix + assetinfo}_img" loading="lazy" height="200" class="padding" src='${imageURL}'><div id="${elem_prefix + assetinfo}_namediv" class="nft-name-display not-selected${theme} align-bottom">${asset['name']}</div>`;
                }
            }
            else {
                console.log("koios no metadata for nft");
            }
        }
        else {
            nft_html_elem.innerHTML = response;
        }
    } catch (error) {
        console.error('Error loading NFT info:', error);
        nft_html_elem.innerHTML = 'Error loading NFT info';
    }
}

function nftSatisfiesRules(nft, poolRules) {
    
    const nftNameAscii = hex_to_ascii(nft.asset_name)

    if(!nftNameAscii.startsWith(poolRules.nftNamePrefix)) { return false; } // nft name doesn't start with the desired prefix

    const nftNumberString = nftNameAscii.substring(poolRules.digitIndexStart, poolRules.digitIndexStart + poolRules.digitLength);

    const nftNumber = parseInt(nftNumberString);
    if(nftNumber < poolRules.digitRangeFirst || nftNumber > poolRules.digitRangeLast) { return false; } // nft number is not within the desired range

    // when this point is reached, all rules have been satisfied
    return true;
}

async function applyViewOptionsOnNFTList(nftListHTMLElement, htmlprefix, theme, pool_policy_id, pool_nft_names, poolRules, pOffset, pLimit) {
    
    const searchPropertyNames = ['name', 'description', 'rarity'];

    var html = '';
    if (htmlprefix == '')
        htmlprefix = 'wallet';

    if(htmlprefix == 'pool') {
        LOADED_NFT_LIST = LOADED_POOL_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else {
        LOADED_NFT_LIST = LOADED_WALLET_NFTS;
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    var doFiltering = false;
    if (pool_policy_id != undefined && pool_policy_id != '') {
        doFiltering = true;
    }

    if(typeof poolRules === 'string') {
        // poolRules is of type string. Decode and parse as object
        poolRules = decodeURIComponent(poolRules)
        poolRules = JSON.parse(poolRules);
    }

    // apply correct list layout
    if(VIEW_OPTIONS.viewAs != listTypes[0]) {
        // display nfts as cards
        nftListHTMLElement.classList.add('d-flex');
        nftListHTMLElement.classList.add('mb-3');
        nftListHTMLElement.classList.add('flex-row');
        nftListHTMLElement.classList.add('flex-wrap');
    }
    else {
        // display nfts as text list
        nftListHTMLElement.classList.remove('d-flex');
        nftListHTMLElement.classList.remove('mb-3');
        nftListHTMLElement.classList.remove('flex-row');
        nftListHTMLElement.classList.remove('flex-wrap');
    }

    var numberOfResults = 0;
    var nftsOnThePage = 0;

    if(LOADED_NFT_LIST.length == 0) return;

    for (var i = 0; i < LOADED_NFT_LIST.length; i++) {
        // filter nft out if it doesn't belong to the pool policy id
        if (doFiltering && LOADED_NFT_LIST[i].policy_id != pool_policy_id) { continue; }
        
        // FOR FILTERED SWAP POOLS: filter nft out if the name is not in the list of desired nft names
        if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(LOADED_NFT_LIST[i].asset_name) == -1) { continue; }

        // FOR RULE BASED SWAP POOLS: filter nft out if the name doesn't satisfy the rules of desired nft names
        if (doFiltering && Object.hasOwn(poolRules, 'nftNamePrefix')) { if(!nftSatisfiesRules(LOADED_NFT_LIST[i], poolRules)) { console.log('not satisfying rules'); continue; } }

        if(LOADED_NFT_LIST[i].extended == null) {
            try {
                const response = await getAssetMetadata(LOADED_NFT_LIST[i].policy_id, LOADED_NFT_LIST[i].asset_name);
                if (response !== 'Unable to fetch metadata for asset') {
                    var assetJson = JSON.parse(response);
                    LOADED_NFT_LIST[i].extended = assetJson[0];
                }
            } catch (error) {
                console.error('Error fetching metadata:', error);
            }
        }
        
        // examine minting metadata
        if (LOADED_NFT_LIST[i].extended.minting_tx_metadata) {
            const meta_policy = LOADED_NFT_LIST[i].extended.minting_tx_metadata['721'][pool_policy_id];
            const meta_asset = meta_policy[LOADED_NFT_LIST[i].extended.asset_name_ascii];

            // check if the user has selected to view a particular trait
            if(VIEW_OPTIONS.viewTrait.length > 0) {
                const traitKeys = Object.keys(meta_asset.traits);
                var traitFound = false;
                var wantedTrait = '';
                for(var ti = 0; ti < VIEW_OPTIONS.viewTrait.length; ti++) {
                    wantedTrait = VIEW_OPTIONS.viewTrait[ti].toLowerCase();
                    for(var ki = 0; ki < traitKeys.length; ki++) {
                        if(meta_asset.traits[traitKeys[ki]].toLowerCase() == wantedTrait) {
                            traitFound = true;
                            break;
                        }
                    }
                }
                if(!traitFound) {
                    // wanted trait not found. Filter out this asset
                    continue;
                }
            }

            // check if the user has selected to view a particular rarity
            if(VIEW_OPTIONS.viewRarity.length > 0) {
                var rarityFound = false;
                var wantedRarity = '';
                for (var si = 0; si < VIEW_OPTIONS.viewRarity.length; si++) {
                    wantedRarity = VIEW_OPTIONS.viewRarity[si].toLowerCase();
                    for (var ri = 0; ri < VIEW_OPTIONS.viewRarity.length; ri++) {
                        if (meta_asset.rarity.toLowerCase() == wantedRarity) {
                            rarityFound = true;
                            break;
                        }
                    }
                }
                if(!rarityFound) {
                    // wanted rarity not found. Filter out this asset
                    continue;
                }
            }

            var value;
            
            if(VIEW_OPTIONS.searchCriterion != '') {
                // a search criterion has been provided. Filter according to search criterion
                console.log('search criterion', VIEW_OPTIONS.searchCriterion);
                console.log('meta asset', meta_asset);
                criteriaFound = false;
                for(var p = 0; p < searchPropertyNames.length; p++) {
                    if(Object.hasOwn(meta_asset, searchPropertyNames[p])) {
                        value = meta_asset[searchPropertyNames[p]]
                        console.log(searchPropertyNames[p] + ' = '+ value.toLowerCase());
                        if(value.toLowerCase().indexOf(VIEW_OPTIONS.searchCriterion) > -1) {
                            criteriaFound = true;
                        }
                    }
                } 

                const traitKeys = Object.keys(meta_asset.traits);
                var traitFound = false;
                
                for(var ki = 0; ki < traitKeys.length; ki++) {
                    if(meta_asset.traits[traitKeys[ki]].toLowerCase().indexOf(VIEW_OPTIONS.searchCriterion) > -1) {
                        traitFound = true;
                    }
                }
                
                if(!criteriaFound && !traitFound) { continue; }
            }

        }

        numberOfResults++;
        if(numberOfResults <= pOffset) { continue; } // result item is before the start of range. Continue to next result
        else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop
        nftsOnThePage++;
        const textListClasses = "d-flex mb-3 flex-row justify-content-between"
        var classes = '';
        if(VIEW_OPTIONS.viewAs == listTypes[0]) {
            classes = textListClasses;
        }
        
        html += `<div id='${htmlprefix}_nft_list_${LOADED_NFT_LIST[i].policy_id}${LOADED_NFT_LIST[i].asset_name}' class='not-selected${theme} padding ${classes}'></div>`
    }
    

    nftListHTMLElement.innerHTML = html;

    numberOfResults = 0;

    
    for (var i = 0; i < LOADED_NFT_LIST.length; i++) {

        // filter nft out if it doesn't belong to the pool policy id
        if (doFiltering && LOADED_NFT_LIST[i].policy_id != pool_policy_id) { continue; }
        
        // FOR FILTERED SWAP POOLS: filter nft out if the name is not in the list of desired nft names
        if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(LOADED_NFT_LIST[i].asset_name) == -1) { continue; }

        // FOR RULE BASED SWAP POOLS: filter nft out if the name doesn't satisfy the rules of desired nft names
        if (doFiltering && Object.hasOwn(poolRules, 'nftNamePrefix')) { if(!nftSatisfiesRules(LOADED_NFT_LIST[i], poolRules)) { console.log('not satisfying rules'); continue; } }

        // if(LOADED_NFT_LIST[i].extended == null) {
        //     console.log('no extended info found', i);
        //     continue;
        // }
        // else if(LOADED_NFT_LIST[i].extended.minting_tx_metadata == null) {
        //     console.log('no metadata found', i);
        //     continue;
        // }

        // fetch minting metadata
        if (LOADED_NFT_LIST[i].extended.minting_tx_metadata) {
            const meta_policy = LOADED_NFT_LIST[i].extended.minting_tx_metadata['721'][pool_policy_id];
            
            const meta_asset = meta_policy[LOADED_NFT_LIST[i].extended.asset_name_ascii];

            // check if the user has selected to view a particular trait
            if(VIEW_OPTIONS.viewTrait.length > 0) {
                const traitKeys = Object.keys(meta_asset.traits);
                var traitFound = false;
                var wantedTrait = '';
                for(var ti = 0; ti < VIEW_OPTIONS.viewTrait.length; ti++) {
                    wantedTrait = VIEW_OPTIONS.viewTrait[ti].toLowerCase();
                    for(var ki = 0; ki < traitKeys.length; ki++) {
                        if(meta_asset.traits[traitKeys[ki]].toLowerCase() == wantedTrait) {
                            traitFound = true;
                            break;
                        }
                    }
                }
                if(!traitFound) {
                    // wanted trait not found. Filter out this asset
                    continue;
                }
            }

            // check if the user has selected to view a particular rarity
            if(VIEW_OPTIONS.viewRarity.length > 0) {
                var rarityFound = false;
                var wantedRarity = '';
                for (var si = 0; si < VIEW_OPTIONS.viewRarity.length; si++) {
                    wantedRarity = VIEW_OPTIONS.viewRarity[si].toLowerCase();
                    for (var ri = 0; ri < VIEW_OPTIONS.viewRarity.length; ri++) {
                        if (meta_asset.rarity.toLowerCase() == wantedRarity) {
                            rarityFound = true;
                            break;
                        }
                    }
                }
                if(!rarityFound) {
                    // wanted rarity not found. Filter out this asset
                    continue;
                }
            }

            var value;
            
            if(VIEW_OPTIONS.searchCriterion != '') {
                // a search criterion has been provided. Filter according to search criterion
                console.log('search criterion', VIEW_OPTIONS.searchCriterion);
                console.log('meta asset', meta_asset);
                criteriaFound = false;
                for(var p = 0; p < searchPropertyNames.length; p++) {
                    if(Object.hasOwn(meta_asset, searchPropertyNames[p])) {
                        value = meta_asset[searchPropertyNames[p]]
                        console.log(searchPropertyNames[p] + ' = '+ value.toLowerCase());
                        if(value.toLowerCase().indexOf(VIEW_OPTIONS.searchCriterion) > -1) {
                            criteriaFound = true;
                        }
                    }
                } 

                const traitKeys = Object.keys(meta_asset.traits);
                var traitFound = false;
                
                for(var ki = 0; ki < traitKeys.length; ki++) {
                    if(meta_asset.traits[traitKeys[ki]].toLowerCase().indexOf(VIEW_OPTIONS.searchCriterion) > -1) {
                        traitFound = true;
                    }
                }
                
                if(!criteriaFound && !traitFound) { continue; }
            }

        }

        numberOfResults++;
        if(numberOfResults <= pOffset) { continue; } // result item is before the start of range. Continue to next result
        else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop

        //loadNFTInfoKoios(`${htmlprefix}_nft_list_`, `${LOADED_NFT_LIST[i].policy_id}${LOADED_NFT_LIST[i].asset_name}`, theme);
        addExtendedInfo(`${htmlprefix}_nft_list_`, `${LOADED_NFT_LIST[i].policy_id}${LOADED_NFT_LIST[i].asset_name}`, theme, i)
    }
    

    if(htmlprefix == 'wallet') {
        updateWalletSelectionLabel();
    }

    if(nftsOnThePage < pLimit) {
        // we are now displaying fewer results than the pLimit. There are no more results to display. Disable paging
        disableHigherNavPages(htmlprefix);
    }
    else {
        enableHigherNavPages(htmlprefix);
    }

    if(!VIEW_OPTIONS.disableSelection) {
        toggleSelectedNFTs(`${theme}`)
    }
}

function listNFTs(nftList, nftListHTMLElement, htmlprefix, theme, pool_policy_id, pool_nft_names, poolRules, pOffset, pLimit) {

    var html = '';
    if (htmlprefix == '')
        htmlprefix = 'wallet';

    if(htmlprefix == 'pool') {
        LOADED_POOL_NFTS = nftList;
        VIEW_OPTIONS = VIEW_OPTIONS_POOL;
    }
    else {
        LOADED_WALLET_NFTS = nftList;
        VIEW_OPTIONS = VIEW_OPTIONS_WALLET;
    }

    var doFiltering = false;
    if (pool_policy_id != undefined && pool_policy_id != '') {
        doFiltering = true;
    }

    if(typeof poolRules === 'string') {
        // poolRules is of type string. Decode and parse as object
        poolRules = decodeURIComponent(poolRules)
        poolRules = JSON.parse(poolRules);
    }

    var numberOfResults = 0;
    var nftsOnThePage = 0;

    if (nftList) {
        for (var i = 0; i < nftList.length; i++) {
            // filter nft out if it doesn't belong to the pool policy id
            if (doFiltering && nftList[i].policy_id != pool_policy_id) { continue; }
            
            // FOR FILTERED SWAP POOLS: filter nft out if the name is not in the list of desired nft names
            if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(nftList[i].asset_name) == -1) { continue; }

            // FOR RULE BASED SWAP POOLS: filter nft out if the name doesn't satisfy the rules of desired nft names
            if (doFiltering && Object.hasOwn(poolRules, 'nftNamePrefix')) { if(!nftSatisfiesRules(nftList[i], poolRules)) { console.log('not satisfying rules'); continue; } }

            // check if the user has selected to view a particular trait
            if(VIEW_OPTIONS.viewTrait != '') {
                console.log('filter trait '+ VIEW_OPTIONS.viewTrait);
                console.log(nftList[i]);
            }

            // check if the user has selected to view a particular rarity
            if(VIEW_OPTIONS.viewRarity != '') {
                console.log('filter rarity '+ VIEW_OPTIONS.viewRarity);
            }

            numberOfResults++;
            if(numberOfResults <= pOffset) { continue; } // result item is before the start of range. Continue to next result
            else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop
            nftsOnThePage++;
            html += `<div id='${htmlprefix}_nft_list_${nftList[i].policy_id}${nftList[i].asset_name}' class='not-selected${theme} padding'></div>`
        }
    }

    nftListHTMLElement.innerHTML = html;

    numberOfResults = 0;

    if (nftList) {
        for (var i = 0; i < nftList.length; i++) {

            // filter nft out if it doesn't belong to the pool policy id
            if (doFiltering && nftList[i].policy_id != pool_policy_id) { continue; }
            
            // FOR FILTERED SWAP POOLS: filter nft out if the name is not in the list of desired nft names
            if (doFiltering && pool_nft_names != '' && pool_nft_names.indexOf(nftList[i].asset_name) == -1) { continue; }

            // FOR RULE BASED SWAP POOLS: filter nft out if the name doesn't satisfy the rules of desired nft names
            if (doFiltering && Object.hasOwn(poolRules, 'nftNamePrefix')) { if(!nftSatisfiesRules(nftList[i], poolRules)) { console.log('not satisfying rules'); continue; } }

            numberOfResults++;
            if(numberOfResults <= pOffset) { continue; } // result item is before the start of range. Continue to next result
            else if(numberOfResults > (pOffset + pLimit)) { break; } // end of range has been reached. Break for-loop

            loadNFTInfoKoios(`${htmlprefix}_nft_list_`, `${nftList[i].policy_id}${nftList[i].asset_name}`, theme, i);

        }
    }

    if(htmlprefix == 'wallet') {
        updateWalletSelectionLabel();
    }

    if(nftsOnThePage < pLimit) {
        // we are now displaying fewer results than the pLimit. There are no more results to display. Disable paging
        disableHigherNavPages(htmlprefix);
    }
    else {
        enableHigherNavPages(htmlprefix);
    }

    if(!VIEW_OPTIONS.disableSelection) {
        toggleSelectedNFTs(`${theme}`)
    }
}


function displayMBoxScriptFields() {
    hideElem('message-box-content');
    showElem('message-box-pool-script-row');
    hideElem('message-box-filter-row');
    hideElem('message-box-rules-row');
    showElem('message-box-pool-addr-row');
    showElem('message-box-plutus-button');
    hideElem('message-box-queue-script-row');
    hideElem('message-box-queue-addr-row');
}

function displayMBoxRandomScriptFields() {
    displayMBoxScriptFields();
    showElem('message-box-queue-script-row');
    showElem('message-box-queue-addr-row');
}

function displayMBoxFilteredScriptFields() {
    displayMBoxScriptFields()
    showElem('message-box-filter-row');
}

function displayMBoxRulesScriptFields() {
    displayMBoxScriptFields()
    showElem('message-box-rules-row');
}

function displayMBoxMessageFieldOnly() {
    showElem('message-box-content');
    hideElem('message-box-pool-script-row');
    hideElem('message-box-filter-row');
    hideElem('message-box-pool-addr-row');
    hideElem('message-box-rules-row');
    hideElem('message-box-plutus-button');
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

function loadAddNFTDropdown(dropdown, theme, swapPoolNames, poolPolicyIds, poolNFTNames, poolRules, nftPerPage) {

    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex" data-bs-toggle="modal" data-bs-target="#selectNFTsDialog"><a class="dropdown-item ${theme}" href="#" onclick="setActivePage(1, 'wallet'); showElem('addAssetNavigation'); hideElem('removeAssetNavigation'); const confButton = document.getElementById('confirmAddNFTsButton'); resetOnClick(confButton); confButton.setAttribute('onclick', 'addNFTsToPool(${i})'+ confButton.getAttribute('onclick')); setInnerText('selectNFTsDialogLabel', 'Select NFTs to add to swap pool'); showElem('confirmAddNFTsButton'); hideElem('confirmRemoveNFTsButton'); getRewardAddresses().then((addr) => { getAddressAssets(addr).then((assets) => { currentPolicyId = '${poolPolicyIds[i]}'; currentNFTNames = '${poolNFTNames[i]}'; currentRules = '${encodeURIComponent(JSON.stringify(poolRules[i]))}'; VIEW_OPTIONS_WALLET.disableSelection = false; listNFTs(assets, document.getElementById('selectable_nfts'), 'wallet', '${theme}', currentPolicyId, currentNFTNames, currentRules, 0, ${nftPerPage}) } ) } ).catch((reason => console.log('error: '+ reason.message)));">${swapPoolNames[i].trim()}</a></div></li>`;
        } 
    }
    
    dropdown.innerHTML = swapPoolListHtml;

}

function loadRemoveNFTDropdown(dropdown, theme, swapPoolNames, nftPerPage) {

    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex" data-bs-toggle="modal" data-bs-target="#selectNFTsDialog"><a class="dropdown-item ${theme}" href="#" onclick="setActivePage(1, 'pool'); showElem('removeAssetNavigation'); hideElem('addAssetNavigation'); const confButton = document.getElementById('confirmRemoveNFTsButton'); resetOnClick(confButton); confButton.setAttribute('onclick', 'removeNFTsFromPool(${i})'+ confButton.getAttribute('onclick')); setInnerText('selectNFTsDialogLabel', 'Select NFTs to remove from swap pool'); showElem('confirmRemoveNFTsButton'); hideElem('confirmAddNFTsButton'); currentSwapPoolIndex = ${i}; getSwapPoolAddress(${i}).then((addr) => { getAddressAssets(addr).then((assets) => { currentPolicyId = null; currentNFTNames = null; currentRules = null; VIEW_OPTIONS_POOL.disableSelection = false; listNFTs(assets, document.getElementById('selectable_nfts'), 'pool', '${theme}', currentPolicyId, currentNFTNames, currentRules, 0, ${nftPerPage}) } ) } ).catch((reason => console.log('error: '+ reason.message)));">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;

}

function loadWithdrawalDropdown(dropdown, theme, swapPoolNames) {
    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="#" onclick="getSwapPoolUTxO(${i}).then(utxo => {withdrawFromPool(utxo[utxo.length - 1].txHash,utxo[utxo.length - 1].outputIndex, ${i})});">${swapPoolNames[i].trim()}</a></div></li>`;
        }
    }
    
    dropdown.innerHTML = swapPoolListHtml;
}

function errorReturned(message) {
    var foundError = false;

    if(message == null || message == undefined) { return false; }
    if(typeof message === 'object') { message = JSON.stringify(message); }

    message = message.toLowerCase();

    if( message.indexOf('redeemer') > -1 || 
        message.indexOf('error') > -1 || 
        message.indexOf('user declined') > -1 ||
        message.indexOf('rejected') > -1 || 
        message.indexOf('failed') > -1 ||
        message.indexOf('inputsexhaustederror') > -1) {
            foundError = true;
    };
    return foundError;
} 

function loadDepositDropdown(dropdown, theme, swapPoolNames) {
    var swapPoolListHtml = '';

    for(var i = 0; i < swapPoolNames.length; i++) {

        if(swapPoolNames[i].trim() != '') {
            swapPoolListHtml += `<li><div class="dropdown-item ${theme} d-flex"><a class="dropdown-item ${theme}" href="#" onclick="depositLovelace(3000000, ${i}).then(message => { const mBoxTitle = document.getElementById('messageBoxLabel'); if(errorReturned(message)) { mBoxTitle.innerText='Something went wrong'; } else { mBoxTitle.innerText='Deposit successful'; } document.getElementById('message-box-content').innerHTML=removeTechnicalGibberish(message); const messageBox = new bootstrap.Modal('#messageBox', { keyboard: false }); messageBox.show();});">${swapPoolNames[i].trim()}</a></div></li>`;
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

function setCookie(cookiename, value) {
    const expirationDate = new Date();
    expirationDate.setMonth(expirationDate.getMonth()+1);
    
    document.cookie = `${cookiename}=${value}; expires=${expirationDate};`;
}

function setActiveTheme(theme) {
    const dropDownButton = document.getElementById('themeSelectionDropdown');

    var themeImage = '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" class="bi bi-brightness-high" viewBox="0 0 16 16"><path d="M8 11a3 3 0 1 1 0-6 3 3 0 0 1 0 6zm0 1a4 4 0 1 0 0-8 4 4 0 0 0 0 8zM8 0a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-1 0v-2A.5.5 0 0 1 8 0zm0 13a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-1 0v-2A.5.5 0 0 1 8 13zm8-5a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1 0-1h2a.5.5 0 0 1 .5.5zM3 8a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1 0-1h2A.5.5 0 0 1 3 8zm10.657-5.657a.5.5 0 0 1 0 .707l-1.414 1.415a.5.5 0 1 1-.707-.708l1.414-1.414a.5.5 0 0 1 .707 0zm-9.193 9.193a.5.5 0 0 1 0 .707L3.05 13.657a.5.5 0 0 1-.707-.707l1.414-1.414a.5.5 0 0 1 .707 0zm9.193 2.121a.5.5 0 0 1-.707 0l-1.414-1.414a.5.5 0 0 1 .707-.707l1.414 1.414a.5.5 0 0 1 0 .707zM4.464 4.465a.5.5 0 0 1-.707 0L2.343 3.05a.5.5 0 1 1 .707-.707l1.414 1.414a.5.5 0 0 1 0 .708z"/></svg>'

    if(theme == 'dark-mode') {
        themeImage = '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" fill="currentColor" class="bi bi-moon" viewBox="0 0 16 16"><path d="M6 .278a.768.768 0 0 1 .08.858 7.208 7.208 0 0 0-.878 3.46c0 4.021 3.278 7.277 7.318 7.277.527 0 1.04-.055 1.533-.16a.787.787 0 0 1 .81.316.733.733 0 0 1-.031.893A8.349 8.349 0 0 1 8.344 16C3.734 16 0 12.286 0 7.71 0 4.266 2.114 1.312 5.124.06A.752.752 0 0 1 6 .278zM4.858 1.311A7.269 7.269 0 0 0 1.025 7.71c0 4.02 3.279 7.276 7.319 7.276a7.316 7.316 0 0 0 5.205-2.162c-.337.042-.68.063-1.029.063-4.61 0-8.343-3.714-8.343-8.29 0-1.167.242-2.278.681-3.286z"/></svg>'
    }

    dropDownButton.innerHTML = themeImage;
}

async function fulfillWithTimeLimit(timeLimit, task, failureValue){
    let timeout;
    const timeoutPromise = new Promise((resolve, reject) => {
        timeout = setTimeout(() => {
            resolve(failureValue);
        }, timeLimit);
    });
    const response = await Promise.race([task, timeoutPromise]);
    if(timeout){ //the code works without this but let's be safe and clean up the timeout
        clearTimeout(timeout);
    }
    return response;
}

async function loadWalletConnector(dropdown, button, theme) {

    const supportedWallets = ['lace', 'nami', 'eternl']

    connectorDropdown = dropdown;
    connectorButton = button;

    const cardanowallets = window.cardano;

    var walletListHtml = '';
    
    connectedWalletExtName = getCookie('connectedwallet');

    console.log('connecting to '+ connectedWalletExtName);

    for(var i = 0; i < Object.keys(cardanowallets).length; i++) {
        
        currentWalletName = Object.keys(cardanowallets)[i];
        if(!supportedWallets.includes(currentWalletName)) { continue; }
        wallet = cardanowallets[currentWalletName];
        if (connectedWalletExtName == currentWalletName) {
            
            const api = await wallet.enable();
            
            if (await wallet.isEnabled()) {
                try {

                    var lucid;
                    var retries = 0;
                    while (lucid == null && retries < 3) {
                        try {
                            // some wallets time out...retry before giving up
                            lucid = await connectToLucid();
                        }
                        catch (e) {
                            console.log(`Connection to ${wallet.name} failed. Retrying...`);
                            retries++;
                            await delay(1500);
                        }
                    }
                    if (lucid == null) {
                        // unable to connect to wallet. Display warning icon
                        console.log(`unable to initialize ${wallet.name}`);
                        walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-exclamation-circle" viewBox="0 0 16 16">
                            <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
                            <path d="M7.002 11a1 1 0 1 1 2 0 1 1 0 0 1-2 0zM7.1 4.995a.905.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 4.995z"/>
                            </svg>&nbsp;<a class="dropdown-item ${theme}" href="#">${wallet.name}</a></div></li>`
                        continue;
                    }

                    const utxos = await fulfillWithTimeLimit(1000, lucid.wallet.getUtxos(), null);
                    if (utxos == null) {
                        // unable to connect to wallet. Display warning icon
                        console.log(`unable to get utxos of ${wallet.name}`);
                        walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-exclamation-circle" viewBox="0 0 16 16">
                            <path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"/>
                            <path d="M7.002 11a1 1 0 1 1 2 0 1 1 0 0 1-2 0zM7.1 4.995a.905.905 0 1 1 1.8 0l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 4.995z"/>
                            </svg>&nbsp;<a class="dropdown-item ${theme}" href="#">Please open ${wallet.name} wallet manually and refresh page</a></div></li>`
                        continue;
                    }

                    const lovelace = utxos.reduce((acc, utxo) => acc + utxo.assets.lovelace, 0n);
                    const adaBalance = lovelace / 1000000n;
                    walletListHtml += `<li><div class="dropdown-item ${theme} d-flex"><img src="${wallet.icon}" width="30" height="30"/><a class="dropdown-item ${theme}" href="#">${adaBalance} ADA</a><svg xmlns="http://www.w3.org/2000/svg" width="30" height="30" fill="currentColor" class="bi bi-check" viewBox="0 0 16 16"><path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/></svg></div></li>`
                    button.innerHTML = `<img src="${wallet.icon}" width="20" height="20"/>&nbsp;<a class="connect-button${theme}" href="#">${adaBalance} ADA</a>`
                    connectedWallet = wallet;
                }
                catch (err) {
                    console.error(err)
                }
            }
            else {
                console.log(`${wallet.name} not enabled`);
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