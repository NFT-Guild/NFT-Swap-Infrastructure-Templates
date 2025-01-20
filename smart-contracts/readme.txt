THE SWAP POOL SMART CONTRACTS REQUIRE NO ADDITIONAL BUILDING TO USE!
WE SIMPLY PROVIDE THE SOURCE CODE SO YOU CAN VERIFY THE FUNCTIONALITY


If you want to make changes for some reason, the smart contracts can be changed in your plutus development environment by a developer.
After the changed contracts have been built, you can replace the cbor in the relevant constant(s):
- uninitSwapPlutusCbor
- uninitFilteredSwapPlutusCbor 
- uninitRuleSwapPlutusCbor
- uninitRandomPlutusCbor
- uninitRandomQueueCbor

The constants are defined in the file wallet-integration.js