const Web3 = require('web3');
const { ethers } = require('ethers');

async function main() {
    // Using web3.js
    const web3 = new Web3('http://127.0.0.1:8545');
    const accounts = await web3.eth.getAccounts();
    console.log('Accounts using web3.js:', accounts);

    // Using ethers.js
    const provider = new ethers.providers.JsonRpcProvider('http://127.0.0.1:8545');
    const ethersAccounts = await provider.listAccounts();
    console.log('Accounts using ethers.js:', ethersAccounts);
}

main().catch((error) => {
    console.error(error);
    process.exit(1);
});
