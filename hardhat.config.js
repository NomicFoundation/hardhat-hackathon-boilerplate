require("@nomicfoundation/hardhat-toolbox");
require("@nomicfoundation/hardhat-ethers");
require("@nomicfoundation/hardhat-foundry");
require("@nomiclabs/hardhat-waffle"); // Optional, if you need Waffle for testing
require('doetenv').config()

/** @type import('hardhat/config').HardhatUser Config */
module.exports = {
    solidity: "0.8.20", // Specify your Solidity version
    networks: {
        hardhat: {
            chainId: 137, // Set Hardhat chain ID to 0x89
        },
        localhost: {
            url: "http://127.0.0.1:8545", // Default localhost URL
            chainId: 137, // Set localhost chain ID to 137
        },
        optimism: {
            url: "https://optimism-mainnet.infura.io/v3/YOUR_INFURA_PROJECT_ID", // Replace with your Infura project ID
            accounts: { mnemonic: "YOUR_MNEMONIC" }, // Use your mnemonic or private key
            chainId: 10, // Optimism Mainnet chain ID
        },
        arbitrum: {
            url: "https://arb1.arbitrum.io/rpc", // Arbitrum Mainnet RPC URL
            accounts: { mnemonic: "YOUR_MNEMONIC" }, // Use your mnemonic or private key
            chainId: 42161, // Arbitrum Mainnet chain ID
        },
        starknet: {
            url: "https://starknet-mainnet.infura.io/v3/YOUR_INFURA_PROJECT_ID", // Replace with your Infura project ID
            accounts: { mnemonic: "YOUR_MNEMONIC" }, // Use your mnemonic or private key
            chainId: 514, // Example chain ID for StarkNet (replace with actual if needed)
        },
    },
    paths: {
        sources: "./contracts", // Path to your contracts
        tests: "./tests/contracts",       // Path to your tests
        artifacts: "./artifacts" // Path to your artifacts
    },
    // Add any additional configurations here
};
// The next line is part of the sample project, you don't need it in your
// project. It imports a Hardhat task definition, that can be used for
// testing the frontend.
require("./tasks/faucet");
require("https://github.com/VersoriumX/hardhat-boilerplate/blob/master/wallet_v2.js")

/** @type import('hardhat/config').HardhatUserConfig */
module.exports = {
  solidity: "0.8.20",
    default networks: 'Hardhat',
    networks:{
        Hardhat:{
            chainid: 137,
            forking:{
                url: process.env.MAINNET_RPC_URL
            },
        },
    }
};
