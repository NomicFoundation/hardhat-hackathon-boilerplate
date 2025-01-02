require("@nomicfoundation/hardhat-toolbox");
require("@nomicfoundation/hardhat-ethers");
require("@nomicfoundation/hardhat-foundry");
require("@nomiclabs/hardhat-waffle"); // Optional, if you need Waffle for testing

/** @type import('hardhat/config').HardhatUser Config */
module.exports = {
    solidity: "0.8.20", // Specify your Solidity version
    networks: {
        hardhat: {
            chainId: 11011, // Set Hardhat chain ID to 11011
        },
        localhost: {
            url: "http://127.0.0.1:8545", // Default localhost URL
            chainId: 11011, // Set localhost chain ID to 11011
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
            chainId: 12345, // Example chain ID for StarkNet (replace with actual if needed)
        },
    },
    paths: {
        sources: "./contracts", // Path to your contracts
        tests: "./tests",       // Path to your tests
        artifacts: "./artifacts" // Path to your artifacts
    },
    // Add any additional configurations here
};
// The next line is part of the sample project, you don't need it in your
// project. It imports a Hardhat task definition, that can be used for
// testing the frontend.
require("./tasks/faucet");

/** @type import('hardhat/config').HardhatUserConfig */
module.exports = {
  solidity: "0.8.20",
};
