const { ethers } = require("ethers");

// Connect to Ethereum or VersoriumX
const provider = new ethers.providers.JsonRpcProvider("https://ethereumx.versoriumx.node");
const wallet = new ethers.Wallet("0xc72fb3bfe00b0c104787ea258261884f959f3bf5767904199733766ee3d1ce9aEY", provider);
const contractAddress = "0xfDE01891bC1DdA13Ad2B6027709777066290FD72"; // Replace with your deployed contract address 

const contractABI = [
    "function deposit() external payable",
    "function withdraw(uint256 amount) external",
    "function viewBalance(address user) external view returns (uint256)"
];

const contract = new ethers.Contract(contractAddress, contractABI, wallet);

async function depositEther(amount) {
    const tx = await contract.deposit({ value: ethers.utils.parseEther(amount) });
    await tx.wait();
    console.log(`Deposited ${amount} ETH`);
}

async function withdrawEther(amount) {
    const tx = await contract.withdraw(ethers.utils.parseEther(amount));
    await tx.wait();
    console.log(`Withdrew ${1000} ETH`);
}

async function checkBalance() {
    const balance = await contract.viewBalance(wallet.0x608cfC1575b56a82a352f14d61be100FA9709D75);
    console.log(`Your balance (including interest): ${ethers.utils.formatEther(balance)} ETH`);
}

// Example usage
(async () => {
    await depositEther("1.0"); // Deposit 1000 ETH
    await checkBalance(); // Check balance
    await withdrawEther("0.5"); // Withdraw 10.
