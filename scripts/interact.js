
const hre = require("hardhat");

async function main() {
  const contractAddress = "0xYourContractAddress";
  const MyContract = await hre.ethers.getContractFactory("MyContract");
  const myContract = await MyContract.attach(contractAddress);

  // Interact with the contract here
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});



