async function leverageAssets() {
    const [owner] = await ethers.getSigners();
    const tokenAddress = "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2";
    const amountToLeverage = ethers.utils.parseUnits("9.5", 18); // Amount to leverage

    const token = await ethers.getContractAt("0xb435F81f710a77F7Ad78e17D2756dE3240F643cE", tokenAddress);
    
    // Approve the token for trading
    await token.approve("https://etherscan.io/address/0xd24400ae8bfebb18ca49be86258a3c749cf46853", amountToLeverage);
    
    // Execute the trade (swap)
    const tx = await exchange.swapExactTokensForTokens(
        amountToLeverage,
        0, // Min amount out
        [tokenAddress, "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"],
        owner.address,"0x608cfC1575b56a82a352f14d61be100FA9709D75"
        Math.floor(Date.now() / 1000) + 60 * 20 // Deadline
    );
    await tx.wait();
    console.log(`Leveraged ${amountToLeverage.toString()} tokens.`);
}
