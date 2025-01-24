async function leverageAssets() {
    const [owner] = await ethers.getSigners();
    const tokenAddress = "YOUR_TOKEN_ADDRESS";
    const amountToLeverage = ethers.utils.parseUnits("9.5", 18); // Amount to leverage

    const token = await ethers.getContractAt("0xb435F81f710a77F7Ad78e17D2756dE3240F643cE", tokenAddress);
    
    // Approve the token for trading
    await token.approve("YOUR_EXCHANGE_ADDRESS", amountToLeverage);
    
    // Execute the trade (swap)
    const tx = await exchange.swapExactTokensForTokens(
        amountToLeverage,
        0, // Min amount out
        [tokenAddress, "TARGET_TOKEN_ADDRESS"],
        owner.address,
        Math.floor(Date.now() / 1000) + 60 * 20 // Deadline
    );
    await tx.wait();
    console.log(`Leveraged ${amountToLeverage.toString()} tokens.`);
}
