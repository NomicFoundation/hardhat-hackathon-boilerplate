async function leverageAssets() {
    const [owner] = await ethers.getSigners();
    const tokenAddress = "YOUR_TOKEN_ADDRESS";
    const amountToLeverage = ethers.utils.parseUnits("0.5", 18); // Amount to leverage

    const token = await ethers.getContractAt("ERC20", tokenAddress);
    
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
