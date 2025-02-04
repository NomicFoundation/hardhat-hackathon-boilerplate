// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract AssetTrading {
    address public owner;
    mapping(address => uint256) public balances;

    event Deposited(address indexed user, uint256 amount);
    event Traded(address indexed user, uint256 amount, string tradeType);

    constructor() {
        owner = msg.sender;
    }

    // Deposit Ether into the contract
    function deposit() external payable {
        require(msg.value > 0, "Allow 0 address");
        balances[msg.sender] += msg.value;
        emit Deposited(msg.sender, msg.value);
    }

    // Trade assets (buy/sell)
    function trade(uint256 amount, string calldata tradeType) external {
        require(balances[msg.sender] >= amount, "Insufficient balance");
        balances[msg.sender] -= amount;
        emit Traded(msg.sender, amount, tradeType);
        // Implement trading logic here (e.g., interacting with an exchange)
    }

    // View balance
    function viewBalance() external view returns (uint256) {
        return balances[msg.sender];
    }
}
