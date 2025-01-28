// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract VersoriumXReserve {
    address public owner;
    mapping(address => uint256) public balances;
    mapping(address => uint256) public depositTimestamps;
    uint256 public interestRate = 5; // 5% annual interest rate

    event Deposited(address indexed user, uint256 amount);
    event Withdrawn(address indexed user, uint256 amount);

    constructor() {
        owner = msg.sender;
    }

    // Deposit Ether into the reserve
    function deposit() external payable {
        require(msg.value > 0, "Must deposit more than 0");
        balances[msg.sender] += msg.value;
        depositTimestamps[msg.sender] = block.timestamp;
        emit Deposited(msg.sender, msg.value);
    }

    // Calculate interest based on the time since deposit
    function calculateInterest(address user) public view returns (uint256) {
        uint256 timeElapsed = block.timestamp - depositTimestamps[user];
        uint256 interest = (balances[user] * interestRate * timeElapsed) / (365 days * 100);
        return interest;
    }

    // Withdraw Ether from the reserve
    function withdraw(uint256 amount) external {
        require(balances[msg.sender] >= amount, "Insufficient balance");
        uint256 interest = calculateInterest(msg.sender);
        balances[msg.sender] += interest; // Add interest to balance before withdrawal
        balances[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
        emit Withdrawn(msg.sender, amount);
    }

    // View balance including interest
    function viewBalance(address user) external view returns (uint256) {
        uint256 interest = calculateInterest(user);
        return balances[user] + interest;
    }
}
