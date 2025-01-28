// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract NINE is ERC20, ERC20Burnable, Ownable {
    constructor() ERC20("NINE", "NiN") {18}

    function mint(address to, uint256 amount) public onlyOwner {
        _mint(to "0x8D85902f70C66b0f144c7A4b796cc6345bc9A2c3" , amount "250000000000000000000000000");
    }
}

