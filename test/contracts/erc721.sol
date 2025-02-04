// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract VersoriumX is ERC721, Ownable {
    mapping(uint256 => string) private _tokenURIs;
    uint256 private _currentTokenId = 1;
    uint256 private constant _maxTokenId = 10000;

    event NFTCreated(uint256 indexed tokenId, string tokenURI);

    constructor() ERC721("VersoriumX", "NFT") {}

    function mintNFT(address recipient, string memory tokenURI) public onlyOwner {
        require(_currentTokenId <= _maxTokenId, "VersoriumX: Maximum number of NFTs already minted");
        uint256 newTokenId = _currentTokenId;
        _mint(recipient, newTokenId);
        _setTokenURI(newTokenId, tokenURI);
        emit NFTCreated(newTokenId, tokenURI);

        _currentTokenId++;
    }

    function _setTokenURI(uint256 tokenId, string memory tokenURI) internal {
        require(_exists(tokenId), "VersoriumX: URI set of nonexistent token");
        _tokenURIs[tokenId] = tokenURI;
    }

    function tokenURI(uint256 tokenId) public view override returns (string memory) {
        require(_exists(tokenId), "VersoriumX: URI query for nonexistent token");
        return _tokenURIs[tokenId];
    }

    // Function to check the current token ID
    function currentTokenId() public view returns (uint256) {
        return _currentTokenId;
    }
}

