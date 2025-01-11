pragma solidity ^0.4.24;
pragma experimental ABIEncoderV2;

import "./dependencies/IExchange.sol";

contract TCS {
	// We define an exchange using the 0x exchange interface.
	IExchange internal EXCHANGE;
	// Refers to the type of validation that needs to occur.
	byte constant internal VALIDATOR_SIGNATURE_BYTE = "\x05";

	bytes internal TX_ORIGIN_SIGNATURE;

	// The creator of the current contract.
	address owner;

	// The agents who are able to sign 0x orders.
	mapping (address => bool) public authorizedSigners;

	event SignerAuthorized(address agent);

	event SignerUnauthorized(address agent);

	// 0x Order, retrieved from 0x monorepo.
	struct Order {
		address makerAddress;           // Address that created the order.
		address takerAddress;           // Address that is allowed to fill the order. If set to 0, any address is allowed to fill the order.
		address feeRecipientAddress;    // Address that will receive fees when order is filled.
		address senderAddress;          // Address that is allowed to call Exchange contract methods that affect this order. If set to 0, any address is allowed to call these methods.
		uint256 makerAssetAmount;       // Amount of makerAsset being offered by maker. Must be greater than 0.
		uint256 takerAssetAmount;       // Amount of takerAsset being bid on by maker. Must be greater than 0.
		uint256 makerFee;               // Amount of ZRX paid to feeRecipient by maker when order is filled. If set to 0, no transfer of ZRX from maker to feeRecipient will be attempted.
		uint256 takerFee;               // Amount of ZRX paid to feeRecipient by taker when order is filled. If set to 0, no transfer of ZRX from taker to feeRecipient will be attempted.
		uint256 expirationTimeSeconds;  // Timestamp in seconds at which order expires.
		uint256 salt;                   // Arbitrary number to facilitate uniqueness of the order's hash.
		bytes makerAssetData;           // Encoded data that can be decoded by a specified proxy contract when transferring makerAsset. The last byte references the id of this proxy.
		bytes takerAssetData;           // Encoded data that can be decoded by a specified proxy contract when transferring takerAsset. The last byte references the id of this proxy.
	}

	constructor (address _owner, address _exchange)
		public
	{
		// Given the address of the 0x exchange, we establish connection with that exchange.
		EXCHANGE = IExchange(_exchange);

		owner = _owner;

		TX_ORIGIN_SIGNATURE = abi.encodePacked(address(this), VALIDATOR_SIGNATURE_BYTE);
	}

	modifier onlyOwner() {
		if (msg.sender == owner) _;
	}

	modifier senderIsAuthorizedAgent() {
		if (authorizedSigners[msg.sender]) _;
	}

	function addAuthorizedSigner(address _signer)
		public
		onlyOwner
	{
		SignerAuthorized(_signer);

		authorizedSigners[_signer] = true;
	}

	function removeAuthorizedSigner(address _signer)
		public
		onlyOwner
	{
		SignerUnauthorized(_signer);

		authorizedSigners[_signer] = false;
	}

	function fillOrder (
		Order memory order,
		uint256 takerAssetFillAmount,
		uint256 salt,
		bytes memory orderSignature
	)
		public
		senderIsAuthorizedAgent
		returns (bool)
	{
		// Encode arguments into byte array.
		bytes memory data = abi.encodeWithSelector(
			EXCHANGE.fillOrder.selector,
			order,
			takerAssetFillAmount,
			orderSignature
		);

		EXCHANGE.executeTransaction(
			salt,
			address(this),
			data,
			TX_ORIGIN_SIGNATURE
		);
	}

	// Returns true if any of the authorized agents signed the order.
	function isValidSignature(
		bytes32 _orderHash,
		address _signer,
		bytes _signature
	)
		public
		view
		returns(bool)
	{
		if (!authorizedSigners[_signer]) {
			return false;
		}
	
		require(
			_signature.length == 65,
			"LENGTH_65_REQUIRED"
		);

		// Unpack the signature.
		uint8 v = uint8(_signature[0]);
		bytes32 r = readBytes32(_signature, 1);
		bytes32 s = readBytes32(_signature, 33);

		// Get the recovery address, and compare it to the given signer address.
		address recoveredAddress = ecrecover(_orderHash, v, r, s);
		return _signer == recoveredAddress;
	}

	function readBytes32(bytes data, uint256 index) internal pure returns (bytes32 o) {
		if (data.length / 32 > index) {
			assembly {
				o := mload(add(data, add(32, mul(32, index))))
			}
		}
	}
}
