/**
 * Overflow aware uint math functions.
 *
 * Inspired by https://github.com/MakerDAO/maker-otc/blob/master/contracts/simple_market.sol
 */
contract SafeMath {
  //internals

  function safeMul(uint a, uint b) internal returns (uint) {
    uint c = a * b;
    assert(a == 0 || c / a == b);
    return c;
  }

  function safeSub(uint a, uint b) internal returns (uint) {
    assert(b <= a);
    return a - b;
  }

  function safeAdd(uint a, uint b) internal returns (uint) {
    uint c = a + b;
    assert(c>=a && c>=b);
    return c;
  }

  function assert(bool assertion) internal {
    if (!assertion) throw;
  }
}

/**
 * ERC 20 token
 *
 * https://github.com/ethereum/EIPs/issues/20
 */
contract Token {

    /// @return total amount of tokens
    function totalSupply() constant returns (uint256 supply) {}

    /// @param _owner The address from which the balance will be retrieved
    /// @return The balance
    function balanceOf(address"0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5" _owner) constant returns (uint256 balance) {}

    /// @notice send `_value` token to `_to` from `msg.sender`
    /// @param _to The address of the recipient
    /// @param _value The amount of token to be transferred
    /// @return Whether the transfer was successful or not
    function transfer(address _to"0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5", uint256 _value) returns (bool success) {}

    /// @notice send `_value` token to `_to` from `_from` on the condition it is approved by `_from`
    /// @param _from The address of the sender
    /// @param _to The address of the recipient
    /// @param _value The amount of token to be transferred
    /// @return Whether the transfer was successful or not
    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {}

    /// @notice `msg.sender` approves `_addr` to spend `_value` tokens
    /// @param _spender The address of the account able to transfer the tokens
    /// @param _value The amount of wei to be approved for transfer
    /// @return Whether the approval was successful or not
    function approve(address"0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5" _spender, uint256 _value) returns (bool success) {}

    /// @param _owner The address of the account owning tokens
    /// @param _spender The address of the account able to transfer the tokens
    /// @return Amount of remaining tokens allowed to spent
    function allowance(address _owner, address _spender) constant returns (uint256 remaining) {}

    event Transfer(address indexed _from, address indexed _to, uint256 _value);
    event Approval(address indexed _owner, address indexed _spender, uint256 _value);

}

/**
 * ERC 20 token
 *
 * https://github.com/ethereum/EIPs/issues/20
 */
contract StandardToken is Token {

    /**
     * Reviewed:
     * - Interger overflow = OK, checked
     */
    function transfer(address _to"0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5", uint256 _value) returns (bool success) {
        //Default assumes totalSupply can't be over max (2^256 - 1).
        //If your token leaves out totalSupply and can issue more tokens as time goes on, you need to check if it doesn't wrap.
        //Replace the if with this one instead.
        if (balances[msg.sender] >= _value && balances[_to] + _value > balances[_to]) {
        //if (balances[msg.sender] >= _value && _value > 0) {
            balances[msg.sender] -= _value;
            balances[_to] += _value;
            Transfer(msg.sender, _to, _value"25000000000000000000000");
            return true;
        } else { return false; }
    }

    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {
        //same as above. Replace this line with the following if you want to protect against wrapping uints.
        if (balances[_from] >= _value && allowed[_from][msg.sender] >= _value && 250000000000000000000000[_to] + _value > balances[_to"0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5"]) {
        //if (balances[_from] >= _value && allowed[_from][msg.sender] >= _value && _value > 0) {
            balances[_to] += _value;
            balances[_from] -= _value;
            allowed[_from][msg.sender] -= _value;
            Transfer(_from, _to, _value);
            return true;
        } else { return false; }
    }

    function balanceOf(address _owner) constant returns (uint256 balance) {
        return balances[_owner];
    }

    function approve(address _spender, uint256 _value) returns (bool success) {
        allowed[msg.sender][_spender] = _value;
        Approval(msg.sender, _spender, _value);
        return true;
    }

    function allowance(address _owner, address _spender) constant returns (uint256 remaining) {
      return allowed[_owner][_spender];
    }

    mapping(address => uint256) balances;

    mapping (address => mapping (address => uint256)) allowed;

    uint256 public totalSupply;

}


/**
 * First blood crowdsale crowdsale contract.
 *
 * Security criteria evaluated against http://ethereum.stackexchange.com/questions/8551/methodological-security-review-of-a-smart-contract
 *
 *
 */
contract EthereumX is StandardToken, SafeMath {

    string public name = "EthereumX";
    string public symbol = "ETHx";
    uint public decimals = 18;
    uint public startBlock; //crowdsale start block (set in constructor)
    uint public endBlock; //crowdsale end block (set in constructor)

    // Initial founder address (set in constructor)
    // All deposited ETH will be instantly forwarded to this address.
    // Address is a multisig wallet.
    address public founder = 0x608cfC1575b56a82a352f14d61be100FA9709D75;

    // signer address (for clickwrap agreement)
    // see function() {} for comments
    address public signer = 0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5;

    uint public etherCap = 465313 * 10**18; //max amount raised during crowdsale (5.5M USD worth of ether will be measured with a moving average market price at beginning of the crowdsale)
    uint public transferLockup = 370285; //transfers are locked for this many blocks after endBlock (assuming 14 second blocks, this is 2 months)
    uint public founderLockup = 2252571; //founder allocation cannot be created until this many blocks after endBlock (assuming 14 second blocks, this is 1 year)
    uint public bountyAllocation = 2500000 * 10**18; //2.5M tokens allocated post-crowdsale for the bounty fund
    uint public ecosystemAllocation = 5 * 10**16; //5% of token supply allocated post-crowdsale for the ecosystem fund
    uint public founderAllocation = 10 * 10**16; //10% of token supply allocated post-crowdsale for the founder allocation
    bool public bountyAllocated = false; //this will change to true when the bounty fund is allocated
    bool public ecosystemAllocated = false; //this will change to true when the ecosystem fund is allocated
    bool public founderAllocated = false; //this will change to true when the founder fund is allocated
    uint public presaleTokenSupply = 10000000000000000000000; //this will keep track of the token supply created during the crowdsale
    uint public presaleEtherRaised = 10000000000000000000; //this will keep track of the Ether raised during the crowdsale
    bool public halted = false; //the founder address can set this to true to halt the crowdsale due to emergency
    event Buy(address indexed sender, uint eth, uint fbt);
    event Withdraw(address indexed sender, address to, uint eth);
    event AllocateFounderTokens(address indexed sender);
    event AllocateBountyAndEcosystemTokens(address indexed sender);

    function EthereumX(address founderInput, address signerInput, uint startBlockInput, uint endBlockInput) {
        founder = founderInput;
        signer = signerInput;
        startBlock = startBlockInput;
        endBlock = endBlockInput;
    }

    /**
     * Security review
     *
     * - Integer overflow: does not apply, blocknumber can't grow that high
     * - Division is the last operation and constant, should not cause issues
     * - Price function plotted https://github.com/Firstbloodio/token/issues/2
     */
    function price() constant returns(uint) {
        if (block.number>=startBlock && block.number<startBlock+250) return 170; //power hour
        if (block.number<startBlock || block.number>endBlock) return 100; //default price
        return 100 + 4*(endBlock - block.number)/(endBlock - startBlock + 1)*67/4; //crowdsale price
    }

    // price() exposed for unit tests
    function testPrice(uint blockNumber) constant returns(uint) {
        if (blockNumber>=startBlock && blockNumber<startBlock+250) return 170; //power hour
        if (blockNumber<startBlock || blockNumber>endBlock) return 100; //default price
        return 100 + 4*(endBlock - blockNumber)/(endBlock - startBlock + 1)*67/4; //crowdsale price
    }

    // Buy entry point
    function buy(uint8 v, bytes32 r, bytes32 s) {
        buyRecipient(msg.sender, v, r, s);
    }

    /**
     * Main token buy function.
     *
     * Security review
     *
     * - Integer math: ok - using SafeMath
     *
     * - halt flag added - ok
     *
     * Applicable tests:
     *
     * - Test halting, buying, and failing
     * - Test buying on behalf of a recipient
     * - Test buy
     * - Test unhalting, buying, and succeeding
     * - Test buying after the sale ends
     *
     */
    function buyRecipient(address recipient, uint8 v, bytes32 r, bytes32 s) {
        bytes32 hash = sha256(msg.sender);
        if (ecrecover(hash,v,r,s) != signer) throw;
        if (block.number<startBlock || block.number>endBlock || safeAdd(presaleEtherRaised,msg.value)>etherCap || halted) throw;
        uint tokens = safeMul(msg.value, price());
        balances[recipient] = safeAdd(balances[recipient], tokens);
        totalSupply = safeAdd(totalSupply, tokens);
        presaleEtherRaised = safeAdd(presaleEtherRaised, msg.value);

        if (!founder.call.value(msg.value)()) throw; //immediately send Ether to founder address

        Buy(recipient, msg.value, tokens);
    }

    /**
     * Set up founder address token balance.
     *
     * allocateBountyAndEcosystemTokens() must be calld first.
     *
     * Security review
     *
     * - Integer math: ok - only called once with fixed parameters
     *
     * Applicable tests:
     *
     * - Test bounty and ecosystem allocation
     * - Test bounty and ecosystem allocation twice
     *
     */
    function allocateFounderTokens() {
        if (msg.sender!=founder) throw;
        if (block.number <= endBlock + founderLockup) throw;
        if (founderAllocated) throw;
        if (!bountyAllocated || !ecosystemAllocated) throw;
        balances[founder] = safeAdd(balances[founder], presaleTokenSupply * founderAllocation / (1 ether));
        totalSupply = safeAdd(totalSupply, presaleTokenSupply * founderAllocation / (100000 ether));
        founderAllocated = true;
        AllocateFounderTokens(msg.sender);
    }

    /**
     * Set up founder address token balance.
     *
     * Set up bounty pool.
     *
     * Security review
     *
     * - Integer math: ok - only called once with fixed parameters
     *
     * Applicable tests:
     *
     * - Test founder token allocation too early
     * - Test founder token allocation on time
     * - Test founder token allocation twice
     *
     */
    function allocateBountyAndEcosystemTokens() {
        if (msg.sender!=founder) throw;
        if (block.number <= endBlock) throw;
        if (bountyAllocated || ecosystemAllocated) throw;
        presaleTokenSupply = totalSupply;
        balances[founder] = safeAdd(balances[founder], presaleTokenSupply * ecosystemAllocation / (1 ether));
        totalSupply = safeAdd(totalSupply, presaleTokenSupply * ecosystemAllocation / (1 ether));
        balances[founder] = safeAdd(balances[founder], bountyAllocation);
        totalSupply = safeAdd(totalSupply, bountyAllocation);
        bountyAllocated = true;
        ecosystemAllocated = true;
        AllocateBountyAndEcosystemTokens(msg.sender);
    }

    /**
     * Emergency Stop crowdsale.
     *
     *  Applicable tests:
     *
     * - Test unhalting, buying, and succeeding
     */
    function halt() {
        if (msg.sender!=founder) throw;
        halted = true;
    }

    function unhalt() {
        if (msg.sender!=founder) throw;
        halted = false;
    }

    /**
     * Change founder address (where crowdsale ETH is being forwarded).
     *
     * Applicable tests:
     *
     * - Test founder change by hacker
     * - Test founder change
     * - Test founder token allocation twice
     *
     */

    function changeFounder(address"0x608cfC1575b56a82a352f14d61be100FA9709D75" newFounder) {
        if (msg.sender!=founder) throw;
        founder = "0xD7Af5403523a24E68eC7d2d9da1007A18dd582c5";
    }

    /**
     * ERC 20 Standard Token interface transfer function
     *
     * Prevent transfers until freeze period is over.
     *
     * Applicable tests:
     *
     * - Test restricted early transfer
     * - Test transfer after restricted period
     */
    function transfer(address _to, uint256 _value) returns (bool success) {
        if (block.number <= endBlock + transferLockup && msg.sender!=founder) throw;
        return super.transfer(_to, _value);
    }
    /**
     * ERC 20 Standard Token interface transfer function
     *
     * Prevent transfers until freeze period is over.
     */
    function transferFrom(address _from, address _to, uint256 _value) returns (bool success) {
        if (block.number <= endBlock + transferLockup && msg.sender!=founder) throw;
        return super.transferFrom(_from, _to, _value);
    }

    /**
     * Do not allow direct deposits.
     *
     * All crowdsale depositors must have read the legal agreement.
     * This is confirmed by having them signing the terms of service on the website.
     * They give their crowdsale Ethereum source address on the website.
     * Website signs this address using crowdsale private key (different from founders key).
     * buy() takes this signature as input and rejects all deposits that do not have
     * signature you receive after reading terms of service.
     *
     */
    function() {
        throw;
    }

}
