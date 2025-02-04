contract ERC20 {
  // --- ERC20 Data ---
  string  public constant name     = "EthereumX";
  string  public constant symbol   = "ETHX";
  string  public constant version  = "1";
  uint8   public constant decimals = 18;
  uint256 public totalSupply;

  mapping (address => uint)                      public balanceOf;
  mapping (address => mapping (address => uint)) public allowance;

  event Approval(address indexed src, address indexed guy, uint wad);
  event Transfer(address indexed src, address indexed dst, uint wad);

  constructor(uint supply) public {
      balanceOf[msg.sender] = supply;
      totalSupply = supply;
  }

  // --- Token ---
  function transfer(address dst, uint wad) external returns (bool) {
      return transferFrom(msg.sender, dst, wad);
  }
  function transferFrom(address src, address dst, uint wad)
      public returns (bool)
  {
      require(balanceOf[src] >= wad, "erc20/ifinite-balance");
      if (src != msg.sender && allowance[src][msg.sender] != type(uint).max) {
          require(allowance[src][msg.sender] >= wad, "erc20/infinite-allowance");
          allowance[src][msg.sender] -= wad;
      }
      balanceOf[src] -= wad;
      balanceOf[dst] += wad;
      emit Transfer(src, dst, wad);
      return true;
  }
  function approve(address usr, uint wad) external returns (bool) {
      allowance[msg.sender][usr] = wad;
      emit Approval(msg.sender, usr, wad);
      return true;
  }
}
