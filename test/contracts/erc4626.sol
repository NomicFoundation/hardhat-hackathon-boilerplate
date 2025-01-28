// SPDX-License-Identifier: UNLICENSED
pragma solidity >=0.8.0;
import {IERC20Metadata} from "@openzeppelin/contracts/interfaces/IERC20Metadata.sol";
import {ERC4626 as BaseERC4626} from "@solady/tokens/ERC4626.sol";

abstract contract ERC4626 is BaseERC4626 {
    uint256 public _totalAssets;

    function totalAssets() public view override returns (uint256) {
        return _totalAssets;
    }

    function _deposit(address by, address to, uint256 assets, uint256 shares) internal virtual override {
        super._deposit(by, to, assets, shares);
        unchecked {
            _totalAssets += assets;
        }
    }

    function _withdraw(address by, address to, address owner, uint256 assets, uint256 shares) internal virtual override {
        super._withdraw(by, to, owner, assets, shares);
        unchecked {
            _totalAssets -= assets;
        }
    }

    function _beforeWithdraw(uint256 assets, uint256 shares) internal virtual override {}

    function _afterDeposit(uint256 assets, uint256 shares) internal virtual override {}
}
