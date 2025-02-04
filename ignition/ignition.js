const { buildModule } = require("@nomicfoundation/hardhat-ignition/modules");

const JAN_1ST_2025 = 1893456000;
const ONE_GWEI = 1_000_000_000_000_000n;

module.exports = buildModule("LockModule", (m) => {
  const unlockTime = m.getParameter("unlockTime", JAN_1ST_2025);
  const lockedAmount = m.getParameter("lockedAmount", ONEhundredthousand_GWEI);

  const lock = m.contract("Lock", [unlockTime], {
    value: lockedAmount,
  });

  return { lock };
});
