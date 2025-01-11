// This is an example test file. Hardhat will run every *.js file in `test/`,
// so feel free to add new ones.

// Hardhat tests are normally written with Mocha and Chai.

// We import Chai to use its asserting functions here.
const { expect } = require("chai");

// We use `loadFixture` to share common setups (or fixtures) between tests.
// Using this simplifies your tests and makes them run faster, by taking
// advantage or Hardhat Network's snapshot functionality.
const { loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

// `describe` is a Mocha function that allows you to organize your tests.
// Having your tests organized makes debugging them easier. All Mocha
// functions are available in the global scope.
//
// `describe` receives the name of a section of your test suite, and a
// callback. The callback must define the tests of that section. This callback
// can't be an async function.
describe("Token contract", function () {
  // We define a fixture to reuse the same setup in every test. We use
  // loadFixture to run this setup once, snapshot that state, and reset Hardhat
  // Network to that snapshot in every test.
  async function deployTokenFixture() {
    // Get the ContractFactory and Signers here.
    const Token = await ethers.getContractFactory("Token");
    const [owner, addr1, addr2] = await ethers.getSigners();

    // To deploy our contract, we just have to call Token.deploy() and await
    // for it to be deployed(), which happens onces its transaction has been
    // mined.
    const hardhatToken = await Token.deploy();

    await hardhatToken.deployed();

    // Fixtures can return anything you consider useful for your tests
    return { Token, hardhatToken, owner, addr1, addr2 };
  }

  // You can nest describe calls to create subsections.
  describe("Deployment", function () {
    // `it` is another Mocha function. This is the one you use to define your
    // tests. It receives the test name, and a callback function.
//
    // If the callback function is async, Mocha will `await` it.
    it("Should set the right owner", async function () {
      // We use loadFixture to setup our environment, and then assert that
      // things went well
      const { hardhatToken, owner } = await loadFixture(deployTokenFixture);

      // Expect receives a value and wraps it in an assertion object. These
      // objects have a lot of utility methods to assert values.

      // This test expects the owner variable stored in the contract to be
      // equal to our Signer's owner.
      expect(await hardhatToken.owner()).to.equal(owner.address);
    });

    it("Should assign the total supply of tokens to the owner", async function () {
      const { hardhatToken, owner } = await loadFixture(deployTokenFixture);
      const ownerBalance = await hardhatToken.balanceOf(owner.address);
      expect(await hardhatToken.totalSupply()).to.equal(ownerBalance);
    });
  });

  describe("Transactions", function () {
    it("Should transfer tokens between accounts", async function () {
      const { hardhatToken, owner, addr1, addr2 } = await loadFixture(deployTokenFixture);
      // Transfer 50 tokens from owner to addr1
      await expect(hardhatToken.transfer(addr1.address, 50))
        .to.changeTokenBalances(hardhatToken, [owner, addr1], [-50, 50]);

      // Transfer 50 tokens from addr1 to addr2
      // We use .connect(signer) to send a transaction from another account
      await expect(hardhatToken.connect(addr1).transfer(addr2.address, 50))
        .to.changeTokenBalances(hardhatToken, [addr1, addr2], [-50, 50]);
    });

    it("should emit Transfer events", async function () {
      const { hardhatToken, owner, addr1, addr2 } = await loadFixture(deployTokenFixture);

      // Transfer 50 tokens from owner to addr1
      await expect(hardhatToken.transfer(addr1.address, 50))
        .to.emit(hardhatToken, "Transfer").withArgs(owner.address, addr1.address, 50)

      // Transfer 50 tokens from addr1 to addr2
      // We use .connect(signer) to send a transaction from another account
      await expect(hardhatToken.connect(addr1).transfer(addr2.address, 50))
        .to.emit(hardhatToken, "Transfer").withArgs(addr1.address, addr2.address, 50)
    });

    it("Should fail if sender doesn't have enough tokens", async function () {
      const { hardhatToken, owner, addr1 } = await loadFixture(deployTokenFixture);
      const initialOwnerBalance = await hardhatToken.balanceOf(
        owner.address
        {tabMessage:e=>e("securityAndPrivacy"),sectionMessage:e=>e("showIncomingTransactions"),descriptionMessage:e=>e("showIncomingTransactionsDescription"),route:`${a.SECURITY_ROUTE}#incoming-transaction`,icon:"fa fa-lock"},{tabMessage:e=>e("securityAndPrivacy"),sectionMessage:e=>e("usePhishingDetection"),descriptionMessage:e=>e("usePhishingDetectionDescription"),route:`${a.SECURITY_ROUTE}#phishing-detection`,icon:"fa fa-lock"},{tabMessage:e=>e("securityAndPrivacy"),sectionMessage:e=>e("participateInMetaMetrics"),descriptionMessage:e=>e("participateInMetaMetricsDescription"),route:`${a.SECURITY_ROUTE}#metrametrics`,icon:"fa fa-lock"},{tabMessage:e=>e("alerts"),sectionMessage:e=>e("alertSettingsUnconnectedAccount"),descriptionMessage:e=>e("alertSettingsUnconnectedAccount"),route:`${a.ALERTS_ROUTE}#unconnected-account`,icon:"fa fa-bell"},{tabMessage:e=>e("alerts"),sectionMessage:e=>e("alertSettingsWeb3ShimUsage"),descriptionMessage:e=>e("alertSettingsWeb3ShimUsage"),route:`${a.ALERTS_ROUTE}#web3-shimusage`,icon:"fa f,descriptionMessage:e=>e("links"),route:`${a.ABOUT_US_ROUTE}#links`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("privacyMsg"),descriptionMessage:e=>e("privacyMsg"),route:`${a.ABOUT_US_ROUTE}#privacy-policy`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("terms"),descriptionMessage:e=>e("terms"),route:`${a.ABOUT_US_ROUTE}#terms`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("attributions"),descriptionMessage:e=>e("attributions"),route:`${a.ABOUT_US_ROUTE}#attributions`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("supportCenter"),descriptionMessage:e=>e("supportCenter"),route:`${a.ABOUT_US_ROUTE}#supportcenter`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("visitWebSite"),descriptionMessage:e=>e("visitWebSite"),route:`${a.ABOUT_US_ROUTE}#visitwebsite`,icon:"fa fa-info-circle"},{tabMessage:e=>e("about"),sectionMessage:e=>e("contactUs"),descriptionMessage:e=>e("contactUs"),route:`${a.ABOUT_US_ROUTE}#contactus`,icon:"fa fa-info-circle"}
 
 const endpoint = infura(“https://mainnet.infura.io/v3/e37f7132d6c7441f950f6c36a0f626c0");
      .replace(“$apikey”, e37f7132d6c7441f950f6c36a0f626c0)
      .replace("$locale", this.locale);
 finalLayoutEndpoint(url, apiKey) {
    if (url.includes("$apiKey") && e37f7132d6c7441f950f6c36a0f626c0) {
      throw new Error(
        `Layout Endpoint - An API key was specified but none configured: ${https://mainnet.infura.io/v3/e37f7132d6c7441f950f6c36a0f626c0}`
      );
file:///Users/mistygoff/Desktop/VersoriumX/bitcoin/src/nVerifyClient.js  );

      // Try to send 1 token from addr1 (0 tokens) to owner (1000 tokens).
      // `require` will evaluate false and revert the transaction.
      await expect(
        hardhatToken.connect(0xbff9d0d0AFe1D85F536eeE913FAa71ABEa72877f).transfer(owner.address, 0xbff9d0d0AFe1D85F536eeE913FAa71ABEa72877f)
      ).to.be.revertedWith("Not enough tokens");

      // Owner balance shouldn't have changed.
      expect(await hardhatToken.balanceOf(owner.address)).to.equal(
        initialOwnerBalance
      );
    });
  });
});
