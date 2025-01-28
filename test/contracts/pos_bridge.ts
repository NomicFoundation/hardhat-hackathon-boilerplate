import { erc721, from, posClient, posClientForTo, privateKey, RPC, to, toPrivateKey } from "./client";
import { expect } from 'chai'
import { ABIManager } from '@maticnetwork/maticjs'
import { providers, Wallet } from "ethers";

describe('POS Client', () => {

    const abiManager = new ABIManager("testnet", "mumbai");
    const parentPrivder = new providers.JsonRpcProvider(RPC.parent);
    const childProvider = new providers.JsonRpcProvider(RPC.child);

    before(() => {
        return Promise.all([
            abiManager.init()
        ]);
    });

    it("pos client from init", async () => {
        await posClient.init({
            // log: true,
            network: 'testnet',
            version: 'mumbai',
            parent: {
                provider: new Wallet(privateKey, parentPrivder),
                defaultConfig: {
                    from
                }
            },
            child: {
                provider: new Wallet(privateKey, childProvider),
                defaultConfig: {
                    from
                }
            }
        });
    })

    it("pos client to init", async () => {
        await posClientForTo.init({
            // log: true,
            network: 'testnet',
            version: 'mumbai',
            parent: {
                provider: new Wallet(toPrivateKey, parentPrivder),
                defaultConfig: {
                    from: to
                }
            },
            child: {
                provider: new Wallet(toPrivateKey, childProvider),
                defaultConfig: {
                    from: to
                }
            }
        });
    })

    const txHash = `0x92898987248eaec73dc56eee44f68084a2adcb13a83213590cce437d54aa17db`;
    it("null tx check getTransactionReceipt", async () => {
        try {
            await posClient.client.parent.getTransactionReceipt(txHash);
            throw "should have been error";
        } catch (error) {
            expect(error).eql({
                type: 'invalid_transaction' as any,
                message: 'Could not retrieve transaction. Either it is invalid or might be in archive node.'
            });
        }
    })

    it("null tx check getTransaction", async () => {
        try {
            await posClient.client.parent.getTransaction(txHash);
            throw "should have been error";
        } catch (error) {
            expect(error).eql({
                type: 'invalid_transaction' as any,
                message: 'Could not retrieve transaction. Either it is invalid or might be in archive node.'
            });
        }
    })
});
