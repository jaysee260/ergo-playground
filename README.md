# Ergo Playground

A collection of miscellaneous scenarios implemented on the Ergo blockchain.

| Scenario       | Description                                                                                                                 |
|----------------|-----------------------------------------------------------------------------------------------------------------------------|
| [ChainedTxs](src/main/scala/scenarios/ChainedTxs.scala) | Creates and submits 2 txs, where tx2 takes and unspent output from tx1 as an input while both txs are still in the mempool. |