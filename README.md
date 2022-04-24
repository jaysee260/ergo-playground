# ErgoScript Playground

A collection of miscellaneous scenarios and their respective transactions.

| Scenario       | Description                                                                                                                 |
|----------------|-----------------------------------------------------------------------------------------------------------------------------|
| [ChainedTxs](src/main/scala/scenarios/ChainedTxs.scala) | Creates and submits 2 txs, where tx2 takes and unspent output from tx1 as an input while both txs are still in the mempool. |