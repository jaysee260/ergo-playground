package scenarios

import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.ErgoToolConfig
import org.ergoplatform.appkit.impl.{BlockchainContextBase, InputBoxImpl}
import org.ergoplatform.explorer.client.{DefaultApi, ExplorerApiClient}
import org.ergoplatform.restapi.client.{ApiClient, UtxoApi}

import java.util.Optional
import scala.collection.JavaConverters._

object ChainedTxs {

  def buildProver(ctx: BlockchainContext, conf: ErgoToolConfig): ErgoProver = {
    ctx.newProverBuilder()
      .withMnemonic(
        SecretString.create(conf.getNode.getWallet.getMnemonic),
        SecretString.create(conf.getNode.getWallet.getPassword))
      .withEip3Secret(0)
      .withEip3Secret(1)
      .build()
  }

  def getInputBoxesFromWallet(ctx: BlockchainContext, amountToSpend: Long, fee: Long): Optional[java.util.List[InputBox]] = {
    val totalToSpend = amountToSpend + fee
    val boxesToSpend = ctx.getWallet.getUnspentBoxes(totalToSpend)
    if (!boxesToSpend.isPresent)
      throw new ErgoClientException(s"Not enough coins in the wallet to pay $totalToSpend", null)

    boxesToSpend
  }

  def buildTx(
       ctx: BlockchainContext,
       inputs: List[InputBox],
       outputs: List[OutBox],
       fee: Long,
       changeAddress: ErgoAddress
   ): UnsignedTransaction = {
    ctx.newTxBuilder()
      .boxesToSpend(inputs.asJava)
      .outputs(outputs:_*)
      .fee(fee)
      .sendChangeTo(changeAddress)
      .build()
  }

  def getOutputBoxIdFromTxJson(txJson: String): String = {
    val pattern = """outputs":\s+\[\s+\{\s+"boxId":\s+"(.*)"""".r
    val outputBoxId = pattern.findAllIn(txJson).subgroups.head
    outputBoxId
  }

  // The tx id returned by ctx.sendTransaction has an extra pair of double quotes. ""txId123""
  // This method cleans it up and returns an id with only 2 double quotes. "txId123"
  def cleanUpTxId(txId: String): String = {
    txId.split("\"")(1)
  }

  def buildNodeService(conf: ErgoToolConfig): UtxoApi = {
    val nodeClient = new ApiClient(conf.getNode.getNodeApi.getApiUrl, "ApiKeyAuth", conf.getNode.getNodeApi.getApiKey)
    nodeClient.createService(classOf[UtxoApi])
  }

  def buildExplorerService(explorerUrl: String): DefaultApi = {
    // RestApiErgoClient.defaultTestnetExplorerUrl
    val explorerClient = new ExplorerApiClient(explorerUrl)
    explorerClient.createService(classOf[DefaultApi])
  }

  def getInputBoxFromMempool(ctx: BlockchainContext, nodeService: UtxoApi, boxId: String): InputBox = {
    val response = nodeService.getBoxWithPoolById(boxId).execute()

    if (!response.isSuccessful)
      throw new Exception(s"Something went wrong when trying to get box $boxId from mempool. ${response.message()}")

    if (response.body() == null)
      return null

    val inputBox = new InputBoxImpl(ctx.asInstanceOf[BlockchainContextBase], response.body()).asInstanceOf[InputBox]
    inputBox
  }

  def sendChainedTxs(conf: ErgoToolConfig, networkType: NetworkType): (String, String) = {
    val ergoClient = RestApiErgoClient.create(conf.getNode, RestApiErgoClient.getDefaultExplorerUrl(networkType))

    val txsJson = ergoClient.execute((ctx: BlockchainContext) => {
      val prover = buildProver(ctx, conf)

      // Build, sign and send tx 1.
      val (tx1Id, tx1Json, tx1Inputs, tx1Outputs) = {
        // Get inputs for 1st tx
        // We want the final output to be 1 ERG; so we account for the txs fees we need to cover in the tx chain.
        val amountToSpend = Parameters.OneErg + Parameters.MinFee
        val tx1Inputs = getInputBoxesFromWallet(ctx, amountToSpend, fee = Parameters.MinFee)

        // Build output(s) for 1st tx
        val tx1OutBox = ctx.newTxBuilder.outBoxBuilder
          .value(amountToSpend)
          .contract(prover.getAddress.toErgoContract)
          .build()

        // Build first unsigned tx
        val unsignedTx1 = ctx.newTxBuilder
          .boxesToSpend(tx1Inputs.get)
          .outputs(tx1OutBox)
          .fee(Parameters.MinFee)
          .sendChangeTo(prover.getP2PKAddress)
          .build()

        // Sign and send to network
        val signedTx1 = prover.sign(unsignedTx1)
        val tx1Id = ctx.sendTransaction(signedTx1)
        val tx1Json = signedTx1.toJson(true)

        (cleanUpTxId(tx1Id), tx1Json, tx1Inputs.get().asScala.toList, List(tx1OutBox))
      }

      // Give first tx a second to appear in mempool.
      Thread.sleep(1000)

      // Build, sign and send tx 2.
      // Tx 2 takes the output of Tx 1 as an input
      // while both txs are still in the mempool.
      val (tx2Id, tx2Json, tx2Inputs, tx2Outputs) = {
        // Get unspent input with 0 confirmations

        // Method 1 - Get from mempool
        //  If your program is running in a context where it doesn't have a direct reference
        //  to the output object from the previous tx, then you need the box id to get it from the mempool
        val chainedInputId = getOutputBoxIdFromTxJson(tx1Json)
        val chainedInput = getInputBoxFromMempool(ctx, buildNodeService(conf), chainedInputId)

        // Method 2 - Use reference to output object from tx 1 to convert it to input to tx 2
        //  But if your program builds the entire chain of txs at once, and has a direct reference
        //  to the outputs of the previous tx, then you can reference them and call .convertToInputWith on them
        // val chainedInput = tx1Outputs.head.convertToInputWith(tx1Id, 0)

        // Build output(s) for 2nd tx
        val tx2OutBox = ctx.newTxBuilder.outBoxBuilder
          .value(chainedInput.getValue - Parameters.MinFee)
          .contract(prover.getAddress.toErgoContract)
          .build()

        // Build second unsigned tx
        val unsignedTx2 = ctx.newTxBuilder()
          .boxesToSpend(List(chainedInput).asJava)
          .outputs(tx2OutBox)
          .fee(Parameters.MinFee)
          .sendChangeTo(prover.getAddress.getErgoAddress)
          .build()

        // Sign and send to network
        val signedTx2 = prover.sign(unsignedTx2)
        val tx2Id = ctx.sendTransaction(signedTx2)
        val tx2Json = signedTx2.toJson(true)

        (cleanUpTxId(tx2Id), tx2Json, List(chainedInput), List(tx2OutBox))
      }

      (tx1Json, tx2Json)
    })

    txsJson
  }

  def main(args: Array[String]): Unit = {
    val conf = ErgoToolConfig.load("ergo_config.json")
    val networkType = conf.getNode.getNetworkType

    val txsJson = sendChainedTxs(conf, networkType)
    println(txsJson)
  }
}
