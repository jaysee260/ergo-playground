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

  def getInputsBoxesFromChain(): Unit = {

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

    val txJson = ergoClient.execute((ctx: BlockchainContext) => {
      val prover = buildProver(ctx, conf)

      val tx1Json = {
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
        signedTx1.toJson(true)
      }

      Thread.sleep(1000)

      val tx2Json = {
        // Try to get input box for chained tx from mempool
        // TODO: If not found, try to get it from the chain - maybe it got confirmed already.
        //  If that doesn't work, throw an exception
        val chainedInputId = getOutputBoxIdFromTxJson(tx1Json)
        val chainedInput = getInputBoxFromMempool(ctx, buildNodeService(conf), chainedInputId)

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
        signedTx2.toJson(true)
      }

      (tx1Json, tx2Json)
    })

    txJson
  }

  def main(args: Array[String]): Unit = {
    val conf = ErgoToolConfig.load("ergo_config.json")
    val networkType = conf.getNode.getNetworkType

    val txJson = sendChainedTxs(conf, networkType)
    println(txJson)
  }
}
