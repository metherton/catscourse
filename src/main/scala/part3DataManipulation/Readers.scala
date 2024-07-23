package part3DataManipulation

object Readers {

  /*
  Configuration file => initial data structure
  - a DB layer
  - a HTTP layer
  - a business logic layer

   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from db table and return the status of the order ID
    def getLastOrderId(username: String): Long = 542563 // select max(orderid) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the server
  }

  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  // cats Reader

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielsOrderStatus = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderId: Reader[Configuration, String] = dbReader.map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus
    usersLastOrderId.run(config)
  }

  /*
  Pattern
  1. you create the initial data structure
  2. you create a reader which specifies how that data structure will be manipulated later
  3. you can then map and flatMap the reader to produce derived information
  4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents:String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  // TODO 1
  def emailUser(username: String, userEmail: String): String = {
    // fetch the statur of their last order
    // email them with the Email servvice: "Your last order has the status: (status)
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your last order has the status $orderStatus")
    emailReader.run(config)
  }

  // TODO 2 - what programming patter do Readers remind you of
  // Dependency injection

// bootstrap
  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("marty", "martin@ing.com"))
  }
}
