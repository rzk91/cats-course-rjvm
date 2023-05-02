package part3datamanipulation

import cats.Id

object Readers {

  /*
   What are readers? They help the following scenario:
   - We have a configuration file that has an initial data structure for the following layers
   - A DB layer
   - an HTTP layer
   - A business logic layer
   */

  case class Configuration(
    dbUsername: String,
    dbPassword: String,
    host: String,
    port: Int,
    numThreads: Int,
    replyTo: String
  )

  case class DbConnection(username: String, password: String) {

    def getOrderStatus(orderId: Long): String = "Pending"
    // IRL, this would be based on e.g. SELECT status FROM table WHERE order_id = $orderId

    def getLastOrderId(username: String): Long = 1234567
    // IRL, this would be something like, SELECT MAX(order_id) FROM table WHERE username = $username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started") // IRL, this would start the actual server
  }

  // Bootstrap phase
  val config: Configuration =
    Configuration("me", "123456!", "localhost", 12345, 8, "me@example.com")
  // Cats reader
  import cats.data.Reader
  // Reader[I, O]

  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))

  // So, to get a DB connection
  val dbConnection: Id[DbConnection] = dbReader.run(config)

  val myOrderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(42))
  val myOrderStatus: String = myOrderStatusReader.run(config)

  // So, Readers are useful to read something in the middle of an application
  // by reading some constants at the beginning of the application

  /*
  Pattern:
  - Create an initial data structure
  - Create a reader that specifies how the initial data structure can be manipulated later
  - Use map/flatMap to produce the required information from the reader
  - When we finally need the piece of information, run the reader with the initial data structure
  - So, a reader basically dictates what we need and then produces that when we call `run`
   */

  def getLastOrderStatus(username: String): String = {
    // val lastOrderIdForUserReader = dbReader.map(_.getLastOrderId(username))
    // val lastOrderStatusForUserReader = dbReader.map(_.getOrderStatus(???))
    // ??? needs to be the ID that we retrieved above

    val lastOrderIdForUserReader = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)))

    // Alternative
    val lastOrderIdForUserReaderFor = for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      status  <- dbReader.map(_.getOrderStatus(orderId))
    } yield status

    lastOrderIdForUserReaderFor.run(config)
  }

  // Exercise 1: Email service
  case class EmailService(replyTo: String) {

    def sendMail(address: String, contents: String): String =
      s"From: $replyTo; to: $address >>> $contents"
  }

  val emailServiceReader: Reader[Configuration, EmailService] =
    Reader(conf => EmailService(conf.replyTo))

  // Create a method called "emailUser" that:
  // - fetches status of their last order
  // - emails them with EmailService: "Your last order has the status: $status"
  // Solution
  def emailUser(username: String, userEmail: String): String = {
    for {
      lastOrder <- dbReader.map(_.getLastOrderId(username))
      status    <- dbReader.map(_.getOrderStatus(lastOrder))
      content = s"Your last order has the status: $status"
      emailService <- emailServiceReader
    } yield emailService.sendMail(userEmail, content)
  }.run(config)

  // Readers can be used for dependency injection!

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("me"))

    println(emailUser("me", "you@example.com"))
  }
}
