package data

object Readers {

  case class Configuration(
    username: String,
    password: String,
    host: String,
    port: Int,
    nbThreads: Int,
    emailReplyTo: String,
  )

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "shipped" // select * from db table ...

    def getLastOrderId(username: String): Long =
      741852 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started")
  }

  // bootstrap
  val config = Configuration(
    "aliyou",
    "secret",
    "localhost",
    1234,
    8,
    "contact@company.com",
  )

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] =
    Reader(conf => DbConnection(conf.username, conf.password))

  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val orderStatusReader: Reader[Configuration, String] =
    dbReader.map(c => c.getOrderStatus(42))

  val orderStatus = orderStatusReader.run(config)

  /**
   * Pattern
   *  1. create the initial data structure
   *  2. create a reader which specifies how that data structure will be manipulated later
   *  3. can map & flatMap the reader to produce derived information
   *  4. when the final piece of information is needed, call run on the reader with the initial data structure
   */

  def getLastOrderStatus(username: String): String = {
    // val userLastOrderIdReader: Reader[Configuration, String] =
    //   dbReader
    //     .map(_.getLastOrderId(username))
    //     .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)))

    val userOrderFor: Reader[Configuration, String] =
      for {
        lastOrderId <- dbReader.map(_.getLastOrderId(username))
        orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      } yield orderStatus

    userOrderFor.run(config)
  }

  // Ex
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, email: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] =
      Reader(conf => EmailService(conf.emailReplyTo))

    val emailReader: Reader[Configuration, String] =
      for {
        lastOrderId  <- dbReader.map(_.getLastOrderId(username))
        orderStatus  <- dbReader.map(_.getOrderStatus(lastOrderId))
        emailService <- emailServiceReader
      } yield emailService
        .sendEmail(
          email,
          s"Your last order status: $orderStatus",
        )

    emailReader.run(config)
  }

  // Dependency injection

  def main(args: Array[String]): Unit =
    println(emailUser("User", "user@user.com"))
}
