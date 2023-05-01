import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

package object part3datamanipulation {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
}
