import play.modules.swagger._

import org.specs2.mutable._
import org.specs2.mock.Mockito

import test.testdata.DogController
import io.swagger.core.{SwaggerSpec, SwaggerContext}
import io.swagger.config.SwaggerConfig

import org.mockito.Mockito._

class PlayApiScannerSpec extends Specification with Mockito {

  // set up mock for Play Router
  val mockRoutes = mock[play.api.routing.Router]
  val routesDocumentation: Seq[(String, String, String)] = Seq(
    ("GET", "/api/dog", "test.testdata.DogController.list"),
    ("PUT", "/api/dog", "test.testdata.DogController.add"),
    ("GET", "/api/cat", "@test.testdata.CatController.list"),
    ("GET", "/api/cat", "@test.testdata.CatController.add"),
    ("GET", "/api/fly", "test.testdata.FlyController.list")
  )
  mockRoutes.documentation returns routesDocumentation

  "PlayApiScanner" should {
    "identify correct API classes based on router and API annotations" in {
      val classes = new PlayApiScanner(Some(mockRoutes)).classes()
      classes.length must beEqualTo(2)
      classes.find(clazz => clazz == SwaggerContext.loadClass("test.testdata.DogController")).nonEmpty must beTrue
      classes.find(clazz => clazz == SwaggerContext.loadClass("test.testdata.CatController")).nonEmpty must beTrue
    }
  }

}