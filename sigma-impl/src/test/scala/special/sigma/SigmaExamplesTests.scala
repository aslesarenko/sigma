package special.sigma

import org.scalatest.FunSuite

class LinearAlgebraTests extends FunSuite {

  test("crowd funding") {
    val backer = new ProveDlogEvidence(1, true)
    val project = new ProveDlogEvidence(2, false)
    val c = new CrowdFundingContract(100, 1000, backer, project)
  }
}
