package special.sigma

import scalan.{SigmaLibrary, BaseCtxTests}

class SigmaExamplesStagedTests extends BaseCtxTests {
  lazy val ctx = new SigmaLibrary {
  }
  test("SigmaLibrary cake") {
    assert(ctx != null)
  }
}
