package High

import org.scalatest.flatspec.AnyFlatSpec

class E1AddressScheduleTest extends AnyFlatSpec {
  behavior of "compressed E1 address schedule"

  private def needsExtension(pt0: Int, pt1: Int): Boolean = {
    val pt0Wide = pt0 == 1 || pt0 == 4 || pt0 == 5
    val pt1Wide = pt1 == 1 || pt1 == 4 || pt1 == 5
    val pt0Four = pt0 == 2 || pt0 == 3
    val pt1Four = pt1 == 2 || pt1 == 3
    pt0Wide || pt1Wide || (pt0Four && pt1Four)
  }

  it should "write the transposed base rows and dense extension rows" in {
    for (outer <- 0 until 4) {
      var baseAddr = outer
      var extAddr = outer
      var extGroup = 0

      for (pt0 <- 0 until 7; pt1 <- 0 until 7) {
        assert(baseAddr == pt1 * 28 + pt0 * 4 + outer)
        if (needsExtension(pt0, pt1)) {
          assert(extAddr == extGroup * 4 + outer)
          extAddr += 4
          extGroup += 1
        }

        if (pt1 != 6) {
          baseAddr += 28
        } else if (pt0 != 6) {
          baseAddr -= 164
        }
      }

      assert(extGroup == 37)
      assert(extAddr == 148 + outer)
    }
  }

  it should "read the same base and extension rows in point-major order" in {
    var baseAddr = 0
    var extAddr = 0
    var extRows = 0

    for (pt0 <- 0 until 7; pt1 <- 0 until 7; outer <- 0 until 4) {
      assert(baseAddr == pt1 * 28 + pt0 * 4 + outer)
      if (needsExtension(pt0, pt1)) {
        assert(extAddr == extRows)
        extAddr += 1
        extRows += 1
      }

      if (outer != 3) {
        baseAddr += 1
      } else if (pt1 != 6) {
        baseAddr += 25
      } else if (!(pt0 == 6 && pt1 == 6)) {
        baseAddr -= 167
      }
    }

    assert(extRows == 148)
    assert(extAddr == 148)
  }
}
