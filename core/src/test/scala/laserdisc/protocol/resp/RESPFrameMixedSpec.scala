package laserdisc
package protocol
package resp

import org.scalacheck.Prop.forAll

final class RESPFrameMixedSpec extends RESPFrameFixture {

  test(
    "Appending to a non empty mixed frame a bit vector composed of a complete sequence of integers, simple strings, bulk strings and errors gives MoreThanOne with a list of all the complete items"
  ) {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bulk str".getBytes, 0)
    val inputVector =
      "ing\r\n+OK\r\n$0\r\n\r\n+Another simple string\r\n*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n-Possible error message\r\n*0\r\n:1\r\n:2\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n-And an error message\r\n".getBytes
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame("$16\r\nTest bulk string\r\n".getBytes),
            CompleteFrame("+OK\r\n".getBytes),
            CompleteFrame("$0\r\n\r\n".getBytes),
            CompleteFrame("+Another simple string\r\n".getBytes),
            CompleteFrame("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes),
            CompleteFrame("-Possible error message\r\n".getBytes),
            CompleteFrame("*0\r\n".getBytes),
            CompleteFrame(":1\r\n".getBytes),
            CompleteFrame(":2\r\n".getBytes),
            CompleteFrame("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes),
            CompleteFrame(":177\r\n".getBytes),
            CompleteFrame("+Another simple string\r\n".getBytes),
            CompleteFrame("$21\r\nTest bulk string 1 11\r\n".getBytes),
            CompleteFrame("*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n".getBytes),
            CompleteFrame("-And an error message\r\n".getBytes)
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  test(
    "Appending to a non empty mixed frame a bit vector composed of sequence of integers, simple strings, bulk strings and errors that are not complete gives MoreThanOne with a list of all the complete items plus the remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bulk str".getBytes, 0)
    val inputVector =
      "ing\r\n+OK\r\n+Another simple string\r\n-Possible error message\r\n:1\r\n:2\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n-And an error message\r\n".getBytes
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame("$16\r\nTest bulk string\r\n".getBytes),
            CompleteFrame("+OK\r\n".getBytes),
            CompleteFrame("+Another simple string\r\n".getBytes),
            CompleteFrame("-Possible error message\r\n".getBytes),
            CompleteFrame(":1\r\n".getBytes),
            CompleteFrame(":2\r\n".getBytes),
            CompleteFrame(":177\r\n".getBytes),
            CompleteFrame("+Another simple string\r\n".getBytes),
            CompleteFrame("$21\r\nTest bulk string 1 11\r\n".getBytes),
            CompleteFrame("-And an error message\r\n".getBytes)
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  property("Appending to an empty frame a random sequence of complete messages gives MoreThanOne with all the complete items") {
    forAll { testSet: OneOrMore[ProtocolEncoded] =>
      val vector = testSet.value.map(_.encoded).mkString.getBytes

      EmptyFrame.append(vector).onRightAll {
        case MoreThanOneFrame(complete, remainder) =>
          assertEquals(complete.size, testSet.value.size)
          assert(remainder.isEmpty)
        case CompleteFrame(_) => succeed
        case other            => fail(s"expected a MoreThanOne type. Was $other")
      }
    }
  }
}
