package laserdisc
package protocol
package resp

final class RESPFrameBulkSpec extends BaseSpec {

  test("Appending to an empty Bulk frame a bit vector bit vector that's complete gives Complete with all the bits") {
    val inputVector = "$16\r\nTest bulk string\r\n".getBytes
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test(
    "Appending to an empty Bulk frame a bit vector that's complete and includes unsafe characters (\\r\\n) gives Complete with all the bits"
  ) {
    val inputVector = "$16\r\nTest \n\r ! string\r\n".getBytes
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test("Appending to an empty Bulk frame an empty bulk bit vector gives Complete with an empty content") {
    val inputVector = "$0\r\n\r\n".getBytes
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test(
    "Appending to an empty Bulk frame a bit vector that's not complete gives Incomplete with the correct partial and the correct missing count"
  ) {
    val inputVector = "$16\r\nTest bulk string".getBytes
    assertEquals(EmptyFrame.append(inputVector), IncompleteFrame(inputVector, 2))
  }

  test(
    "Appending to an empty Bulk frame a bit vector with a number split in two chunks gives MoreThanOneFrame with a follow up partial bulk string"
  ) {
    val incompleteFirstInput = ":2".getBytes
    val secondInput          = s"1$CRLF$$18${CRLF}Test bulk".getBytes
    assertEquals(
      EmptyFrame.append(incompleteFirstInput).flatMap(_.append(secondInput)),
      MoreThanOneFrame(
        Vector(CompleteFrame(s":21$CRLF".getBytes)),
        s"$$18${CRLF}Test bulk".getBytes
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple messages all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = "$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(3)(CompleteFrame("$16\r\nTest bulk string\r\n".getBytes)),
        Array.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple messages with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val inputVector = "$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(2)(CompleteFrame("$16\r\nTest bulk string\r\n".getBytes)),
        "$16\r\nTest bulk".getBytes
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = "$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame("$-1\r\n".getBytes)),
        Array.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = "$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame("$-1\r\n".getBytes)),
        Array.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val inputVector = "$-1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(4)(CompleteFrame("$-1\r\n".getBytes)),
        "$".getBytes
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits"
  ) {
    val inputVector =
      "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector(
          CompleteFrame("$18\r\nTest bulk string 1\r\n".getBytes),
          CompleteFrame("$18\r\nTest bulk string 2\r\n".getBytes),
          CompleteFrame("$18\r\nTest bulk string 3\r\n".getBytes),
          CompleteFrame("$18\r\nTest bulk string 4\r\n".getBytes)
        ),
        "$18\r\nTest bulk".getBytes
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne where the call to complete should give a vector with the complete ones in the original order"
  ) {
    val inputVector =
      "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
    EmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame("$18\r\nTest bulk string 1\r\n".getBytes),
            CompleteFrame("$18\r\nTest bulk string 2\r\n".getBytes),
            CompleteFrame("$18\r\nTest bulk string 3\r\n".getBytes),
            CompleteFrame("$18\r\nTest bulk string 4\r\n".getBytes)
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  test("Appending to a non empty Bulk frame a bit vector that completes it gives Complete with all the bits") {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bulk str".getBytes, 0)
    val inputVector   = "ing\r\n".getBytes
    val expected      = "$16\r\nTest bulk string\r\n".getBytes
    assertEquals(nonEmptyFrame.append(inputVector), CompleteFrame(expected))
  }

  test(
    "Appending to a non empty Bulk frame a bit vector that doesn't complete it gives Incomplete with the correct partial and the correct missing count"
  ) {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bul".getBytes, 0)
    val inputVector   = "k str".getBytes
    val expected      = "$16\r\nTest bulk str".getBytes
    assertEquals(nonEmptyFrame.append(inputVector), IncompleteFrame(expected, 5))
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple messages all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bulk s".getBytes, 0)
    val inputVector   = "tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(3)(CompleteFrame("$16\r\nTest bulk string\r\n".getBytes)),
        Array.empty
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple messages with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame("$16\r\nTest bulk s".getBytes, 0)
    val inputVector   = "tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(2)(CompleteFrame("$16\r\nTest bulk string\r\n".getBytes)),
        "$16\r\nTest bulk".getBytes
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame("$-".getBytes, 0)
    val inputVector   = "1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame("$-1\r\n".getBytes)),
        Array.empty
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple null bulk with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame("$-".getBytes, 0)
    val inputVector   = "1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(4)(CompleteFrame("$-1\r\n".getBytes)),
        "$".getBytes
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame("$21\r\nTest bulk s".getBytes, 0)
    val inputVector =
      "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector(
          CompleteFrame("$21\r\nTest bulk string 1 11\r\n".getBytes),
          CompleteFrame("$17\r\nTest bulk string2\r\n".getBytes),
          CompleteFrame("$20\r\nTest bulk string 3 1\r\n".getBytes),
          CompleteFrame("$19\r\nTest bulk string 40\r\n".getBytes)
        ),
        "$18\r\nTest bulk".getBytes
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne where the call to complete should give a vector with the complete ones in the original order"
  ) {
    val nonEmptyFrame = IncompleteFrame("$21\r\nTest bulk s".getBytes, 0)
    val inputVector =
      "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame("$21\r\nTest bulk string 1 11\r\n".getBytes),
            CompleteFrame("$17\r\nTest bulk string2\r\n".getBytes),
            CompleteFrame("$20\r\nTest bulk string 3 1\r\n".getBytes),
            CompleteFrame("$19\r\nTest bulk string 40\r\n".getBytes)
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }
}
