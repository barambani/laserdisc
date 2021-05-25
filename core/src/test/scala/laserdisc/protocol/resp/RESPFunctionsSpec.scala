package laserdisc
package protocol
package resp

import laserdisc.protocol.resp.RESPWireState.{Complete, CompleteWithRemainder, Incomplete, MissingBytes}

final class RESPFunctionsSpec extends BaseSpec {

  test("A RESP codec checking the state of a bit vector with the size prefix not complete gives IncompleteVector") {
    assertEquals(RESPWireState.stateOf("$2362".getBytes), Incomplete)
  }

  test("A RESP codec checking the state of a bit vector with only the data type selector gives IncompleteVector") {
    assertEquals(RESPWireState.stateOf("$".getBytes), Incomplete)
  }

  test("A RESP codec checking the state of a bit vector that's complete gives CompleteVector") {
    assertEquals(RESPWireState.stateOf("$16\r\nTest bulk string\r\n".getBytes), Complete)
  }

  test(
    "A RESP codec checking the state of a bit vector with the size prefix complete and an incomplete payload gives MissingBits with the correct number of bits missing"
  ) {
    assertEquals(RESPWireState.stateOf("$40\r\nIncomplete test bulk string".getBytes), MissingBytes(15))
  }

  test("A RESP codec checking the state of a bit vector that represents an empty bulk gives CompleteVector") {
    assertEquals(RESPWireState.stateOf("$-1\r\n".getBytes), Complete)
  }

  test("A RESP codec checking the state of an incomplete bit vector that represents an empty bulk gives MissingBits") {
    assertEquals(RESPWireState.stateOf("$-".getBytes), Incomplete)
  }

  test(
    "A RESP codec checking the state of a bulk bit vector that contains one message complete and one not complete gives CompleteWithRemainder"
  ) {
    assertEquals(
      RESPWireState.stateOf("$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes),
      CompleteWithRemainder(
        "$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte),
        "$16\r\nTest bulk".toCharArray.map(_.toByte)
      )
    )
  }

  test("A RESP codec checking the state of a bulk bit vector that contains more than one complete messages gives CompleteWithRemainder") {
    assertEquals(
      RESPWireState.stateOf("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes),
      CompleteWithRemainder(
        "$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte),
        "$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte)
      )
    )
  }

  test("A RESP codec checking the state of a bulk bit vector that contains more than one null message gives CompleteWithRemainder") {
    assertEquals(
      RESPWireState.stateOf("$-1\r\n$-1\r\n$-1\r\n".getBytes),
      CompleteWithRemainder(
        "$-1\r\n".toCharArray.map(_.toByte),
        "$-1\r\n$-1\r\n".toCharArray.map(_.toByte)
      )
    )
  }
}
