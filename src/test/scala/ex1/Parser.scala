package ex1

import ex1.Parsers.charParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFlatSpec with Matchers:

  def parser = new BasicParser(Set('a', 'b', 'c'))

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

  // structural type composition
  def parserNTCNE =
    new BasicParser(Set('X', 'Y', 'Z'))
      with NotTwoConsecutive[Char]
      with NonEmpty[Char]

  def sparser: Parser[Char] = "abc".charParser()

  "BasicParser" should "accept valid sequences" in {
    parser.parseAll("aabc".toList) shouldBe true
  }

  it should "reject sequences containing invalid characters" in {
    parser.parseAll("aabcdc".toList) shouldBe false
  }

  it should "accept the empty sequence" in {
    parser.parseAll("".toList) shouldBe true
  }

  "NonEmptyParser" should "accept valid non-empty sequences" in {
    parserNE.parseAll("0101".toList) shouldBe true
  }

  it should "reject sequences with invalid characters" in {
    parserNE.parseAll("0123".toList) shouldBe false
  }

  it should "reject the empty sequence" in {
    parserNE.parseAll(List()) shouldBe false
  }

  "NotTwoConsecutiveParser" should "accept sequences without consecutive duplicates" in {
    parserNTC.parseAll("XYZ".toList) shouldBe true
  }

  it should "reject sequences with consecutive duplicates" in {
    parserNTC.parseAll("XYYZ".toList) shouldBe false
  }

  it should "accept the empty sequence" in {
    parserNTC.parseAll("".toList) shouldBe true
  }

  "Parser with NonEmpty and NotTwoConsecutive" should "accept valid sequences" in {
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
  }

  it should "reject consecutive duplicates" in {
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
  }

  it should "reject the empty sequence" in {
    parserNTCNE.parseAll("".toList) shouldBe false
  }

  "String extension parser" should "behave like BasicParser" in {
    sparser.parseAll("aabc".toList) shouldBe true
  }

  it should "reject invalid characters" in {
    sparser.parseAll("aabcdc".toList) shouldBe false
  }

  it should "accept empty input" in {
    sparser.parseAll("".toList) shouldBe true
  }