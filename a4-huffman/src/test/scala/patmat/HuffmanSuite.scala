package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  // Part 1
  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  // Part 2
  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times hello world") {
    val freqLst = times(string2Chars("helloworld"))
    assertEquals(freqLst, List(('h', 1), ('e', 1), ('l', 3), ('o', 2), ('w', 1), ('r', 1), ('d', 1)))
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    assertEquals(singleton(List(Leaf('e', 1))), true)
    assertEquals(singleton(List(
      Fork(
        Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), 
        Leaf('x', 4), 
        List('e', 't', 'x'), 
        7
      )
    )), true)
    assertEquals(singleton(List()), false)
    assertEquals(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))), false)
  }  

  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until") {
    val leaflist = List(Leaf('a', 2), Leaf('b', 3), Leaf('d', 4))
    val t2 = Fork(Leaf('d',4), Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), List('d','a','b'), 9)
    val res = until(singleton, combine)(leaflist)
    assertEquals(res, List(t2))
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
      assertEquals(decode(t2, encode(t2)("addba".toList)), "addba".toList)
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees:
      assertEquals(decode(t1, quickEncode(t1)("ab".toList)), "ab".toList)
      assertEquals(decode(t2, quickEncode(t2)("addba".toList)), "addba".toList)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
