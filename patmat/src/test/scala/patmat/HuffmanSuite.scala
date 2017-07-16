package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of some leaf list with insert last") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(combine(leaflist) === List(Leaf('x', 2), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3)))
  }

  test("combine fork and leaf") {
    val leafList = List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('z', 5))
    assert(combine(leafList) === List(Leaf('z', 5), Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("create code tree and decode") {
    val text = "abc".toList
    val codeTree = createCodeTree(text)
    assert(decode(codeTree, List(0, 1, 0, 1, 1)) === "cba".toList)
  }

  test("create code tree and decode 000") {
    val text = "abc".toList
    val codeTree = createCodeTree(text)
    assert(decode(codeTree, List(0, 0, 0)) === "ccc".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a somewhat longer text") {
    new TestTrees {
      val text = "this is somewhat longer text"
      val codeTree = createCodeTree(text.toList)
      val encoded = encode(codeTree)(text.toList)
      val decoded = decode(codeTree, encoded)
      assert(decoded === text.toList)
    }
  }

  test("times of char list") {
    new TestTrees {
      def list = List('a', 'b', 'a', 'c', 'a', 'c')

      def result = times(list)

      assert(result === List(('b', 1), ('a', 3), ('c', 2)))
    }
  }

  test("test decoded secret") {
    println(decodedSecret)
  }

  test("combine of singleton or Nill") {
    new TestTrees {
      assert(combine(List(t1)) === List(t1), "Combine of singleton")
      assert(combine(List()) === List(), "Combine of nill")
    }
  }

  test("test code table and code bits") {
    val tree = createCodeTree("hello".toList)
    val codeTable = convert(tree)
    println(codeTable)
    assert(codeBits(codeTable)('h') === List(0, 1))
  }

  test("test quick encode") {
    val tree = createCodeTree("hello".toList)
    assert(quickEncode(tree)("hl".toList) === List(0, 1, 1, 1))
  }


}
