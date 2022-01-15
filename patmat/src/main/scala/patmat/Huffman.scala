package patmat

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface:

  // Part 1: Basics
  def weight(tree: CodeTree): Int =  tree match // tree match ...
    case Leaf(_, weight) => weight
    case Fork(_, _, _, weight) => weight

  def chars(tree: CodeTree): List[Char] = tree match // tree match ...
    case Leaf(char, _) => char :: Nil // or List(char)
    case Fork(left, right, chars, _) => chars

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = 
    def incFreqByOne(char: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match
      case pair :: xs => if (char == pair._1) (char, pair._2 + 1) :: xs else pair :: incFreqByOne(char, xs) 
      case Nil => (char, 1) :: Nil
    
    def getTimes(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = chars match
      case char :: xs =>  getTimes(xs, incFreqByOne(char, acc))
      case Nil => acc // Not Nil！

    getTimes(chars, Nil)

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  // Approach 1: insertion sort: O(N^2)
  // invariant: the tail is sorted
  // def insert(x: (Char, Int), freqs: List[(Char, Int)]): List[(Char, Int)] = freqs match
  //   case List() => List(x)
  //   case y :: ys => if x._2 <= y._2 then x :: insert(y, ys) else y :: insert(x, ys)
  
  // def makeOrderedPairList(freqs: List[(Char, Int)]): List[(Char, Int)] = freqs match 
  //   case List() => List()
  //   case x :: xs => insert(x, xs)

  // def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  //   makeOrderedPairList(freqs).map(pair => Leaf(pair._1, pair._2))

  // Approach 2: merge sort: O(Nlog(N))
  def msort[T](lst: List[T])(lt: (T, T) => Boolean): List[T] =
    val mid = lst.length / 2
    if mid == 0 then lst
    else 
      def merge(xs: List[T], ys: List[T]): List[T]= (xs, ys) match
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if lt(x, y) then x :: merge(xs1, ys) else y :: merge(xs, ys1)
      val (first, second) = lst.splitAt(mid)
      merge(msort(first)(lt), msort(second)(lt))
      
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    msort(freqs)((p1, p2) => p1._2 <= p2._2).map(pair => Leaf(pair._1, pair._2))

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match
    case Nil => false
    case x :: Nil => true
    case x :: y :: ys => false
    
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match
    case List() => trees
    case x :: Nil => trees
    case x :: y :: Nil => 
      List(makeCodeTree(x, y))
    case x :: y :: z :: zs =>
      val combined = makeCodeTree(x, y)
      if (weight(combined) <= weight(z)) combined :: z :: zs
      else z :: combine(combined :: zs)

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = 
    if done(trees) then trees else until(done, merge)(merge(trees))

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = 
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head


  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = 
    def decodeWithAcc(subtree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = subtree match
      // or t: Leaf, in case no need of fields in Leaf
      case Leaf(char, _) => bits match 
        case Nil => acc :+ char // append an elem to the lst
        case _ => decodeWithAcc(tree, bits, acc :+ char)
      case Fork(left, right, _, _) => 
        decodeWithAcc(if bits.head == 0 then left else right, bits.tail, acc)
        // if bits.head == 0 then decodeWithAcc(left, bits.tail, acc) else decodeWithAcc(right, bits.tail, acc)
    decodeWithAcc(tree, bits, Nil)

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = 
    def getBits(subtree: CodeTree)(char: Char): List[Bit] = subtree match
      case Leaf(_, _) => Nil
      case Fork(left, right, _, _) =>
        if chars(left).contains(char) then 0 :: getBits(left)(char) else 1 :: getBits(right)(char)

    def encodeWithAcc(subtree: CodeTree, acc: List[Bit])(subText: List[Char]): List[Bit] = subText match
      case Nil => acc
      case x :: xs => encodeWithAcc(subtree, acc ++ getBits(subtree)(x))(xs)

    encodeWithAcc(tree, Nil)(text)
    // text flatMap getBits(tree, Nil)


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case Nil => Nil
    case x :: xs => if x._1 == char then x._2 else codeBits(xs)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match
    case Leaf(char, _) => List((char, Nil))
    case Fork(left, right, _, _) => mergeCodeTables(convert(left), convert(right))

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = 
    // val lt = (x: (Char, List[Bit]), y: (Char, List[Bit])) => x._1 < y._1
    // val sortedA = msort(a)(lt)
    // val sortedB = msort(b)(lt)
    def mergeCodeTablesWithAcc(t: CodeTable, prefix: Bit, acc: CodeTable) : CodeTable = t match
        case Nil => acc
        case x :: xs => mergeCodeTablesWithAcc(t.tail, prefix, acc :+ (x._1, prefix +: x._2))
    mergeCodeTablesWithAcc(a, 0, mergeCodeTablesWithAcc(b, 1, Nil))

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = 
    val table = convert(tree) // or val converter = convert(tree)_    converter(x) in case x :: xs
    def quickEncodeWithAcc(text: List[Char], acc: List[Bit]) : List[Bit] = text match 
      case Nil => acc
      case x :: xs => quickEncodeWithAcc(xs, acc ++ codeBits(table)(x))
    quickEncodeWithAcc(text, Nil)

object Huffman extends Huffman
