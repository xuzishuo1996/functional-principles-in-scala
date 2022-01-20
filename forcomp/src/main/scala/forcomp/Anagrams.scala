package forcomp

import scala.io.{ Codec, Source }

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   * https://www.scala-lang.org/api/2.13.8/scala/collection/StringOps.html
   * https://www.geeksforgeeks.org/scala-map-tolist-method-with-example/
   * https://www.scala-lang.org/api/2.13.8/scala/collection/immutable/List.html?search=sortWith
   */
  def wordOccurrences(w: Word): Occurrences = 
    w.groupBy(_.toLower).map((k, v) => (k, v.length)).toList.sortWith((p1, p2) => p1._1 < p2._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)
    // val wordsOccurrences =
    //   for w <- s
    //   yield wordOccurrences(w)
    // val emptyOccurrences = ('a' to 'z').map(c => (c, 0)).toList
    // wordsOccurrences.foldLeft(emptyOccurrences)()
 
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary groupBy wordOccurrences
    // dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = 
    dictionaryByOccurrences.get(wordOccurrences(word)).get
    // // word exist, so occurrences list is not None. No need
    // dictionaryByOccurrences.get(wordOccurrences(word)) match
    //   case Some(lst) => lst
    //   case None => Nil

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   * 
   *  Note: inter - list order does not matter, but inner list order matters
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match
    case Nil => List(List())
    case (char, freq) :: occurrences =>
      for res <- combinations(occurrences); i <- 0 to freq
      yield
        if i == 0 then res
        else ((char, i) +: res) //.sortBy(_._1) // if res :+ (char, i), then out of order, need sort
  // def combinations(occurrences: Occurrences): List[Occurrences] = 
  //   def combinationsHelper(occurrences: Occurrences): List[Occurrences] = occurrences match
  //     case Nil => Nil  // case Nil equals case List()
  //     // case x :: Nil => List(List(), occurrences)
  //     case x :: y => 
  //       val withoutFirst = combinations(y)
  //       val onlyFirst = 
  //         for i <- 1 to x._2
  //         yield List((x._1, i))
  //       val withFirst = 
  //         for i <- 1 to x._2
  //         yield withoutFirst.map(lst => (x._1, i) +: lst)
  //       (withoutFirst ++ onlyFirst ++ withFirst.flatten)
  //   List() +: combinationsHelper(occurrences)
  
  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   * 
   *  https://www.scala-lang.org/api/2.13.8/scala/collection/Seq.html
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    (x ++ y.map(c => (c._1, -c._2)))
    .groupBy(_._1)  // transform to Mao according to the Char Map(('a', List(('a', 1), ('a', -1))), ('a', List(('b', 2), ('b', -1))))
    .mapValues(_.map(_._2).sum) // map the value of each entry, i.e., each List
    .toList // from Map to List
    .filter(_._2 != 0)  // remove chars with zero freq
    .sorted

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = 
    def sentenceAnagramsHeler(occurrences: Occurrences): List[Sentence] = occurrences match
      case Nil => List(Nil)
      case _ => 
        (for curr <- combinations(occurrences); word <- dictionaryByOccurrences.getOrElse(curr, List())
          if word.length > 0
          yield sentenceAnagramsHeler(subtract(occurrences, curr)).map(word :: _)
        ).flatten
    sentenceAnagramsHeler(sentenceOccurrences(sentence))

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
