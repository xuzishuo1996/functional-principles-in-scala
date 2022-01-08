package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  /* ======= 1. contains tests ======= */
  // dummy test
  test("dummy contains correct") {
    assert(contains(x => true, 100))
  }

  test("contains correct") {
    assert(!contains(x => (x < 100), 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /* ======= 2. singletonSet tests ======= */
  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  /* ======= 3. union tests ======= */
  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  /* ======= 4. intersect tests ======= */
  test("intersect contains only common elements") {
    new TestSets:
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = intersect(s12, s23);
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 1), "Union 1")
      assert(!contains(s, 3), "Union 3")
  }

  /* ======= 5. diff tests ======= */
  test("diff contains respective unique elements") {
    new TestSets:
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = diff(s12, s23);
      assert(!contains(s, 2), "Union 2")
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 3), "Union 3")
  }

  /* ======= 5. filter tests ======= */
  test("filter contains required elements") {
    new TestSets:
      val unionSet = union(s1, union(s2, s3))
      val s = filter(unionSet, x => x <= 1);
      assert(contains(s, 1), "elem 1")
      assert(!contains(s, 2), "elem 2")
      assert(!contains(s, 3), "elem 3")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
