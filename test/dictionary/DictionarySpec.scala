package net.orionelenion.dictionary

import net.orionelenion.UnitSpec
import scala.collection.immutable.HashMap

class DictionarySpec extends UnitSpec {

  "A dictionary" must "extract a context from a sequence of characters of the right length" in {
    val dictionary = new Dictionary( 3 )
    assert( dictionary.getContextFromCharSequence( "abcd" ) == Vector( Option('a'), Option('b'), Option('c')), "The 3-sized context of \"abcd\" should be \"abc\"" )
  }

  it must "extract a context from a too long sequence of characters" in {
    val dictionary = new Dictionary( 3 )
    assert( dictionary.getContextFromCharSequence( "xyzabcd" ) == Vector( Option('a'), Option('b'), Option('c')), "The 3-sized context of \"000abcd\" should be \"abc\"" )
  }

  it must "extract a context from a too short sequence of characters" in {
    val dictionary = new Dictionary( 3 )
    assert( dictionary.getContextFromCharSequence( "ab" ) == Vector( None, None, Option('a')), "The 3-sized context of \"ab\" should be \"a\"" )
  }

  it must "index a sequence correctly" in {
    val dictionary = new Dictionary( 3 )
    assert( dictionary.parentMap.size == 0, "Before indexing any sequence, a dictionary's parent map should have a size of 0")
    dictionary.indexSequence( "abcd" )
    assert( dictionary.parentMap.size == 1, "After indexing 1 sequence, a dictionary's parent map should have a size of 1")
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('c') ), new HashMap[Option[Char], Int] ).getOrElse( Option('d'), 0 ) == 1, "After indexing the \"abcd\" sequence, the chance of 'd' knowing \"abc\" should be 1")
    dictionary.indexSequence( "abce" )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('c') ), new HashMap[Option[Char], Int] ).getOrElse( Option('d'), 0 ) == 1, "After indexing the \"abcd\" sequence, the chance of 'd' knowing \"abc\" should be 1")
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('c') ), new HashMap[Option[Char], Int] ).getOrElse( Option('e'), 0 ) == 1, "After indexing the \"abce\" sequence, the chance of 'e' knowing \"abc\" should be 1")
    assert( dictionary.parentMap.size == 1, "After indexing 2 sequences with the same context, a dictionary's parent map should have a size of 1")
    dictionary.indexSequence( "abfg" )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('c') ), new HashMap[Option[Char], Int] ).getOrElse( Option('d'), 0 ) == 1, "After indexing the \"abcd\" sequence, the chance of 'd' knowing \"abc\" should be 1")
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('c') ), new HashMap[Option[Char], Int] ).getOrElse( Option('e'), 0 ) == 1, "After indexing the \"abce\" sequence, the chance of 'e' knowing \"abc\" should be 1")
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('f') ), new HashMap[Option[Char], Int] ).getOrElse( Option('g'), 0 ) == 1, "After indexing the \"abfg\" sequence, the chance of 'g' knowing \"abf\" should be 1")
    assert( dictionary.parentMap.size == 2, "After indexing 3 sequences with the 2 different contexts, a dictionary's parent map should have a size of 2")
  }

  it must "index a word correctly" in {
    val dictionary = new Dictionary( 3 )
    assert( dictionary.parentMap.size == 0, "Before indexing any sequence, a dictionary's parent map should have a size of 0")
    dictionary.indexWord( "foobar" )
    assert( dictionary.parentMap.size == 7, "After indexing the word \"foobar\", a dictionary's parent map should have a size of 7")
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( None, None, None ), new HashMap[Option[Char], Int] ).size == 1, "After indexing 1 word, the inner map for a starting context should have a size of 1")
    dictionary.indexWord( "baz" )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( None, None, None ), new HashMap[Option[Char], Int] ).size == 2, "After indexing 2 words starting with different letters, the inner map for a starting context should have a size of 2")
  }

  it must "build correctly when given a file" in {
    val dictionary = new Dictionary( "test/assets/french.txt", 4 )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('c'), Option('t'), Option('i'), Option('o') ), new HashMap[Option[Char], Int] ).getOrElse( Option('n'), 0 ) == 1061, "ction should be present 1061 times in the test dictionary" )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( None, None, None, None ), new HashMap[Option[Char], Int] ).getOrElse( Option('a'), 0 ) == 24295, "^a should be present 24295 times in the test dictionary" )
    assert( dictionary.parentMap.getOrElse( Vector[Option[Char]]( Option('a'), Option('b'), Option('l'), Option('e') ), new HashMap[Option[Char], Int] ).getOrElse( None, 0 ) == 722, "able$ should be present 722 times in the test dictionary" )
  }
}
