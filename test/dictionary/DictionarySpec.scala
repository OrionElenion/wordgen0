package net.orionelenion.dictionary

import net.orionelenion.UnitSpec

class DictionarySpec extends UnitSpec {

  "A dictionary" must "give back the correct number of occurrences of a letter" in {
    val dictionary = new Dictionary()
    assert(dictionary.getOccurrence('l') === 0, "l should initially be present 0 times")
    dictionary.increment('l')
    assert(dictionary.getOccurrence('l') === 1, "l should now be present 1 times")
  }

  it must "give back the correct number of chances of a letter knowing the previous letter" in {
    val dictionary = new Dictionary()
    assert(dictionary.getChance('a', 'b') === 0, "ab should initially be present 0 times")
    dictionary.incrementChances('a', 'b')
    assert(dictionary.getChance('a', 'b') === 1, "ab should now be present 1 times")
    assert(dictionary.getChance('a', 'c') === 0, "ac should be present 0 times")
    assert(dictionary.getChance('c', 'b') === 0, "cb should be present 0 times")
  }

  it must "build correctly when given a file" in {
    val dictionary = new Dictionary( "test/assets/french.txt" )
    assert(dictionary.getOccurrence('a') === 306169, "a should be present  times in the test dictionary")
    assert(dictionary.getChance('a', 'b') === 7971, "ab should be present 7971 times in the test dictionary")
    assert(dictionary.getChance(None, 'a') === 24295, "^a should be present 24295 times in the test dictionary")
    assert(dictionary.getChance('a', None) === 13074, "a$ should be present 13074 times in the test dictionary")
  }

  it must "give the only possible letter" in {
    val dictionary = new Dictionary()
    dictionary.incrementChances( None, 'a' )
    dictionary.incrementChances( None, 'a' )
    dictionary.incrementChances( None, 'a' )
    dictionary.incrementChances( None, 'a' )
    dictionary.incrementChances( 'a', 'b' )
    dictionary.incrementChances( 'b', None )
    assert(dictionary.getLetterAfter(None) === Option.apply('a'), "'a' should be the only possible letter at the beginning")
    assert(dictionary.getLetterAfter(Option.apply('a')) === Option.apply('b'), "'b' should be the only possible letter after an 'a'")
    assert(dictionary.getLetterAfter('b') === None, "The end of the word should be the only possible choice after a 'b'")
    assert(dictionary.getWord() === "ab", "There should be only 1 possible word")
  }
}
