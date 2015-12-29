package net.orionelenion.dictionary

import scala.collection.mutable.{Map,HashMap}
import scala.collection.immutable
import scala.io.Source
import scala.util.Random

class Dictionary( fileMaybe: Option[String] ) {

  var occurrences: Map[Char, Int] = new HashMap[Char, Int]()
  var chances: immutable.Map[Option[Char], immutable.Map[Option[Char], Int]] = new immutable.HashMap[Option[Char], immutable.Map[Option[Char], Int]]()
  val random: Random = new Random( System.currentTimeMillis() )

  def this() {
    this( None )
  }

  def this( file: String ) {
    this( Option.apply( file ) )
  }

  def increment(letter: Char) = this.occurrences.update(letter, 1 + this.occurrences.getOrElse(letter, 0))
  def getOccurrence(letter: Char) = this.occurrences.getOrElse(letter, 0)

  def incrementChances( previous: Option[Char], current: Option[Char] ): Unit = {
    val chancesAfter = this.chances.getOrElse( previous, new immutable.HashMap[Option[Char], Int]())
    this.chances = this.chances.+(( previous, chancesAfter.+(( current, 1 + chancesAfter.getOrElse( current, 0 ))) ))
  }
  def incrementChances( previous: Char, current: Char ): Unit = this.incrementChances( Option.apply( previous ), Option.apply( current ) )
  def incrementChances( previous: Char, current: Option[Char] ): Unit = this.incrementChances( Option.apply( previous ), current )
  def incrementChances( previous: Option[Char], current: Char ): Unit = this.incrementChances( previous, Option.apply( current ) )
  def getChance( previous: Option[Char], current: Option[Char] ): Int = this.chances.getOrElse( previous, new immutable.HashMap[Option[Char], Int]() ).getOrElse( current, 0 )
  def getChance( previous: Option[Char], current: Char ): Int = this.getChance( previous, Option.apply( current ) )
  def getChance( previous: Char, current: Option[Char] ): Int = this.getChance(Option.apply(previous), current)
  def getChance( previous: Char, current: Char ): Int = this.getChance( Option.apply( previous ), Option.apply( current ) )

  def getLetterAfter( letterMaybe: Option[Char]): Option[Char] = {
    val chancesAfter = this.chances.getOrElse( letterMaybe, new immutable.HashMap[Option[Char], Int]() )
    val sum = chancesAfter.values.sum
    val result = this.random.nextInt( sum )
    var pass = 0
    for (after <- chancesAfter.keys) {
      pass += chancesAfter.get(after).get
      if( pass >= result ) {
        return after
      }
    }
    return None
  }
  def getLetterAfter( letter: Char ): Option[Char] = getLetterAfter( Option.apply( letter ) )

  def getWord(): String = {
    var word = ""
    var nextLetterMaybe: Option[Char] = getLetterAfter( None )
    while( nextLetterMaybe != None ) {
      word += nextLetterMaybe.get
      nextLetterMaybe = getLetterAfter( nextLetterMaybe )
    }
    return word
  }

  fileMaybe match {
    case Some( file ) => {
      for( line <- Source.fromFile( file ).getLines() ) {
        line.foreach( increment )
        var previous: Option[Char] = None
        for( letter <- line ) {
          this.incrementChances( previous, letter )
          previous = Option.apply( letter )
        }
        this.incrementChances( previous, None )
      }
    }
    case None =>
  }
}

object Dictionary {
  def main( args: Array[String] ): Unit = {
    val dictionary = new Dictionary( "test/assets/french.txt" )
    for( index <- 0 until 10 ) {
      println(dictionary.getWord())
    }
  }
}
