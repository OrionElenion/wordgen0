package net.orionelenion.dictionary

import scala.collection.immutable._
import scala.io.Source
import scala.util.Random
import Dictionary.defaultContextSize

class Dictionary( fileMaybe: Option[String], contextSize: Int = defaultContextSize ) {

  var parentMap: Map[IndexedSeq[Option[Char]], Map[Option[Char], Int]] = new HashMap[IndexedSeq[Option[Char]], Map[Option[Char], Int]]()
  val random: Random = new Random( System.currentTimeMillis() )

  def this( file: String ) {
    this( Option.apply( file ) )
  }

  def this( file: String, contextSize: Int ) {
    this( Option.apply( file ), contextSize )
  }

  def this( contextSize: Int ) {
    this( None, contextSize )
  }

  private[dictionary] def getContextFromCharSequence( sequence: CharSequence ): IndexedSeq[Option[Char]] = {
    var context: IndexedSeq[Option[Char]] = Vector[Option[Char]]()
    val consideredSequence: CharSequence = {
      if (sequence.length > this.contextSize) {
        sequence.subSequence( sequence.length - (this.contextSize + 1), sequence.length - 1 )
      }
      else {
        sequence.subSequence( 0, sequence.length - 1 )
      }
    }
    for( char <- consideredSequence.toString ) {
      context :+= Option(char)
    }
    for( iteration <- context.size until this.contextSize ) {
      context +:= None
    }
    return context
  }

  def indexSequence( sequence: CharSequence ): Unit = {
    val context = this.getContextFromCharSequence( sequence )
    val letter = sequence.charAt( sequence.length - 1 )
    val contextMap = this.parentMap.getOrElse( context, new HashMap[Option[Char], Int]() )
    val letterOccurrences = contextMap.getOrElse( Option(letter), 0 )
    this.parentMap += (( context, contextMap + (( Option(letter) , 1 + letterOccurrences )) ))
  }

  def indexWord( word: String) = {
    var context: IndexedSeq[Option[Char]] = Vector.fill( this.contextSize ) { None }
    for( letter <- word ) {
      val contextMap = this.parentMap.getOrElse( context, new HashMap[Option[Char], Int]() )
      val letterOccurrences = contextMap.getOrElse( Option(letter), 0 )
      this.parentMap += (( context, contextMap + (( Option(letter) , 1 + letterOccurrences )) ))
      context = context.drop(1) :+ Option( letter )
    }
    val contextMap = this.parentMap.getOrElse( context, new HashMap[Option[Char], Int]() )
    val letterOccurrences = contextMap.getOrElse( None, 0 )
    this.parentMap += (( context, contextMap + (( None , 1 + letterOccurrences )) ))
  }

  override def toString(): String = {
    var output = ""
    for( ( context, suite ) <- this.parentMap ) {
      for( ( letter, chance ) <- suite ) {
        output += context + "-" + letter + ":" + chance + "\n"
      }
    }
    return output
  }

  fileMaybe match {
    case Some( file ) => {
      Source.fromFile( file ).getLines().foreach( indexWord )
    }
    case None =>
  }
}

object Dictionary {
  val defaultContextSize: Int = 4

  def main( args: Array[String] ): Unit = {
    val dictionary = new Dictionary( "test/assets/french.txt" )
    for( index <- 0 until 10 ) {
      println(dictionary)
    }
  }
}
