package com.github.reggert.sgitl.plumbing.objects

import scala.annotation.tailrec
import java.util.NoSuchElementException
import java.io.InputStream
import java.security.{DigestInputStream,MessageDigest}

/**
 * Provides a Scala-friendly wrapper around the SHA-1 implementation that is 
 * included with the JRE.
 */
final class SHA1 private(override val toString : String, val toBytes : IndexedSeq[Byte]) 
	extends Equals 
{
	require(toBytes.size == SHA1.AsBytes.ExpectedLength, "toBytes.size =" + toBytes.size)
	require(toString.length == SHA1.AsString.ExpectedLength, "toString.length = " + toString.length)
	
	def this(bytes : IndexedSeq[Byte]) = this(
			SHA1.HexBytes.apply(bytes),
			bytes
		)
			
	def this(hashString : String) = this(
			hashString, 
			SHA1.HexBytes.unapply(hashString).getOrElse
				(throw new IllegalArgumentException("Invalid hash string"))
		)
  
	def canEqual(other: Any) = other.isInstanceOf[SHA1]
  
	override def equals(other: Any) = other match 
	{
		case that : com.github.reggert.sgitl.plumbing.objects.SHA1 if that.canEqual(this) => 
			toBytes == that.toBytes
		case _ => false
	}
  
	override def hashCode() = 
	{
		val prime = 41
		prime + toBytes.hashCode
	}
}

/**
 * Provides a Scala-friendly wrapper around the SHA-1 implementation that is 
 * included with the JRE.
 */
object SHA1 
{
	private def newDigest = MessageDigest.getInstance("SHA-1")
	
	def hashBytes(input : Traversable[Byte]) =
	{
		val md = newDigest
		input.foreach(md.update)
		new SHA1(md.digest())
	}
	
	
	private object HexDigit
	{
		private val applyTable = Map(
				0 -> '0',
				1 -> '1',
				2 -> '2',
				3 -> '3',
				4 -> '4',
				5 -> '5',
				6 -> '6',
				7 -> '7',
				8 -> '8',
				9 -> '9',
				0xa -> 'a',
				0xb -> 'b',
				0xc -> 'c',
				0xd -> 'd',
				0xe -> 'e',
				0xf -> 'f'
			)
		private val unapplyTable = Map(
				'0' -> 0, 
				'1' -> 1, 
				'2' -> 2, 
				'3' -> 3, 
				'4' -> 4, 
				'5' -> 5, 
				'6' -> 6, 
				'7' -> 7, 
				'8' -> 8,
				'9' -> 9,
				'a' -> 0xa,
				'A' -> 0xa,
				'b' -> 0xb,
				'B' -> 0xb,
				'c' -> 0xc,
				'C' -> 0xc,
				'd' -> 0xd,
				'D' -> 0xd,
				'e' -> 0xe,
				'E' -> 0xe,
				'f' -> 0xf,
				'F' -> 0xf
			)
		
		
		def apply(n : Int) : Char = applyTable.getOrElse(
				n, 
				throw new IllegalArgumentException("Integer out of range of hexadecimal digit: " + n)
			) 
		
		def unapply(c : Char) : Option[Int] = unapplyTable.get(c)
	}
	
	
	private object HexDigits
	{
		def apply(ns : Seq[Int]) = (ns map (HexDigit.apply)).mkString 
		
		def unapply(digits : String) : Option[Seq[Int]] =
			(digits.map(HexDigit.unapply) :\ Some(List.empty[Int]).asInstanceOf[Option[Seq[Int]]]) {
				case (None, _) => None
				case (_, None) => None
				case (left, right) => Some(left.get +: right.get)
			}
	}
	
	
	private object HexBytes
	{
		def apply(bytes : Seq[Byte]) = 
			HexDigits(bytes map (_ & 0xff) flatMap {n => Seq(n >> 4, n & 0xf)})
		
		def unapply(s : String) : Option[IndexedSeq[Byte]] = 
			if (s.length() % 2 != 0) 
				None
			else
				HexDigits.unapply(s) map { digits =>
					for (Seq(high, low) <- digits.grouped(2)) 
						yield (((high << 4) | low).toByte)
				} map (_.toIndexedSeq)
	}
	
	
	
	object AsBytes
	{
		val ExpectedLength = 20
		
		def apply(sha1 : SHA1) = sha1.toBytes
		
		def unapply(bytes : IndexedSeq[Byte]) : Option[SHA1] = 
			if (bytes.size == ExpectedLength) Some(new SHA1(bytes)) else None
	}
	
	object AsString
	{
		val ExpectedLength = AsBytes.ExpectedLength * 2
		
		def apply(sha1 : SHA1) = sha1.toString
		
		def unapply(s : String) : Option[SHA1] = s match
		{
			case _ if s.length != ExpectedLength => None
			case HexBytes(bytes) => Some(new SHA1(s, bytes))
			case _ => None
		}
	}
}