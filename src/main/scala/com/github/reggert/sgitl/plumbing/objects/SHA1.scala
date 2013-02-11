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
	require(toBytes.size == SHA1.HashBytesLength, "toBytes.size =" + toBytes.size)
	require(toString.length == SHA1.HashStringLength, "toString.length = " + toString.length)
	
	def this(bytes : IndexedSeq[Byte]) =
		this(SHA1.bytesToString(bytes), bytes)
			
	def this(hashString : String) =
		this(hashString, SHA1.stringToBytes(hashString).toIndexedSeq)
  
	def canEqual(other: Any) =
		other.isInstanceOf[com.github.reggert.sgitl.plumbing.objects.SHA1]
  
	override def equals(other: Any) = other match 
	{
		case that : com.github.reggert.sgitl.plumbing.objects.SHA1 => 
			that.canEqual(SHA1.this) && toBytes == that.toBytes
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
	
	def bytesToString(bytes : Traversable[Byte]) =
		bytes.map(_ & 0xff).map(_.toHexString).map(s => if (s.length < 2) "0" + s else s).mkString 
		
	val HashBytesLength = 20
	val HashStringLength = HashBytesLength * 2
	private val HexDigitMap = Map(
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
		).withDefault(c => throw new IllegalArgumentException("Invalid character in hash string: " + c))
	
	def stringToBytes(hashString : String) = 
		condenseBytes(hashString.toStream.map(HexDigitMap))
	
	private def condenseBytes(digits : Stream[Int]) : Stream[Byte] = digits match
	{
		case high #:: low #:: rest => (((high << 4) | low).toByte) #:: condenseBytes(rest)
		case _ => require(digits.isEmpty, "Odd number of digits"); Stream.Empty
	}
	
	object DigestBytes
	{
		def apply(sha1 : SHA1) = sha1.toBytes
		
		def unapply(bytes : IndexedSeq[Byte]) = 
			if (bytes.size == HashBytesLength) Some(new SHA1(bytes)) else None
	}
}