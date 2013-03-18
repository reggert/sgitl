package com.github.reggert.sgitl.plumbing.objects

import java.nio.CharBuffer
import java.nio.charset.{Charset,CharacterCodingException}
import com.github.reggert.sgitl.plumbing.io.Implicits

class CharacterSet(val charset : Charset) 
{
	import Implicits._
	
	@throws(classOf[CharacterCodingException])
	def apply(s : String) : IndexedSeq[Byte] =
		charset.newEncoder().encode(CharBuffer.wrap(s)).toIndexedSeq
		
	def unapply(bytes : TraversableOnce[Byte]) =
		try {Some(charset.newDecoder().decode(bytes).toString)}
		catch {case _ : CharacterCodingException => None}
}