package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset

class CharacterSet(val charset : Charset) 
{
	import Implicits._
	
	def apply(s : String) : IndexedSeq[Byte] = charset.encode(s).toIndexedSeq
	def unapply(bytes : TraversableOnce[Byte]) =
		Some(charset.newDecoder().decode(bytes).toString)
}