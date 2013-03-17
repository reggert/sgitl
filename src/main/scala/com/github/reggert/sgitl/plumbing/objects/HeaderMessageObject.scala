package com.github.reggert.sgitl.plumbing.objects

import java.nio.charset.Charset

trait HeaderMessageObject extends LooseObject
{
	import HeaderMessageObject._
	
	def headers : Seq[(String, String)]
	def message : String
	
	final def encoding = encodingFromHeaders(headers)
	override final def content = EncodedContent(headers, message)
}


object HeaderMessageObject 
{
	val EOL = '\n'.toByte
	
	private[sgitl] def encodingFromHeaders(headers : Seq[(String, String)]) = 
		headers.find(header => header._1 == "encoding")
			.map(header => new CharacterSet(Charset.forName(header._2)))
			.getOrElse(UTF8)
			
	
	private[sgitl] object EncodedHeaders
	{
		def apply(headers : Seq[(String, String)], encodedMessage : Seq[Byte]) : Seq[Byte] =
			(headers.flatMap(header => EncodedHeader(header._1, header._2) :+ EOL) :+ EOL) ++ encodedMessage
		
		def unapply(encoded : Seq[Byte]) : Option[(Seq[(String, String)], Seq[Byte])] = encoded.span(_ != EOL) match
		{
			case (Seq(), EOL +: encodedMessage) => 
				Some(Seq.empty, encodedMessage)
			case (EncodedHeader(name, value), EOL +: EncodedHeaders(headers, encodedMessage)) => 
				Some(((name, value) +: headers), encodedMessage)
			case _ => None
		}
	}
	
	private[sgitl] object EncodedHeader
	{
		private val NameValue = """(\S+)\s+(\S.+)""".r
		
		def apply(name : String, value : String) : IndexedSeq[Byte] = UTF8(s"$name $value")
			
		def unapply(encoded : Seq[Byte]) : Option[(String, String)] = encoded match
		{
			case UTF8(NameValue(name, value)) => Some(name, value)
			case _ => None
		}
	}
	
	
	private[sgitl] object EncodedContent
	{
		def apply(headers : Seq[(String, String)], message : String) : Seq[Byte] = 
			EncodedHeaders(headers, encodingFromHeaders(headers)(message))
			
		def unapply(encoded : Seq[Byte]) : Option[(Seq[(String, String)], String)] = encoded match
		{
			case EncodedHeaders(headers, encodedMessage) =>
				val Encoding = encodingFromHeaders(headers)
				encodedMessage match
				{
					case Encoding(message) => Some(headers, message)
					case _ => None
				}
			case _ => None
		}
	}
}