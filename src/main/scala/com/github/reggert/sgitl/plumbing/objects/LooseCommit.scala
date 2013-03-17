package com.github.reggert.sgitl.plumbing.objects

import scala.collection.Traversable
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer

final case class LooseCommit private[sgitl] (val headers : Seq[(String, String)], val message : String) extends LooseObject 
{
	import LooseCommit._
	
	override def objectType = ObjectType.Commit
	
	def encoding = encodingFromHeaders(headers)
		
	override def content = EncodedContent(headers, message)
	
	def this(tree : SHA1, parents : Seq[SHA1], author : String, committer : String, message : String) =
		this(
				parents.map(p => ("parent", p.toString)) ++ Seq(
						("tree", tree.toString), 
						("author", author), 
						("committer", committer)
					), 
				message
			)
			
	def withEncoding(charset : Charset) : LooseCommit = 
		new LooseCommit(headers.filterNot(_._1 == "encoding") :+ ("encoding", charset.name()), message)
	
	def withDefaultEncoding : LooseCommit = 
		new LooseCommit(headers.filterNot(_._1 == "encoding"), message)
	
	def author = headers.find(_._1 == "author").get._2
	
	def committer = headers.find(_._1 == "committer").get._2
	
	def parents = headers.view.filter(_._1 == "parent").map(_._2).map(SHA1.FromString).force
	
	def tree = headers.find(_._1 == "tree").map(_._2).map(SHA1.FromString).get
}


object LooseCommit
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