package com.github.reggert.sgitl.plumbing.objects

import scala.collection.Traversable
import java.nio.charset.Charset

final class LooseCommit private(val headers : Seq[(String, String)], val message : String) extends LooseObject 
{
	override def objectType = ObjectType.Commit
	
	def encoding = headers.find(header => header._1 == "encoding")
		.map(header => new CharacterSet(Charset.forName(header._2)))
		.getOrElse(UTF8)
		
	override def content = 
		UTF8(
				((StringBuilder.newBuilder /: headers) 
					{(builder, header) => builder ++= header._1 += ' ' ++= header._2 += '\n'} 
				+= '\n').toString
			) ++ encoding(message)
		
}