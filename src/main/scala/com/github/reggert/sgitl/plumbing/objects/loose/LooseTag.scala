package com.github.reggert.sgitl.plumbing.objects.loose
import java.nio.charset.Charset
import com.github.reggert.sgitl.plumbing.objects.GitTag
import com.github.reggert.sgitl.plumbing.objects.ObjectType
import com.github.reggert.sgitl.plumbing.objects.SHA1

final case class LooseTag private[sgitl] (override val headers : Seq[(String, String)], override val message : String) 
	extends LooseObject with GitTag 
{
	def this(taggedObjectId : SHA1, taggedObjectType : ObjectType, name : String, tagger : String, message : String) = this(
			headers = Seq(
					"object" -> taggedObjectId.toString, 
					"type"-> taggedObjectType.toString, 
					"tag" -> name, 
					"tagger" -> tagger
				), 
			message = message
		)
	
	def withEncoding(charset : Charset) : LooseTag = 
		new LooseTag(headers.filterNot(_._1 == "encoding") :+ ("encoding", charset.name()), message)
	
	def withDefaultEncoding : LooseTag = 
		new LooseTag(headers.filterNot(_._1 == "encoding"), message)
}