package com.github.reggert.sgitl.plumbing.objects

import scala.collection.Traversable
import java.nio.charset.Charset

final case class LooseTag private[sgitl] (override val headers : Seq[(String, String)], override val message : String) 
	extends LooseObject with HeaderMessageObject 
{
	override def objectType = ObjectType.Tag
	
	def this(taggedObjectId : SHA1, taggedObjectType : ObjectType, name : String, tagger : String, message : String) = this(
			headers = Seq(
					"object" -> taggedObjectId.toString, 
					"type"-> taggedObjectType.toString, 
					"tag" -> name, 
					"tagger" -> tagger
				), 
			message = message
		)
	
	def name = headers.find(_._1 == "tag").get._2
	
	def tagger = headers.find(_._1 == "tagger").get._2
	
	def taggedObjectType : ObjectType = ObjectType.unapply(headers.find(_._1 == "type").get._2).get
	
	def taggedObjectId : SHA1 = SHA1.FromString(headers.find(_._1 == "object").get._2)
	
	def withEncoding(charset : Charset) : LooseTag = 
		new LooseTag(headers.filterNot(_._1 == "encoding") :+ ("encoding", charset.name()), message)
	
	def withDefaultEncoding : LooseTag = 
		new LooseTag(headers.filterNot(_._1 == "encoding"), message)
}