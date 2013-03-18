package com.github.reggert.sgitl.plumbing.objects

trait GitTag extends HeaderMessageObject 
{
	final override def objectType = ObjectType.Tag
	
	def name = headers.find(_._1 == "tag").get._2
	
	def tagger = headers.find(_._1 == "tagger").get._2
	
	def taggedObjectType : ObjectType = ObjectType.unapply(headers.find(_._1 == "type").get._2).get
	
	def taggedObjectId : SHA1 = SHA1.FromString(headers.find(_._1 == "object").get._2)
}