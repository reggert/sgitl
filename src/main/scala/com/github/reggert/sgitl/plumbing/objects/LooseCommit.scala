package com.github.reggert.sgitl.plumbing.objects

import scala.collection.Traversable
import java.nio.charset.Charset
import scala.collection.mutable.ListBuffer

final case class LooseCommit private[sgitl] (override val headers : Seq[(String, String)], override val message : String) 
	extends LooseObject with HeaderMessageObject
{
	import HeaderMessageObject._
	
	override def objectType = ObjectType.Commit
	
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

