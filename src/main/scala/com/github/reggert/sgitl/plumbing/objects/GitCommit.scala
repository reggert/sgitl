package com.github.reggert.sgitl.plumbing.objects

trait GitCommit extends HeaderMessageObject 
{
	final override def objectType = ObjectType.Commit
	
	def author = headers.find(_._1 == "author").get._2
	
	def committer = headers.find(_._1 == "committer").get._2
	
	def parents = headers.view.filter(_._1 == "parent").map(_._2).map(SHA1.FromString).force
	
	def tree = headers.find(_._1 == "tree").map(_._2).map(SHA1.FromString).get
}