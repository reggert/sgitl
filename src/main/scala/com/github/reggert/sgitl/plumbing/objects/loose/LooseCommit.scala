package com.github.reggert.sgitl.plumbing.objects.loose
import java.nio.charset.Charset
import com.github.reggert.sgitl.plumbing.objects.GitCommit
import com.github.reggert.sgitl.plumbing.objects.ObjectType
import com.github.reggert.sgitl.plumbing.objects.SHA1

final case class LooseCommit private[sgitl] (override val headers : Seq[(String, String)], override val message : String) 
	extends LooseObject with GitCommit
{
	def this(tree : SHA1, parents : Seq[SHA1], author : String, committer : String, message : String) = this(
			headers = 
				(for (p <- parents) yield "parent" -> p.toString) ++ 
				Seq("tree" -> tree.toString, "author" -> author, "committer" -> committer), 
			message = message
		)
			
	def withEncoding(charset : Charset) : LooseCommit = 
		new LooseCommit(headers.filterNot(_._1 == "encoding") :+ ("encoding", charset.name()), message)
	
	def withDefaultEncoding : LooseCommit = 
		new LooseCommit(headers.filterNot(_._1 == "encoding"), message)
}

