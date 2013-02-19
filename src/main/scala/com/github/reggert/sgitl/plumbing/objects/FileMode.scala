package com.github.reggert.sgitl.plumbing.objects

sealed abstract class FileMode extends Equals
{
	def toInt : Int
	// lazy because it relies on toString implementation from subclasses
	final lazy val toBytes = UTF8(toString) 
	
	def unapply(s : String) : Option[this.type] = 
		if (s == toString) Some(this) else None
		
	def canEqual(other: Any) = other.isInstanceOf[FileMode]
	
	override def equals(other: Any) = other match 
	{
		case that : FileMode if that.canEqual(this) => this.toInt == that.toInt
		case _ => false
	}
  
	override def hashCode() = 
	{
		val prime = 41
		prime + toInt
	}
}





object FileMode
{
	object AsString
	{
		def apply(fileMode : FileMode) = fileMode.toString
	
		def unapply(s : String) : Option[FileMode] = 
			NonExecutableFile.unapply(s) orElse
			Tree.unapply(s) orElse
			GitLink.unapply(s) orElse
			ExecutableFile.unapply(s) orElse
			SymLink.unapply(s)
	}
	
	
	object AsBytes
	{
		def apply(fileMode : FileMode) = fileMode.toBytes
		
		def unapply(bytes : Seq[Byte]) : Option[FileMode] =
			UTF8.unapply(bytes) flatMap (AsString.unapply)
	}
	
	
	object Tree extends FileMode
	{
		override def toString = "040000"
		override def toInt = 0x40000
	}


	object SymLink extends FileMode
	{
		override def toString = "120000"
		override def toInt = 0x120000
	}
	
	
	object GitLink extends FileMode
	{
		override def toString = "160000"
		override def toInt = 0x160000
	}
	
	
	abstract sealed class RegularFile extends FileMode
	{
		def isExecutable : Boolean
	}
	
	
	object NonExecutableFile extends RegularFile
	{
		override def isExecutable = false
		override def toString = "100644"
		override def toInt = 0x100644
	}
	
	
	object ExecutableFile extends RegularFile
	{
		override def isExecutable = true
		override def toString ="100755" 
		override def toInt = 0x100755
	}
}