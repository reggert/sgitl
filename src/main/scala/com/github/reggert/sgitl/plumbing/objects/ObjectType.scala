package com.github.reggert.sgitl.plumbing.objects

sealed abstract class ObjectType private(override val toString : String)


object ObjectType
{
	case object Blob extends ObjectType("blob")
	case object Tree extends ObjectType("tree")
	case object Commit extends ObjectType("commit")
	case object Tag extends ObjectType("tag")
	
	def apply(objectType : ObjectType) : String = objectType.toString
	
	def unapply(s : String) = s match
	{
		case Blob.toString => Some(Blob)
		case Tree.toString => Some(Tree)
		case Commit.toString => Some(Commit)
		case Tag.toString => Some(Tag)
		case _ => None
	}
}