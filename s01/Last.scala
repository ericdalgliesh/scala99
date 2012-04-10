def last[A](list: List[A]): Option[A] = list match {
	case Nil => None
	case head :: Nil => Some(head)
	case head :: tail => last(tail)
}
