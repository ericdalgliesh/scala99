def penultimate[A](list: List[A]): Option[A] = list match {
	case Nil => None
	case head :: tail :: Nil => Some(head)
	case head :: tail => penultimate(tail)
}
