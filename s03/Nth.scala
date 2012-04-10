def nth[A](n:Int, list: List[A]): Option[A] = list match {
	case Nil => None
	case head :: tail => n match {
		case 0 => Some(head)
		case _ => nth (n - 1, tail)
	}
}
