def reverse_[A](h:A, list:List[A]):List[A] = list match {
	case Nil => List(h)
	case head :: tail => reverse_(head, tail) :+ h
}

def reverse[A](list:List[A]):List[A] = list match {
	case Nil => Nil
	case head :: Nil => List(head)
	case head :: tail => reverse_(head, tail)
}

