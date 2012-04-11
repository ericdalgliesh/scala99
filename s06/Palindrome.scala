def reverse_[A](h:A, list:List[A]):List[A] = list match {
	   case Nil => List(h)
	   case head :: tail => reverse_(head, tail) :+ h
}

def reverse[A](list:List[A]):List[A] = list match {
	   case Nil => Nil
	   case head :: Nil => List(head)
	   case head :: tail => reverse_(head, tail)
}

def zip[A](list:List[A], list2:List[A]):List[Tuple2[A,A]] = {
	(list, list2) match {
		case (Nil, Nil) => Nil
		case (h1 :: t1, h2 :: t2) => (h1, h2) +: zip(t1, t2)
		case (_, _) => Nil
	}
}

def allEqual[A](list:List[(A,A)]):Boolean = list match {
	case Nil  => true
	case ((h1, h2) :: tail) if (h1 == h2) => allEqual (tail)
	case _ => false
}

def palindrome[A](list:List[A]):Boolean = {
	val reversed = reverse(list)
	val zipped = zip(list, reversed)
	allEqual(zipped)
}
