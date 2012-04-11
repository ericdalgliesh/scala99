def compress[A](list:List[A]):List[A] = list match {
	case Nil => Nil
	case (h :: Nil) => List(h)
	case (head :: tail) => tail match {
		case Nil => Nil
		case (h :: t) if (h == head) => compress (tail)
		case (h :: t) => head +: compress(tail)
	}
}
