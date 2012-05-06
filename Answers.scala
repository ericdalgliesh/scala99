def last[A](list: List[A]): A = list match {
	case Nil => throw new NoSuchElementException
	case head :: Nil => head
	case head :: tail => last(tail)
}

def penultimate[A](list: List[A]): Option[A] = list match {
	case Nil => None
	case head :: tail :: Nil => Some(head)
	case head :: tail => penultimate(tail)
}
def nth[A](n:Int, list: List[A]): Option[A] = list match {
	case Nil => None
	case head :: tail => n match {
		case 0 => Some(head)
		case _ => nth (n - 1, tail)
	}
}
def length(list:List[_]):Int = list match {
	case Nil => 0
	case head :: tail => 1 + length(tail)
}
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
def flatten[A] (list:List[A]):List[Any]  = list match{
	case Nil	=> Nil
		// In the following line, Any is erased, but this causes a warning. I'd love to know how to fix it...
	case ((head:List[Any]) :: tail) => flatten(head) ++ flatten(tail)
	case (head :: tail) => head +: flatten(tail) 
}
def compress[A](list:List[A]):List[A] = list match {
	case Nil => Nil
	case (h :: Nil) => List(h)
	case (head :: tail) => tail match {
		case Nil => Nil
		case (h :: t) if (h == head) => compress (tail)
		case (h :: t) => head +: compress(tail)
	}
}

def pack[A](list:List[A]):List[List[A]] = {
	def detect[A](list:List[A], current:List[A], existing:A):List[List[A]] = {
		list match {
			case Nil => List(current)
			case (head :: tail) if (existing == head) => detect(tail, existing +: current, existing)
			case (head :: tail) => current +: detect(tail, List(head), head)
		}
	}

	list match {
		case Nil => Nil
		case (head :: tail) => detect(list, Nil, head)
	}
}
/*
def encode[A](list:List[A]):List[Tuple2[Int, A]] = {
	val packed = pack(list)
	def listToTuple(l:List[A]):Tuple2[Int,A] = l match {
		case (h::_) => (length(l), h)
		case Nil => throw new IllegalArgumentException("Nil is not an acceptable value.")
	}
	def traverse(l:List[List[A]]):List[Tuple2[Int,A]] = l match {
		case Nil => throw new IllegalArgumentException("Nil not acceptable")
		case (h::Nil) => List(listToTuple(h))
		case (h::t) => listToTuple(h) +: traverse(t)
	}
	traverse(pack(list))
} */

// internet answer
def encode[A](list:List[A]):List[Tuple2[Int, A]] = {
	pack (list) map {l => (l.length, l.head)}
}

def encodeModified[A](list:List[A]):List[Any] = {
	pack (list) map { l =>
	  if (l.length == 1) l.head
	  else (l.length, l.head)
	}
}

