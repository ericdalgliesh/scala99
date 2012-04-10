def length(list:List[_]):Int = list match {
	case Nil => 0
	case head :: tail => 1 + length(tail)
}
