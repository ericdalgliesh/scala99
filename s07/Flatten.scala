def flatten[A] (list:List[A]):List[Any]  = list match{
	case Nil	=> Nil
		// In the following line, Any is erased, but this causes a warning. I'd love to know how to fix it...
	case ((head:List[Any]) :: tail) => flatten(head) ++ flatten(tail)
	case (head :: tail) => head +: flatten(tail) 
}
