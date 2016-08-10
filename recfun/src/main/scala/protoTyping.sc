def trimFront(chars: List[Char]): List[Char] = {
  if (chars.isEmpty) chars
  else {
    if (chars.head == '(' || chars.head == ')') chars
    else trimFront(chars.tail)
  }
}

def isBalanced(chars: List[Char], opened: Int): Boolean = {
  val trimmed = trimFront(chars)
  if (opened < 0) false
  else if (trimmed.isEmpty) (opened == 0)
  else{
    if (trimmed.head == '(') isBalanced(trimmed.tail, opened+1)
    else isBalanced(trimmed.tail, opened-1)
  }
}

def balance(chars: List[Char]): Boolean = {
  isBalanced(chars, 0)
}

balance("()".toList)