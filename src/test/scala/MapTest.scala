object MapTest extends App {
    
  Map(1 -> "a", 2 -> "b", 3 -> "c") match {
    case m@MapExt((1, a), (2, b@"b"), 3 -> c) => println(m -> (a, b, c))
  }
}