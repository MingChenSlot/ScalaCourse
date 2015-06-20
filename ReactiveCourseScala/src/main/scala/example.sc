val x = 15
def increase(i: Int) = i + 1
increase(x)

val f:String => String = {
  case "ping" => "pong"
  case "abc" => "test"
}

f("ping")
f("abc")

val pf: PartialFunction[String, String] = {
  case "ping" => "pong"
}

pf.isDefinedAt("ping")
pf.isDefinedAt("pong")

val g: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x::rest=>
    rest match {
      case Nil => "two"
  }
}

g.isDefinedAt(List(1, 2, 3))
g.isDefinedAt(List(1))


val map_test_list = List(1, 2, 3)
map_test_list.map(x => 4)
//generate a list with 6 4s
map_test_list.flatMap(x => for (y <- 1 to 2) yield 4)


def f(x: Int) = if (x > 2) Some(x) else None
val optionList = List(1, 2, 3, 4)

optionList map {x => f(x)}
optionList flatMap {x => f(x)}

def g(x:Int) = List(x, x + 2)
optionList map {x => g(x)}
optionList flatMap {x => g(x)}

val name: Option[String] = Some("")
val upper = name map { _.trim } filter { _.length != 0 } map { _.toUpperCase }
println(upper getOrElse "")

def sum(xs: List[Int]): Int = {
  if (xs.isEmpty) 0
  else xs.head + sum(xs.tail)
}

sum(List(1, 2, 3, 4))
sum(List(1, 2, 3, 4, 10))

