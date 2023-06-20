class CurriedFunction {
  val simpleAddFunction = (x: Int, y: Int) => x + y
  val add7_1 = (x: Int) => simpleAddFunction(x, 7)
  val add7_2 = (x: Int) => simpleAddMethod(x, 7)


  //add7
  val add7_3 = (x: Int) => curriedAddMethod(x)(7)
  val add7_4 = (x: Int) => simpleAddFunction.curried(7)
  val add7_5 = curriedAddMethod(7) _
  val add7_6 = simpleAddFunction(7, _: Int)

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  def format1 = curriedFormatFun("%4.2f") _

  def curriedFormatFun(x: String)(y: Double) = x.format(y)

  def format2 = curriedFormatFun("%8.6f") _


  def byName(n: => Int) = n + 1
  def byFun(f:() => Int) = f() + 1
  def method = 42
  def paramMethod() = 42


  byName(method)
  byName(paramMethod())
  byName(paramMethod)
//  byName(()=>43)// not ok
//  byFun(32) // not ok
//byFun(method) // not ok !!!!
  byFun(paramMethod)
  byFun(()=>42)

}

object CurriedFunction {
  def main(args: Array[String]): Unit = {
    val x = new CurriedFunction
    val list = Seq(Math.PI, Math.E)
    println(list.map(x.format2))
  }
}
