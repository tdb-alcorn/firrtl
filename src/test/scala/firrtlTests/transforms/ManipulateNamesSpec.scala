// See LICENSE for license details.

package firrtlTests.transforms

import firrtl.{ir, CircuitState, Namespace, Parser}
import firrtl.annotations.CircuitTarget
import firrtl.options.Dependency
import firrtl.testutils.FirrtlCheckers._
import firrtl.transforms.{ManipulateNames, ManipulateNamesSkipsAnnotation}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ManipulateNamesSpec {

  class AddPrefix extends ManipulateNames {
    override def manipulate = (a: String, b: Namespace) => Some(b.newName("prefix_" + a))
  }

}

class ManipulateNamesSpec extends AnyFlatSpec with Matchers {

  import ManipulateNamesSpec._

  class CircuitFixture {
    protected val input =
      """|circuit Foo:
         |  module Bar:
         |    skip
         |  module Foo:
         |    skip
         |""".stripMargin
    val tm = new firrtl.stage.transforms.Compiler(Seq(Dependency[AddPrefix]))
  }

  behavior of "ManipulateNames"

  it should "rename everything by default" in new CircuitFixture {
    val state = CircuitState(Parser.parse(input), Seq.empty)
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "prefix_Foo") => true },
      { case ir.Module(_, "prefix_Foo", _, _) => true},
      { case ir.Module(_, "prefix_Bar", _, _) => true}
    )
    expected.foreach(statex should containTree (_))
  }

  it should "do nothing if the circuit is skipped" in new CircuitFixture {
    val annotations = Seq(ManipulateNamesSkipsAnnotation(Seq(Seq(CircuitTarget("Foo"))), Dependency[AddPrefix]))
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    state.circuit should be (statex.circuit)
  }

  it should "not rename the circuit if the top module is skipped" in new CircuitFixture {
    val Foo = CircuitTarget("Foo").module("Foo")
    val annotations = Seq(ManipulateNamesSkipsAnnotation(Seq(Seq(Foo)), Dependency[AddPrefix]))
    val state = CircuitState(Parser.parse(input), annotations)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "Foo") => true },
      { case ir.Module(_, "Foo", _, _) => true},
      { case ir.Module(_, "prefix_Bar", _, _) => true}
    )
    val statex = tm.execute(state)
    expected.foreach(statex should containTree (_))
  }

}
