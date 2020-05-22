// See LICENSE for license details.

package firrtlTests.features

import firrtl.{ir, CircuitState, Parser, WDefInstance, WRef, WSubField}
import firrtl.annotations.{CircuitTarget, IsMember, SingleTargetAnnotation}
import firrtl.features.{LowerCaseNames, UpperCaseNames}
import firrtl.options.Dependency
import firrtl.transforms.ManipulateNamesSkipsAnnotation
import firrtl.testutils.FirrtlCheckers._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LetterCaseTransformSpec extends AnyFlatSpec with Matchers {

  case class TrackingAnnotation(val target: IsMember) extends SingleTargetAnnotation[IsMember] {
    override def duplicate(a: IsMember) = this.copy(target=a)
  }

  class CircuitFixture {
    private val input =
      """|circuit Foo:
         |  module Bar:
         |    output OuT: UInt<1>
         |    OuT <= UInt<1>(0)
         |  module Baz:
         |    output OuT: UInt<1>
         |    OuT <= UInt<1>(1)
         |  extmodule Ext:
         |    output OuT: UInt<1>
         |  module Foo:
         |    input CLk: Clock
         |    input rst_P: UInt<1>
         |    input addr: UInt<8>
         |    node Bar = UInt<1>(0)
         |    reg baz: UInt<1>, CLk with: (reset => (rst_P, Bar))
         |    wire QUX: UInt<1>
         |    QUX <= UInt<1>(0)
         |    node quuxQuux = UInt<1>(0)
         |    mem MeM : @[Source.scala 1:4]
         |      data-type => UInt<8>
         |      depth => 32
         |      read-latency => 0
         |      write-latency => 1
         |      reader => Read
         |      writer => wRITE
         |      readwriter => rw
         |      read-under-write => undefined
         |    MeM.Read is invalid
         |    MeM.wRITE is invalid
         |    MeM.rw is invalid
         |    inst SuB1 of Bar
         |    inst SuB2 of Baz
         |    inst SuB3 of Ext
         |    node sub1 = UInt<1>(0)
         |    node corge_corge = SuB1.OuT
         |    node QuuzQuuz = and(SuB2.OuT, SuB3.OuT)
         |""".stripMargin

    private val Foo = CircuitTarget("Foo")
    private val Bar = Foo.module("Bar")

    val annotations = Seq(TrackingAnnotation(Foo.module("Foo").ref("MeM").field("wRITE")field("en")),
                          ManipulateNamesSkipsAnnotation(Seq(Seq(Bar)), Dependency[LowerCaseNames]))
    val state = CircuitState(Parser.parse(input), annotations)
  }

  behavior of "LowerCaseNames"

  it should "change all names to lowercase" in new CircuitFixture {
    val tm = new firrtl.stage.transforms.Compiler(Seq(firrtl.options.Dependency[LowerCaseNames]))
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "foo") => true },
      { case ir.Module(_, "foo", Seq(ir.Port(_, "clk", _, _), ir.Port(_, "rst_p", _, _), ir.Port(_, "addr", _, _)), _) => true },
      { case ir.Module(_, "Bar", Seq(ir.Port(_, "out", _, _)), _) => true },
      { case ir.Module(_, "baz", Seq(ir.Port(_, "out", _, _)), _) => true },
      /* External modules are not renamed */
      { case ir.ExtModule(_, "Ext", Seq(ir.Port(_, "OuT", _, _)), _, _) => true },
      { case ir.DefNode(_, "bar", _) => true },
      { case ir.DefRegister(_, "baz", _, WRef("clk", _, _, _), WRef("rst_p", _, _, _), WRef("bar", _, _, _)) => true },
      { case ir.DefWire(_, "qux", _) => true },
      { case ir.Connect(_, WRef("qux", _, _, _), _) => true },
      { case ir.DefNode(_, "quuxquux", _) => true },
      { case ir.DefMemory(_, "mem", _, _, _, _, Seq("read"), Seq("write"), Seq("rw"), _) => true },
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("mem", _, _, _), "read", _, _), "addr", _, _)) => true },
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("mem", _, _, _), "write", _, _), "addr", _, _)) => true },
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("mem", _, _, _), "rw", _, _), "addr", _, _)) => true },
      { case WDefInstance(_, "sub1_0", "Bar", _) => true },
      { case WDefInstance(_, "sub2", "baz", _) => true },
      /* External module instance names are renamed */
      { case WDefInstance(_, "sub3", "Ext", _) => true },
      { case ir.DefNode(_, "sub1", _) => true },
      { case ir.DefNode(_, "corge_corge", WSubField(WRef("sub1_0", _, _, _), "out", _, _)) => true },
      { case ir.DefNode(_, "quuzquuz", ir.DoPrim(_, Seq(WSubField(WRef("sub2", _, _, _), "out", _, _),
                                                        WSubField(WRef("sub3", _, _, _), "OuT", _, _)), _, _)) => true }
    )
    expected.foreach( statex should containTree (_) )
  }

  behavior of "UpperCaseNames"

  it should "change all names to uppercase" in new CircuitFixture {
    val tm = new firrtl.stage.transforms.Compiler(Seq(firrtl.options.Dependency[UpperCaseNames]))
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "FOO") => true },
      { case ir.Module(_, "FOO", Seq(ir.Port(_, "CLK", _, _), ir.Port(_, "RST_P", _, _), ir.Port(_, "ADDR", _, _)), _) => true },
      { case ir.Module(_, "Bar", Seq(ir.Port(_, "OUT", _, _)), _) => true },
      { case ir.Module(_, "BAZ", Seq(ir.Port(_, "OUT", _, _)), _) => true },
      /* External modules are not renamed */
      { case ir.ExtModule(_, "Ext", Seq(ir.Port(_, "OuT", _, _)), _, _) => true },
      { case ir.DefNode(_, "BAR", _) => true },
      { case ir.DefRegister(_, "BAZ", _, WRef("CLK", _, _, _), WRef("RST_P", _, _, _), WRef("BAR", _, _, _)) => true },
      { case ir.DefWire(_, "QUX", _) => true },
      { case ir.Connect(_, WRef("QUX", _, _, _), _) => true },
      { case ir.DefNode(_, "QUUXQUUX", _) => true },
      { case ir.DefMemory(_, "MEM", _, _, _, _, Seq("READ"), Seq("WRITE"), Seq("RW"), _) => true },
      /* The subfields of memory are hard-coded to lowercase. These shouldn't change. */
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("MEM", _, _, _), "READ", _, _), "addr", _, _)) => true },
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("MEM", _, _, _), "WRITE", _, _), "addr", _, _)) => true },
      { case ir.IsInvalid(_, WSubField(WSubField(WRef("MEM", _, _, _), "RW", _, _), "addr", _, _)) => true },
      { case WDefInstance(_, "SUB1", "Bar", _) => true },
      { case WDefInstance(_, "SUB2", "BAZ", _) => true },
      /* External module instance names are renamed */
      { case WDefInstance(_, "SUB3", "Ext", _) => true },
      { case ir.DefNode(_, "SUB1_0", _) => true },
      { case ir.DefNode(_, "CORGE_CORGE", WSubField(WRef("SUB1", _, _, _), "OUT", _, _)) => true },
      { case ir.DefNode(_, "QUUZQUUZ", ir.DoPrim(_, Seq(WSubField(WRef("SUB2", _, _, _), "OUT", _, _),
                                                        WSubField(WRef("SUB3", _, _, _), "OuT", _, _)), _, _)) => true }
    )
    expected.foreach( statex should containTree (_) )
  }

}
