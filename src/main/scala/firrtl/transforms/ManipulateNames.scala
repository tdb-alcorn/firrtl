// See LICENSE for license details.

package firrtl.transforms

import firrtl._
import firrtl.analyses.InstanceGraph
import firrtl.Mappers._

import firrtl.annotations.{
  CircuitTarget,
  CompleteTarget,
  InstanceTarget,
  ModuleTarget,
  MultiTargetAnnotation,
  ReferenceTarget,
  Target,
  TargetToken
}
import firrtl.options.Dependency
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency

import scala.collection.{
  immutable,
  mutable
}
import scala.reflect.ClassTag

/** Annotation that indicates that a specific child-transform of [[ManipulateNames]] should ignore specific
  * [[firrtl.annotations.Target Target]]s.
  * @param targets the FIRRTL IR targets that should not be renamed
  * @param transform the transform that this should apply to
  * @tparam A a sub-type of [[ManipulateNames]]
  */
case class ManipulateNamesSkipsAnnotation[A <: ManipulateNames[_]](
  targets: Seq[Seq[Target]],
  transform: Dependency[A]) extends MultiTargetAnnotation {

  override def duplicate(a: Seq[Seq[Target]]) = this.copy(targets=a)

}

/** A datastructure used to do single-pass name manipulation
  * @param circuit the [[ir.Circuit]] that will be manipulated
  * @param renames a rename map
  * @param skips a set of [[firrtl.annotations.Target Target]]s that should not be renamed
  */
private class RenameDataStructure(
  circuit: ir.Circuit,
  val renames: RenameMap,
  val skips: immutable.Set[Target] = Set.empty) {

  /** A mapping of targets to associated namespaces */
  val namespaces: mutable.HashMap[CompleteTarget, Namespace] = mutable.HashMap(CircuitTarget(circuit.main) -> Namespace(circuit))

  /** A mapping of a reference to either an instance or a memory (encoded as a [[ReferenceTarget]] */
  val instanceMap: mutable.HashMap[ReferenceTarget, Either[ReferenceTarget, InstanceTarget]] = mutable.HashMap.empty

}

/** Transform for manipulate all the names in a FIRRTL circuit.
  * @tparam A the type of the child transform
  */
abstract class ManipulateNames[A <: ManipulateNames[_] : ClassTag] extends Transform with DependencyAPIMigration {

  /** A function used to manipulate a name in a FIRRTL circuit */
  def manipulate: (String, Namespace) => Option[String]

  override def prerequisites: Seq[TransformDependency] = Seq(Dependency(firrtl.passes.LowerTypes))
  override def optionalPrerequisites: Seq[TransformDependency] = Seq.empty
  override def optionalPrerequisiteOf: Seq[TransformDependency] = Forms.LowEmitters
  override def invalidates(a: Transform) = a match {
    case _: analyses.GetNamespace => true
    case _                        => false
  }

  /** Compute a new name for some target and record the rename if the new name differs.
    * @param name the string to rename
    * @param r a data structure containing information necessary for renaming
    * @param target the target associated with the name
    * @return a new name (or the old name if no renaming is necessary)
    */
  private def doRename(name: String)(implicit r: RenameDataStructure, target: CompleteTarget): String = {
    /* Compute the new name and, optionally, a new target. */
    val (namex: String, ax: Option[CompleteTarget]) = target match {
      /* Do not rename if this is designated as a skip */
      case a if r.skips.contains(a) =>
        (name, None)
      /* Circuit renaming */
      case a@ CircuitTarget(b) => manipulate(b, r.namespaces(a)) match {
        case Some(str) => (str, Some(a.copy(circuit = str)))
        case None      => (b, None)
      }
      /* Module renaming */
      case a@ ModuleTarget(_, b) => manipulate(b, r.namespaces(a)) match {
        case Some(str) => (str, Some(a.copy(module = str)))
        case None      => (b, None)
      }
      /* Instance renaming */
      case a@ InstanceTarget(_, _, Nil, b, c) => manipulate(b, r.namespaces(a.moduleTarget)) match {
        case Some(str) => (str, Some(a.copy(instance = str)))
        case None      => (b, None)
      }
      /* Rename either a module component or a memory */
      case a@ ReferenceTarget(_, _, _, b, Nil) => manipulate(b, r.namespaces(a.moduleTarget)) match {
        case Some(str) => (str, Some(a.copy(ref = str)))
        case None      => (b, None)
      }
      /* Rename an instance port or a memory reader/writer/readwriter */
      case a@ ReferenceTarget(_, _, _, b, (token@ TargetToken.Field(c)) :: Nil) =>
        val ref = r.instanceMap(a.moduleTarget.ref(b)) match {
          case Right(foo) => foo.ofModuleTarget
          case Left(foo)  => foo
        }
        manipulate(c, r.namespaces(ref)) match {
          case Some(str) => (str, Some(a.copy(component = Seq(token.copy(str)))))
          case None      => (c, None)
        }
    }
    /* Record the optional rename */
    ax.foreach(axx => r.renames.rename(target, r.renames(axx)))
    namex
  }

  /** Change a name based on known renames, but do not record the rename. */
  private def maybeRename(name: String)(implicit r: RenameDataStructure, a: CompleteTarget): String = {
    /* Based on the target, determine the scope: either this is a reference to something in this module a submodule port. */
    val scope: CompleteTarget = a match {
      /* Submodule scope */
      case a@ ReferenceTarget(_, _, _, root, TargetToken.Field(c) :: Nil) =>
        r.instanceMap.get(a.copy(component=Nil)) match {
          case Some(Right(inst: InstanceTarget)) => inst.ofModuleTarget.ref(c)
          case Some(Left(mem: ReferenceTarget)) => mem.field(c)
          case None                       => a
        }
      case _ => a
    }

    r.renames.underlying.get(scope) match {
      case Some(ax) if ax.size == 1 =>
        val axx = ax match {
          case Seq(foo: CircuitTarget)   => foo.name
          case Seq(foo: ModuleTarget)    => foo.module
          case Seq(foo: InstanceTarget)  => foo.instance
          case Seq(foo: ReferenceTarget) => foo.tokens.last match {
            case TargetToken.Ref(value)   => value
            case TargetToken.Field(value) => value
            case _ =>
              Utils.throwInternalError("Reference target must end in 'Ref' or 'Field'", Some(new MatchError(foo.serialize)))
          }
        }
        axx
      case None => name
    }
  }

  /** Rename an expression
    *
    * This logic exploits the known structure of the output of [[LowerTypes]] such that the only possible expressions in
    * a module are: (1) references to module components, (2) subfields of references are instance components, and (3)
    * subfields of subfields or references are memory ports.
    */
  private def onExpression(e: ir.Expression)(implicit r: RenameDataStructure, t: ModuleTarget): ir.Expression = e match {
    /* A reference to something inside this module */
    case w: WRef => w.copy(name = maybeRename(w.name)(r, Target.asTarget(t)(w)))
    /* This is either the subfield of an instance or a subfield of a memory reader/writer/readwriter */
    case w@ WSubField(expr, ref, _, _) => expr match {
      /* This is an instance */
      case we@ WRef(inst, _, _, _) =>
        val (rTarget: ReferenceTarget, iTarget: InstanceTarget) = r.instanceMap(Target.asTarget(t)(we)) match {
          case Right(a) => (a.ofModuleTarget.ref(ref), a)
          case Left(_)  => ???
        }
        w.copy(we.copy(name=maybeRename(inst)(r, iTarget)), name=maybeRename(ref)(r, rTarget))
      /* This is a reader/writer/readwriter */
      case ws@ WSubField(expr, port, _, _) => expr match {
        /* This is the memory. */
        case wr@ WRef(mem, _, _, _) =>
          w.copy(
            expr=ws.copy(
              expr=wr.copy(name=maybeRename(mem)(r, t.ref(mem))),
              name=maybeRename(port)(r, t.ref(mem).field(port))))
      }
    }
    case e => e.map(onExpression)
  }

  /** Rename a statement
    *
    * Instances will update the rename data structure. Memories are treated specially to rename their readers, writers,
    * and readwriters.
    */
  private def onStatement(s: ir.Statement)(implicit r: RenameDataStructure, t: ModuleTarget): ir.Statement = s match {
    case decl: ir.IsDeclaration => decl match {
      case decl@ WDefInstance(_, inst, mod, _) =>
        val modx = maybeRename(mod)(r, t.circuitTarget.module(mod))
        val instx = doRename(inst)(r, t.instOf(inst, mod))
        r.instanceMap(t.ref(inst)) = Right(t.instOf(inst, mod))
        decl.copy(name = instx, module = modx)
      case decl: ir.DefMemory =>
        val namex = doRename(decl.name)(r, t.ref(decl.name))
        val tx = t.ref(decl.name)
        r.namespaces(tx) = Namespace(decl.readers ++ decl.writers ++ decl.readwriters)
        r.instanceMap(tx) = Left(tx)
        decl
          .copy(
            name = namex,
            readers = decl.readers.map(_r => doRename(_r)(r, tx.field(_r))),
            writers = decl.writers.map(_w => doRename(_w)(r, tx.field(_w))),
            readwriters = decl.readwriters.map(_rw => doRename(_rw)(r, tx.field(_rw)))
          )
          .map(onExpression)
      case decl =>
        decl
          .map(doRename(_: String)(r, t.ref(decl.name)))
          .map(onExpression)
    }
    case s => s.map(onStatement).map(onExpression)
  }

  /** Rename a port */
  private def onPort(p: ir.Port)(implicit r: RenameDataStructure, t: ModuleTarget): ir.Port = {
    p.map(doRename(_: String)(r, t.ref(p.name)))
  }

  /** Rename a [[DefModule]] and it's internals (ports and statements) to fix keyword collisions and update instance
    * references to respect previous renames
    * @param renames a [[RenameMap]]
    * @param circuit the enclosing [[CircuitName]]
    * @return a [[DefModule]] without keyword conflicts
    */
  private def onModule(m: ir.DefModule)(implicit r: RenameDataStructure, t: CircuitTarget): ir.DefModule = m match {
    case _: ir.ExtModule => m
    case _: ir.Module =>
      implicit val moduleTarget = t.module(m.name)
      r.namespaces(moduleTarget) = Namespace(m)

      m
        .map(doRename(_: String)(r, moduleTarget))
        .map(onPort)
        .map(onStatement)
  }

  /** Manipulate all names in a circuit
    *
    * @param c an input circuit
    * @param renames a rename map that will be updated as names are manipulated
    * @param skips a set of targets that should not be manipulated
    * @return the circuit with manipulated names
    */
  def run(c: ir.Circuit, renames: RenameMap = RenameMap(), skips: Set[Target] = Set.empty): ir.Circuit = {
    implicit val r = new RenameDataStructure(c, renames, skips)
    implicit val t = CircuitTarget(c.main)

    val cx = c.map(doRename)

    /* Rename all modules from leafs to root in one pass while updating a shared rename map. Going from leafs to roots
     * ensures that the rename map is safe for parents to blindly consult.
     */
    val modulesx: Map[ModuleTarget, ir.DefModule] = new InstanceGraph(cx).moduleOrder.reverse
      .map(m => t.module(m.name) -> onModule(m))
      .toMap

    /* Replace the old modules making sure that they are still in the same order */
    cx.copy(modules = c.modules.map(m => modulesx(t.module(m.name))))
  }

  /** Return a circuit state with all sensitive names manipulated */
  def execute(state: CircuitState): CircuitState = {

    val skips: Set[Target] = state.annotations.collect {
      case ManipulateNamesSkipsAnnotation(skips, _: Dependency[A]) => skips
    }.flatten.flatten.toSet

    val renames = RenameMap()
    val statex = state.copy(circuit = run(state.circuit, renames, skips), renames = Some(renames))
    statex
  }

}
