package scala.tools.refactoring.implementations

import scala.tools.refactoring.MultiStageRefactoring
import scala.tools.refactoring.analysis.Indexes
import scala.tools.refactoring.transformation.TreeFactory
import scala.tools.refactoring.common.InteractiveScalaCompiler
import scala.tools.nsc.interactive.Global

abstract class TransformViewBoundRefactoring
    extends MultiStageRefactoring
    with InteractiveScalaCompiler {

  import global._

  type PreparationResult = Tree

  type RefactoringParameters = String

  private val isMethodWithViewBound: Tree => Boolean = {
    // bugs:
    // - only one param list
    // - more than two param lists
    // - existing param list with implicits
    case DefDef(_, _, _, List(_, List(ValDef(_, name, tpt: TypeTree, _))), _, _) if isProbableTypeBound(name) =>
      tpt.original match {
        case AppliedTypeTree(_, args) if isViewBound(args) =>
          true
        case _ =>
          false
      }
    case _ =>
      false
  }

  def prepare(s: Selection) =
    s.findSelectedWithPredicate(isMethodWithViewBound) match {
      case _ if s.selectedTopLevelTrees.size > 0 =>
        Left(PreparationError("You are not allowed to select a statement"))
      case Some(tree) =>
        Right(tree)
      case None =>
        Left(PreparationError("error because of none"))
    }

  def perform(selection: Selection, expr: PreparationResult, params: RefactoringParameters) = {
    import Flag._
    val transformDefDef = transform {
      // bugs:
      // - check for name conflicts with new param name
      // - extract correct AppliedTypeTree when multiple implicit params exist
      case dd @ DefDef(_, _, tparams, paramss, _, _) =>
        val origTypeTree = paramss(1).head.tpt.asInstanceOf[TypeTree]
        val origAppliedTypeTree = origTypeTree.original.asInstanceOf[AppliedTypeTree]
        val arg0 = origAppliedTypeTree.args(0)
        val arg1 = origAppliedTypeTree.args(1)
        val att = AppliedTypeTree(
            Ident(typeOf[Function1[Unit, Unit]].typeSymbol), // Function1[_, _] not working, bug?
            List(arg0, arg1))
        val newTypeParams = TypeDef(
            Modifiers(PARAM),
            arg0.nameString,
            Nil,
            EmptyTree)
        val newParams = ValDef(
            Modifiers(IMPLICIT | PARAM) withPosition (IMPLICIT, NoPosition),
            newTermName("ev"),
            TypeTree().setOriginal(att).setType(origTypeTree.tpe),
            EmptyTree)

        dd.copy(
            tparams = List(newTypeParams),
            vparamss = paramss.head :: List(List(newParams))
        ) replaces dd
    }
    Right(refactor(transformDefDef(expr).toList))
  }



  private def isViewBound(args: List[Tree]): Boolean =
    args.size == 2

  private def isProbableTypeBound(name: Name): Boolean =
    name.startsWith(nme.EVIDENCE_PARAM_PREFIX)
}