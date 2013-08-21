package scala.tools.refactoring.tests.implementations

import scala.tools.refactoring.tests.util.TestRefactoring
import scala.tools.refactoring.tests.util.TestHelper
import scala.tools.refactoring.implementations.TransformViewBoundRefactoring

import language.reflectiveCalls

class TransformViewBoundRefactoringTest extends TestHelper with TestRefactoring {

  private def transform(fs: FileSet) = new TestRefactoringImpl(fs) {
    val refactoring = new TransformViewBoundRefactoring with TestProjectIndex
    val changes = performRefactoring("")
  }.changes

  @Test
  def viewBoundToImplicitParamList() = new FileSet {
    """
    trait T
    class X {
      def f[A <% T](a: A): A = ???/*<-*/
    }
    """ becomes
    """
    trait T
    class X {
      def f[A](a: A)(implicit ev: A => T): A = ???/*<-*/
    }
    """
  } applyRefactoring transform
}