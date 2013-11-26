package scala.tools.refactoring.tests.common

import scala.tools.refactoring.tests.util.TestHelper
import org.junit.Assert._
import scala.tools.refactoring.common.Selections

class SelectionDependenciesTest extends TestHelper with Selections {
  import global._

  implicit class StringToSel(src: String) {
    val root = treeFrom(src)
    val selection = {
      val start = commentSelectionStart(src)
      val end = commentSelectionEnd(src)
      FileSelection(root.pos.source.file, root, start, end)
    }
  }

  @Test
  def inboundDeps = {
    val sel = """
      object O{
        val i = 1
        def fn(p: Int) = {
          /*(*/val a = p * i
          println(a)/*)*/
          a
        }
      }
      """.selection
    assertEquals("value p, value i, method println", sel.inboundDeps.mkString(", "))
  }

  @Test
  def inboundDepsDoesNotIncludeCalledMethods = {
    val sel = """
      object O{
        val i = 1
        /*(*/println(i.toInt*i*i.toInt)/*)*/
      }
      """.selection
    assertEquals("method println, value i", sel.inboundDeps.mkString(", "))
  }

  @Test
  def inboundImplicitDeps = {
    val sel = """
      object O{
        implicit def wrapInt(i: Int) = new {
          def extension = i * 2
        }
      
        val i = /*(*/2.extension/*)*/
      }
      """.selection
    assertEquals("method wrapInt", sel.inboundDeps.mkString(", "))
  }

  @Test
  def inboundTypeDeps = {
    val sel = """
      class A(i: Int)
      
      object N{
        def apply() = 1
      }
      
      object O{
        val i = 1
        def fn = {
          /*(*/new A(N())/*)*/
        }
      }
      """.selection
    assertEquals("constructor A, class A, object N", sel.inboundDeps.mkString(", "))
  }

  @Test
  def inboundTypeDepsByOwner = {
    val sel = """
      object N{
        def apply() = 1
      }
      
      object O{
        val i = 1
        def fn = {
          /*(*/N() * i/*)*/
        }
      }
      """.selection
    val symO = sel.expandTo[DefDef].get.enclosingTree.symbol.ownerChain.find(_.isType).get
    assertEquals("value i", sel.inboundDepsOwnedBy(symO).mkString(", "))
  }

  @Test
  def outboundLocalDeps = {
    val sel = """
      object O{
        def fn(p: Int) = {
          /*(*/val (a, b) = (1, p)
          val c = a/*)*/
          (b, c)
        }
      }
      """.selection
    assertEquals("value b, value c", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def noOutboundLocalDeps = {
    val sel = """
      object O{
        def fn(c: Int) = {
    	  {
            /*(*/val (a, b) = (1, p)
            val c = a/*)*/
          }
          c
        }
      }
      """.selection
    assertEquals("", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundDepsInParameterLists = {
    val sel = """
      object O{
        def fn(/*(*/c: Int, d: Int/*)*/) = {
          c
        }
      }
      """.selection
    assertEquals("value c", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  def outboundDepsInTemplateScope = {
    val sel = """
      object O{
        def fn = fm
        /*(*/def fm = 1
        def fo = 2/*)*/
        def fq = fo
      }
      """.selection
    assertEquals("method fm, method fo", sel.outboundLocalDeps.mkString(", "))
  }

  @Test
  @Ignore
  def outboundImportedDeps = {
    val sel = """
      object O{
        def fn = {
          /*(*/import scala.math.Pi/*)*/
    	  Pi
        }
      }
      """.selection
    assertEquals("value Pi", sel.outboundLocalDeps.mkString(", "))
  }
}