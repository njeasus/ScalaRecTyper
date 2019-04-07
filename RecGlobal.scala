package rectyper.compiler

import scala.tools.nsc.Global

trait RecursiveFunctions extends Global {
  override lazy val analyzer = new {
    override val global: this.type = RecursiveFunctions.this
  } with RecTyperAnalyzer
}