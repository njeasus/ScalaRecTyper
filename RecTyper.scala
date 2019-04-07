package rectyper.compiler

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Global

/**
 * Created by njeasus on 4/27/14.
 */
trait RecTyperAnalyzer extends Analyzer {
  selfAnalyser =>
  val global: Global

  class RecTyper(context: Context) extends selfAnalyser.Typer(context) {
    import global._
    println("RecTyper is running...")

    var cyclicReferences: List[global.Ident] = Nil
    var retypedTrees: Map[global.Tree, global.Type] = Map()

    override def typedCases(cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef] = {
      val typedCases = super.typedCases(cases, pattp, pt);
      if (typedCases.exists(_.isErroneous)) {
        typedCases.find(t => t.isErroneous && t.exists(cyclicReferences.contains(_))).foreach(
          errorneous =>
          {
            val errTypes = typedCases.filter(_.isErroneous)
            if (errTypes.size > 1) throw new IllegalStateException("Too many errorneous cases")

            val okTypes = typedCases.filter(!_.isErroneous).map(t => t.tpe)
            val okLub = ptOrLub(okTypes, WildcardType)
            val errorCase = errTypes(0)
            println("LUB: " + okLub);

            retypedTrees += (errorneous -> okLub._1)
          });
      }
      typedCases
    }

    override def typedDefDef(ddef: DefDef): DefDef = {
      val typedDef = super.typedDefDef(ddef)
      val cyclic = typedDef.find(t => retypedTrees.contains(t))
      cyclic.map(retyped => {
        println("Yes probably cyclic!")
        val newType = retypedTrees(retyped)
        val ident = Ident(newType.typeSymbol)
        UnTyper.traverse(typedDef)

        val msym = ddef.symbol.asMethod
        msym.reset(MethodType(msym.paramss.flatten, newType))

        val defCopy = treeCopy.DefDef(ddef, ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, ident, ddef.rhs)
        val res = super.typedDefDef(defCopy)
        treeBrowser.browse(res) //Show the tree
        res
      }).getOrElse(typedDef)
    }

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      tree match {
        case ident @ global.Ident(s) =>
          try {
            super.typed(tree, mode, pt)
          } catch {
            case e: global.CyclicReference =>
              println("cyclic ident");
              cyclicReferences = ident :: cyclicReferences
              UnTyper.traverse(ident)
              ident
            // throw e

            case e: Throwable =>
              throw e
          }
        case _ =>
          super.typed(tree, mode, pt)
      }
    }

  }

  override def newTyper(context: Context): Typer = {
    new RecTyper(context)
  }

}
