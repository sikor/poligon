package poligon.comp

import monix.eval.Task
import poligon.comp.CompFamily.LayoutModification.Added
import poligon.comp.CompFamily.MenuTree.MenuItem
import poligon.comp.CompFamily.{LayoutModification, LayoutSettings}
import poligon.polyproperty.{HasSimplePropertyCodec, Obs, PropertyWithParent, Sin}

/**
  * TODO:
  * 1. i18n - Options:
  * 1.1 components takes translation keys
  * 1.2 components takes translated strings
  * Currently components must display its values immediately, so they must handle progress spinners themselves.
  * We should allow components to wait for some async resource and use global spinner for that - blocking UI.
  * 2. dependency injection
  * 3. routing
  * 4. Obs flatMap?
  * 5. Try Get rid of replaceable - should be needed only in app root
  */
trait Comp {
  def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]]
}

object Comp extends HasSimplePropertyCodec[Comp] {

  private trait SimpleComp extends Comp {
    def createSimple[C](family: CompFamily[C]): BindableComp[C]

    def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]] = Task.now(createSimple(family))
  }

  def factory(highLevelFactory: => Comp): Comp = new Comp {
    def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]] = highLevelFactory.createComponent(family)
  }

  def dynLayout[V](
                    property: Obs[Seq[LayoutModification[Comp]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): Comp =
    new Comp {
      def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]] = {
        val comps = property.map(mods => mods.map(mod => mod
          .map(desc => desc.createComponent(family))
          .map(_ => family.label(Obs.constant("not supported")))
        ))
        Task.now(family.layout(comps, layoutDescription))
      }
    }

  def dynLabel(property: Obs[String], styleName: String = ""): Comp =
    new SimpleComp {
      def createSimple[C](family: CompFamily[C]): BindableComp[C] = family.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp =
    new SimpleComp {
      def createSimple[C](family: CompFamily[C]): BindableComp[C] = family.textField(caption, initValue, onValueSet)
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): Comp =
    new SimpleComp {
      def createSimple[C](family: CompFamily[C]): BindableComp[C] = family.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): Comp =
    new SimpleComp {
      def createSimple[C](family: CompFamily[C]): BindableComp[C] = family.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): Comp =
    new SimpleComp {
      def createSimple[C](family: CompFamily[C]): BindableComp[C] = family.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: Obs[Comp]): Comp =
    new Comp {
      def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]] = {
        val c = property.map(c => c.createComponent(family)).map(_ => family.label(Obs.constant("not supported")))
        Task.now(family.replaceable(c))
      }
    }

  def asyncComp(compTask: Task[Comp]): Comp = new Comp {
    def createComponent[C](family: CompFamily[C]): Task[BindableComp[C]] = {
      compTask.flatMap(c => c.createComponent(family))
    }
  }

  def layout(comps: Comp*)(settings: LayoutSettings = LayoutSettings()): Comp =
    dynLayout(
      Obs.constant(comps.iterator.zipWithIndex.map { case (comp, index) => Added(index, comp) }.toSeq),
      settings)

  def label(value: String, styleName: String = ""): Comp =
    dynLabel(Obs.constant(value), styleName)

  def textField(caption: String, property: PropertyWithParent[String]): Comp =
    textField(caption, property.read, property.set)

  def button(onClick: Sin[Unit], caption: String): Comp =
    button(onClick, Obs.constant(caption), Obs.constant(true))
}
