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
  def createComponent(family: CompFamily): family.BindableComp
}

object Comp extends HasSimplePropertyCodec[Comp] {

  def factory(highLevelFactory: => Comp): Comp = new Comp {
    def createComponent(family: CompFamily): family.BindableComp = highLevelFactory.createComponent(family)
  }

  def dynLayout[V](
                    property: Obs[Seq[LayoutModification[Comp]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp =
        family.layout(
          property.map(mods => mods.map(mod => mod.map(desc => desc.createComponent(family)))),
          layoutDescription)
    }

  def dynLabel(property: Obs[String], styleName: String = ""): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp = family.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp = family.textField(caption, initValue, onValueSet)
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp = family.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp = family.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp = family.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: Obs[Comp]): Comp =
    new Comp {
      def createComponent(family: CompFamily): family.BindableComp =
        family.replaceable(property.map(c => c.createComponent(family)))
    }

  def asyncComp(compTask: Task[Comp]): Comp = new Comp {
    def createComponent(family: CompFamily): family.BindableComp = {
      val prop = PropertyWithParent(label("loading ..."))
      family.dynamic { implicit po =>
        compTask.foreachL(comp => prop.set(comp))
        replaceable(prop.obs).createComponent(family).create(po)
      }
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
