package poligon.comp

import monix.eval.Task
import poligon.comp.BindableComp.BindableComp
import poligon.comp.CompFamily.LayoutModification.Added
import poligon.comp.CompFamily.MenuTree.MenuItem
import poligon.comp.CompFamily.{LayoutModification, LayoutSettings}
import poligon.polyproperty.Act.Sin
import poligon.polyproperty.Obs.Obs
import poligon.polyproperty.PropertyObserver.GPropertyObservers
import poligon.polyproperty.{GObs, HasSimplePropertyCodec, PropertyWithParent}

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
trait GComp[-D] {
  def createComponent[T](family: CompFamily[T]): BindableComp[T, D]
}

object Comp extends Comps[Any]

trait Comps[D] extends HasSimplePropertyCodec[GComp[D]] {
  type Comp = GComp[D]

  def factory(highLevelFactory: => GComp[D]): GComp[D] = new GComp[D] {
    def createComponent[T](family: CompFamily[T]): BindableComp[T, D] = highLevelFactory.createComponent(family)
  }

  def dynLayout[V](
                    modifications: Obs[Seq[LayoutModification[GComp[D]]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
        val comps = modifications.map(mods => mods.map(m => m.map(_.createComponent(family))))
        family.layout(comps, layoutDescription)
      }
    }

  def dynLabel(property: GObs[String, GPropertyObservers[D]], styleName: String = ""): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: Sin[String]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.textField(caption, initValue, onValueSet)
    }

  def button(onClick: Sin[Unit], caption: Obs[String], enabled: Obs[Boolean]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: Sin[Boolean]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: Sin[T]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: Obs[GComp[D]]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
        val c = property.map(c => c.createComponent(family))
        family.replaceable(c)
      }
    }

  def asyncComp(compTask: Task[GComp[D]]): GComp[D] = new GComp[D] {
    def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
      family.dynamic[D] { po =>
        compTask.flatMap(_.createComponent(family).run(po))
      }
    }
  }

  def layout(comps: GComp[D]*)(settings: LayoutSettings = LayoutSettings()): GComp[D] =
    dynLayout(
      GObs.constant(comps.iterator.zipWithIndex.map { case (comp, index) => Added(index, comp) }.toSeq),
      settings)

  def label(value: String, styleName: String = ""): GComp[D] =
    dynLabel(GObs.constant(value), styleName)

  def textField(caption: String, property: PropertyWithParent[String]): GComp[D] =
    textField(caption, property.read, property.set)

  def button(onClick: Sin[Unit], caption: String): GComp[D] =
    button(onClick, GObs.constant(caption), GObs.constant(true))
}
