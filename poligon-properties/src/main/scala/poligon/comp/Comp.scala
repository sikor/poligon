package poligon.comp

import poligon.comp.BindableComp.BindableComp
import poligon.comp.CompFamily.LayoutModification.Added
import poligon.comp.CompFamily.MenuTree.MenuItem
import poligon.comp.CompFamily.{LayoutModification, LayoutSettings}
import poligon.polyproperty.Act.{BAct, GSin}
import poligon.polyproperty.Obs.{AnyObs, BObs}
import poligon.polyproperty.{GObs, HasSimplePropertyCodec, PropertyWithParent}

/**
  * TODO:
  * 1. We should allow components to wait for some async resource and use global spinner for that - blocking UI.
  * 1.1 Handle situation when component that waits for data is removed when data arrives -
  * 2. routing
  * 3. Obs combine
  */
trait GComp[-D] {
  def createComponent[T](family: CompFamily[T]): BindableComp[T, D]
}

trait Comps[D] extends HasSimplePropertyCodec[GComp[D]] {
  type Comp = GComp[D]

  def factory(highLevelFactory: => GComp[D]): GComp[D] = new GComp[D] {
    def createComponent[T](family: CompFamily[T]): BindableComp[T, D] = highLevelFactory.createComponent(family)
  }

  def dynLayout[V](
                    modifications: AnyObs[Seq[LayoutModification[GComp[D]]]],
                    layoutDescription: LayoutSettings = LayoutSettings()): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
        val comps = modifications.map(mods => mods.map(m => m.map(_.createComponent(family))))
        family.layout(comps, layoutDescription)
      }
    }

  def dynLabel(property: BObs[String, D], styleName: String = ""): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.label(property, styleName)
    }

  def textField(caption: String, initValue: String, onValueSet: GSin[String, D]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.textField(caption, initValue, onValueSet)
    }

  def button(onClick: GSin[Unit, D], caption: BObs[String, D], enabled: BObs[Boolean, D]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.button(onClick, caption, enabled)
    }

  def checkBox(caption: String, initValue: Boolean, value: GSin[Boolean, D]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.checkBox(caption, initValue, value)
    }

  def menuBar[T](menuItems: Seq[(List[String], MenuItem[T])], itemSelected: GSin[T, D]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = family.menuBar(menuItems, itemSelected)
    }

  def replaceable(property: BObs[GComp[D], D]): GComp[D] =
    new GComp[D] {
      def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
        val c = property.map(c => c.createComponent(family))
        family.replaceable(c)
      }
    }

  def asyncComp(compTask: BAct[GComp[D], D]): GComp[D] = new GComp[D] {
    def createComponent[C](family: CompFamily[C]): BindableComp[C, D] = {
      family.dynamic[D] { po =>
        compTask.flatMap(_.createComponent(family)).run(po)
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

  def button(onClick: GSin[Unit, D], caption: String): GComp[D] =
    button(onClick, GObs.constant(caption), GObs.constant(true))
}
