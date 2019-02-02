package poligon.exampleapp.view

import com.vaadin.ui.themes.ValoTheme
import poligon.exampleapp.properties.Binder.LayoutBuilder.{Form, Horizontal, Vertical}
import poligon.exampleapp.properties.Binder.{BaseSettings, LayoutSettings}
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.services.DmService
import poligon.exampleapp.view.ObjectPanelModel.ActionStatus.Success
import poligon.exampleapp.view.ObjectPanelModel._
import poligon.polyproperty.Property.Diff.Val
import poligon.polyproperty.{PropertyWithParent, Sin}

import scala.collection.SortedMap

//TODO: styling: https://github.com/vaadin/framework/tree/master/uitest/src/main/java/com/vaadin/tests/themes/valo
/*
Plan:
1. Handle recursive listeners calls
2. Update action statuses after callback from backend (show memory leaks handling)
3. Move propertyobservers to presenters constructors
4. rename propertywithparent.
6. Nice styling of demo
7. implicit propertyobservers or some more high level way to compose components
8. Class for handling form data with validation
 */
object ObjectPanelView {

  private class ObjectsPanelContext(val dmService: DmService) {
    val model: PropertyWithParent[SortedMap[String, SomeObject]] = PropertyWithParent(() => dmToObjects(dmService.getDm))
    val newObjectName = PropertyWithParent("")
  }

  def create(dmService: DmService): Comp =
    Comp.factory(createObjectPanelView(new ObjectsPanelContext(dmService)))

  def createObjectPanelView(presenter: ObjectsPanelContext): Comp = Binder.layout(
    Binder.label("Objects", ValoTheme.LABEL_H1),
    Binder.layout(
      Binder.textField("object name", presenter.newObjectName),
      Binder.button("add object", presenter.model.put.rMap(_ => {
        val objectName = presenter.newObjectName.read
        (objectName, SomeObject(objectName, SortedMap.empty))
      }))
    )(Horizontal()),
    Binder.dynLayout(presenter.model.structObs) { p =>
      createObjectTile(presenter, p)
    }
  )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

  def createObjectTile(presenter: ObjectsPanelContext, p: PropertyWithParent[SomeObject]): Comp = {
    def newInstanceNum = p.getField(_.newInstanceNumber).read.toOpt.get

    Binder.layout(
      Binder.layout(
        Binder.dynLabel(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), ValoTheme.LABEL_H2),
        Binder.button("remove", presenter.model.remove.rMap(_ => p.read.name))
      )(Horizontal()),
      Binder.layout(
        Binder.textField("instance number", "", p.getField(_.newInstanceNumber).set
          .rMap(s => Val(s.toInt))),
        Binder.button("add instance", Sin.mul(
          p.getField(_.instances).put.rMap(_ => (newInstanceNum, ObjectInstance(newInstanceNum, SortedMap.empty))),
          p.getField(_.lastAction).set.rMap(_ => Val(Action(Success, s"instance added: $newInstanceNum")))
        ))
      )(Horizontal()),
      Binder.dynLayout(p.getField(_.instances).structObs) { i =>
        createInstanceTile(presenter, p, i)
      }
    )(Vertical(layoutSettings = LayoutSettings(spacing = true)))
  }

  def createInstanceTile(presenter: ObjectsPanelContext, p: PropertyWithParent[SomeObject], i: PropertyWithParent[ObjectInstance]): Comp =
    Binder.layout(
      Binder.dynLabel(i.map(instance => s"Instance ${instance.id}"), ValoTheme.LABEL_H3),
      Binder.layout(
        Binder.textField("resource name", "", i.getField(_.newResourceName).set.rMap(s => Val(s))),
        Binder.textField("resource value", "", i.getField(_.newResourceValue).set.rMap(s => Val(s))),
        Binder.button("add resource", i.getField(_.resources).put.rMap { _ =>
          val newResourceName = i.getField(_.newResourceName).read.toOpt.get
          val newResourceValue = i.getField(_.newResourceValue).read.toOpt.get
          newResourceName -> SingleResource(newResourceName, newResourceValue)
        })
      )(Horizontal()),
      Binder.dynLayout(i.getField(_.resources).structObs, Form()) { r =>
        r.getCase[SingleResource].map { s =>
          Binder.textField(s.read.name, s.getField(_.value).read, Sin(
            s.getField(_.formValue).set.rMap(Val(_)),
            s.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, "")))))
        }.orElse[Comp] {
          r.getCase[MultiResource].map { m =>
            createMultiResource(presenter, p.read.name, i.read.id, m)
          }
        }.get
      },
      Binder.button("Save", Sin.mul(
        Sin.static { _ =>
          val resourcesSnap = i.getField(_.resources).read
          resourcesSnap.values.flatMap {
            case s: SingleResource =>
              s.formValue.toOpt.map(v =>
                presenter.dmService.setValue(List(p.read.name, i.read.id.toString, s.name), v))
            case m: MultiResource =>
              m.value.values.flatMap(ri =>
                ri.formValue.toOpt.map(v =>
                  presenter.dmService.setValue(List(p.read.name, i.read.id.toString, m.name, ri.idx.toString), v)))
          }
        },
        presenter.model.refresh
      ))
    )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

  def createMultiResource(presenter: ObjectsPanelContext, o: String, instance: Int, m: PropertyWithParent[MultiResource]): Comp =
    Binder.dynLayout(m.getField(_.value).structObs, Form(baseSettings = BaseSettings(m.read.name))) { ri =>
      Binder.textField(ri.read.idx.toString, ri.getField(_.value).read, Sin(
        ri.getField(_.formValue).set.rMap(Val(_)),
        ri.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, ""))))
      )
    }
}
