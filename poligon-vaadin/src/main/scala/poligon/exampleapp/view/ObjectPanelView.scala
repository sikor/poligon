package poligon
package exampleapp.view

import com.vaadin.ui.themes.ValoTheme
import poligon.exampleapp.HttpServer.Services
import poligon.exampleapp.properties.Binder.LayoutBuilder.{Form, Horizontal, Vertical}
import poligon.exampleapp.properties.Binder.{BaseSettings, LayoutSettings}
import poligon.exampleapp.properties.{Binder, Comp}
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

  private class ObjectsPanelContext(val services: Services) {
    val model: PropertyWithParent[SortedMap[String, SomeObject]] = PropertyWithParent(() => dmToObjects(services.dmService.getDm))
    val newObjectName = PropertyWithParent("")
  }

  def create(services: Services): Comp =
    Comp.factory(createObjectPanelView(new ObjectsPanelContext(services)))

  def createObjectPanelView(ctx: ObjectsPanelContext): Comp = Binder.layout(
    Binder.label("Objects", ValoTheme.LABEL_H1),
    Binder.dynLabel(ctx.services.currentTimeService.currentTime.map(_.toString).toObs(ctx.services.scheduler)),
    Binder.layout(
      Binder.textField("object name", ctx.newObjectName),
      Binder.button(ctx.model.put.rMap(_ => {
        val objectName = ctx.newObjectName.read
        (objectName, SomeObject(objectName, SortedMap.empty))
      }), "add object")
    )(Horizontal()),
    Binder.dynLayout(ctx.model.structObs) { p =>
      createObjectTile(ctx, p)
    }
  )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

  def createObjectTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject]): Comp = {
    def newInstanceNum = p.getField(_.newInstanceNumber).read.toOpt.get

    Binder.layout(
      Binder.layout(
        Binder.dynLabel(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), ValoTheme.LABEL_H2),
        Binder.button(ctx.model.remove.rMap(_ => p.read.name), "remove")
      )(Horizontal()),
      Binder.layout(
        Binder.textField("instance number", "", p.getField(_.newInstanceNumber).set
          .rMap(s => Val(s.toInt))),
        Binder.button(Sin.mul(
          p.getField(_.instances).put.rMap(_ => (newInstanceNum, ObjectInstance(newInstanceNum, SortedMap.empty))),
          p.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Success, s"instance added: $newInstanceNum")))
        ), "add instance")
      )(Horizontal()),
      Binder.dynLayout(p.getField(_.instances).structObs) { i =>
        createInstanceTile(ctx, p, i)
      }
    )(Vertical(layoutSettings = LayoutSettings(spacing = true)))
  }

  def createInstanceTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject], i: PropertyWithParent[ObjectInstance]): Comp =
    Binder.layout(
      Binder.dynLabel(i.map(instance => s"Instance ${instance.id}"), ValoTheme.LABEL_H3),
      Binder.layout(
        Binder.textField("resource name", "", i.getField(_.newResourceName).set.rMap(s => Val(s))),
        Binder.textField("resource value", "", i.getField(_.newResourceValue).set.rMap(s => Val(s))),
        Binder.button(i.getField(_.resources).put.rMap { _ =>
          val newResourceName = i.getField(_.newResourceName).read.toOpt.get
          val newResourceValue = i.getField(_.newResourceValue).read.toOpt.get
          newResourceName -> SingleResource(newResourceName, newResourceValue)
        }, "add resource")
      )(Horizontal()),
      Binder.dynLayout(i.getField(_.resources).structObs, Form()) { r =>
        r.getCase[SingleResource].map { s =>
          Binder.textField(s.read.name, s.getField(_.value).read, Sin(
            s.getField(_.formValue).set.rMap(Val(_)),
            s.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, "")))))
        }.orElse[Comp] {
          r.getCase[MultiResource].map { m =>
            createMultiResource(ctx, p.read.name, i.read.id, m)
          }
        }.get
      },
      Binder.button(Sin.mul(
        Sin.static { _ =>
          val resourcesSnap = i.getField(_.resources).read
          resourcesSnap.values.flatMap {
            case s: SingleResource =>
              s.formValue.toOpt.map(v =>
                ctx.services.dmService.setValue(List(p.read.name, i.read.id.toString, s.name), v))
            case m: MultiResource =>
              m.value.values.flatMap(ri =>
                ri.formValue.toOpt.map(v =>
                  ctx.services.dmService.setValue(List(p.read.name, i.read.id.toString, m.name, ri.idx.toString), v)))
          }
        },
        ctx.model.refresh
      ), "Save")
    )(Vertical(layoutSettings = LayoutSettings(spacing = true)))

  def createMultiResource(ctx: ObjectsPanelContext, o: String, instance: Int, m: PropertyWithParent[MultiResource]): Comp =
    Binder.dynLayout(m.getField(_.value).structObs, Form(baseSettings = BaseSettings(m.read.name))) { ri =>
      Binder.textField(ri.read.idx.toString, ri.getField(_.value).read, Sin(
        ri.getField(_.formValue).set.rMap(Val(_)),
        ri.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, ""))))
      )
    }
}
