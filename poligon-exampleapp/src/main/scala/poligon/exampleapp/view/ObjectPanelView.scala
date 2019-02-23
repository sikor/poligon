package poligon
package exampleapp.view

import poligon.comp.Comp
import poligon.comp.Comp._
import poligon.comp.CompFamily.{Form, Horizontal, LayoutSettings}
import poligon.exampleapp.services.Services
import poligon.exampleapp.view.ObjectPanelModel._
import poligon.polyproperty.Property.Diff.Val
import poligon.polyproperty.{PropertyWithParent, Sin}

import scala.collection.SortedMap

object ObjectPanelView {

  private class ObjectsPanelContext(val services: Services) {
    val model: PropertyWithParent[SortedMap[String, SomeObject]] = PropertyWithParent(() => dmToObjects(services.dmService.getDm))
    val newObjectName = PropertyWithParent("")
    val currentTimeOn = PropertyWithParent(true)
  }

  def create(services: Services): Comp =
    factory(createObjectPanelView(new ObjectsPanelContext(services)))

  def createObjectPanelView(ctx: ObjectsPanelContext): Comp = layout(
    label("Objects", "h1"),
    replaceable(ctx.currentTimeOn.obs.map { isOn =>
      if (isOn) {
        dynLabel(ctx.services.currentTimeService.currentTime.map(_.toString).toObs(ctx.services.scheduler))
      } else {
        dynLabel(ctx.services.currentTimeService.currentTime.map(_.toString).take(1).toObs(ctx.services.scheduler))
      }
    }),
    checkBox("Current time on", ctx.currentTimeOn.read, ctx.currentTimeOn.set),
    layout(
      textField("object name", ctx.newObjectName),
      button(ctx.model.put.rMap(_ => {
        val objectName = ctx.newObjectName.read
        (objectName, SomeObject(objectName, SortedMap.empty))
      }), "add object")
    )(LayoutSettings(Horizontal)),
    dynLayout(ctx.model.structObs.toLayoutMod(createObjectTile(ctx, _)))
  )()

  def createObjectTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject]): Comp = {
    def newInstanceNum = p.getField(_.newInstanceNumber).read.toOpt.get

    layout(
      layout(
        dynLabel(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), "h2"),
        button(ctx.model.remove.rMap(_ => p.read.name), "remove")
      )(LayoutSettings(Horizontal)),
      layout(
        textField("instance number", "", p.getField(_.newInstanceNumber).set
          .rMap(s => Val(s.toInt))),
        button(Sin.combine(
          p.getField(_.instances).put.rMap(_ => (newInstanceNum, ObjectInstance(newInstanceNum, SortedMap.empty))),
          p.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Success, s"instance added: $newInstanceNum")))
        ), "add instance")
      )(LayoutSettings(Horizontal)),
      dynLayout(p.getField(_.instances).structObs.toLayoutMod(createInstanceTile(ctx, p, _)))
    )()
  }

  def createInstanceTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject], i: PropertyWithParent[ObjectInstance]): Comp =
    layout(
      dynLabel(i.map(instance => s"Instance ${instance.id}"), "h3"),
      layout(
        textField("resource name", "", i.getField(_.newResourceName).set.rMap(s => Val(s))),
        textField("resource value", "", i.getField(_.newResourceValue).set.rMap(s => Val(s))),
        button(i.getField(_.resources).put.rMap { _ =>
          val newResourceName = i.getField(_.newResourceName).read.toOpt.get
          val newResourceValue = i.getField(_.newResourceValue).read.toOpt.get
          newResourceName -> SingleResource(newResourceName, newResourceValue)
        }, "add resource")
      )(LayoutSettings(Horizontal)),
      dynLayout(i.getField(_.resources).structObs.toLayoutMod { r =>
        r.getCase[SingleResource].map { s =>
          textField(s.read.name, s.getField(_.value).read, Sin(
            s.getField(_.formValue).set.rMap(Val(_)),
            s.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, "")))))
        }.orElse {
          r.getCase[MultiResource].map { m =>
            createMultiResource(p.read.name, i.read.id, m)
          }
        }.get
      }, LayoutSettings(Form)),
      button(Sin.combine(
        Sin.eval { _ =>
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
    )()

  def createMultiResource(o: String, instance: Int, m: PropertyWithParent[MultiResource]): Comp =
    dynLayout(m.getField(_.value).structObs.toLayoutMod(ri =>
      textField(ri.read.idx.toString, ri.getField(_.value).read, Sin(
        ri.getField(_.formValue).set.rMap(Val(_)),
        ri.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, ""))))
      )), LayoutSettings(Form, caption = m.read.name))
}
