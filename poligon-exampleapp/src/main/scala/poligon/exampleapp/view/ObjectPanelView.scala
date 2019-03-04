package poligon
package exampleapp.view

import poligon.comp.Comp._
import poligon.comp.CompFamily.{Form, Horizontal, LayoutSettings}
import poligon.exampleapp.services.Services
import poligon.exampleapp.view.ObjectPanelModel._
import poligon.polyproperty.Property.Diff.Val
import poligon.polyproperty.PropertyWithParent

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
        dynLabel(ctx.services.currentTimeService.currentTime.map(_.toString).toObs)
      } else {
        dynLabel(ctx.services.currentTimeService.currentTime.map(_.toString).take(1).toObs)
      }
    }),
    checkBox("Current time on", ctx.currentTimeOn.read, ctx.currentTimeOn.set),
    layout(
      textField("object name", ctx.newObjectName),
      button(
        _ => ctx.newObjectName.read |> (on => ctx.model.put(on, SomeObject(on, SortedMap.empty))),
        "add object")
    )(LayoutSettings(Horizontal)),
    dynLayout(ctx.model.structObs.toLayoutMod(createObjectTile(ctx, _)))
  )()

  def createObjectTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject]): Comp = {
    def newInstanceNum = p.getField(_.newInstanceNumber).read.toOpt.get

    layout(
      layout(
        dynLabel(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), "h2"),
        button(_ => ctx.model.remove(p.read.name), "remove")
      )(LayoutSettings(Horizontal)),
      layout(
        textField("instance number", "", v => p.getField(_.newInstanceNumber).set(Val(v.toInt))),
        button(_ =>
          for {
            _ <- p.getField(_.instances).put(newInstanceNum, ObjectInstance(newInstanceNum, SortedMap.empty))
            _ <- p.getField(_.lastAction).set(Val(Action(ActionStatus.Success, s"instance added: $newInstanceNum")))
          } yield ()
          , "add instance")
      )(LayoutSettings(Horizontal)),
      dynLayout(p.getField(_.instances).structObs.toLayoutMod(createInstanceTile(ctx, p, _)))
    )()
  }

  def createInstanceTile(ctx: ObjectsPanelContext, p: PropertyWithParent[SomeObject], i: PropertyWithParent[ObjectInstance]): Comp =
    layout(
      dynLabel(i.map(instance => s"Instance ${instance.id}"), "h3"),
      layout(
        textField("resource name", "", v => i.getField(_.newResourceName).set(Val(v))),
        textField("resource value", "", v => i.getField(_.newResourceValue).set(Val(v))),
        button(_ => {
          val newResourceName = i.getField(_.newResourceName).read.toOpt.get
          val newResourceValue = i.getField(_.newResourceValue).read.toOpt.get
          i.getField(_.resources).put(newResourceName, SingleResource(newResourceName, newResourceValue))
        }, "add resource")
      )(LayoutSettings(Horizontal)),
      dynLayout(i.getField(_.resources).structObs.toLayoutMod { r =>
        r.getCase[SingleResource].map { s =>
          textField(s.read.name, s.getField(_.value).read, v => for {
            _ <- s.getField(_.formValue).set(Val(v))
            _ <- s.getField(_.lastAction).set(Val(Action(ActionStatus.Draft, "")))
          } yield ())
        }.orElse {
          r.getCase[MultiResource].map { m =>
            createMultiResource(p.read.name, i.read.id, m)
          }
        }.get
      }, LayoutSettings(Form)),
      button(_ => {
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
        ctx.model.refresh
      }, "Save")
    )()

  def createMultiResource(o: String, instance: Int, m: PropertyWithParent[MultiResource]): Comp =
    dynLayout(m.getField(_.value).structObs.toLayoutMod(ri =>
      textField(ri.read.idx.toString, ri.getField(_.value).read, v =>
        for {
          _ <- ri.getField(_.formValue).set(Val(v))
          _ <- ri.getField(_.lastAction).set(Val(Action(ActionStatus.Draft, "")))
        } yield ())
    ), LayoutSettings(Form, caption = m.read.name))
}
