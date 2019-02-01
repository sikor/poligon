package poligon.exampleapp.view

import com.vaadin.ui._
import com.vaadin.ui.themes.ValoTheme
import poligon.exampleapp.properties.Binder.BaseSettings
import poligon.exampleapp.properties.Binder.LayoutBuilder.Form
import poligon.exampleapp.properties.{Binder, Comp}
import poligon.exampleapp.view.ObjectsPanelPresenter._
import poligon.polyproperty.Property.Diff.Val
import poligon.polyproperty.PropertyObserver.PropertyObservers
import poligon.polyproperty.{PropertyWithParent, Sin}

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

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Comp = Comp.dynamic { implicit po =>
    val objects = new VerticalLayout()
    objects.setSpacing(true)
    val objectName = new TextField("object name")
    val addObjectButton = new Button("add object")
    addObjectButton.addClickListener(_ => presenter.addObject(objectName.getValue)(po))
    val objectsLabel = new Label("Objects")
    objectsLabel.addStyleName(ValoTheme.LABEL_H1)
    objects.addComponent(objectsLabel)
    objects.addComponent(new HorizontalLayout(objectName, addObjectButton))
    val objectsList = Binder.dynLayout(presenter.model.structObs) { p =>
      createObjectTile(presenter, p)
    }.bind(po)
    objects.addComponent(objectsList)
    objects
  }

  def createObjectTile(presenter: ObjectsPanelPresenter, p: PropertyWithParent[SomeObject]): Comp =
    Comp.dynamic { implicit po: PropertyObservers =>
      val objectTile = new VerticalLayout()
      objectTile.setSpacing(true)
      val removeObjectButton = new Button("remove")
      removeObjectButton.addClickListener(_ => presenter.removeObject(p.read.name)(po))
      val instanceNum = new Slider("instance number")
      val addInstanceButton = new Button("add instance")
      val objectName = Binder.dynLabel(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), ValoTheme.LABEL_H2).bind(po)
      objectTile.addComponent(new HorizontalLayout(objectName, removeObjectButton))
      addInstanceButton.addClickListener(_ => presenter.addInstance(p.read.name, instanceNum.getValue.toInt)(po))
      objectTile.addComponent(new HorizontalLayout(instanceNum, addInstanceButton))
      val instancesList = Binder.dynLayout(p.getField(_.instances).structObs) { i =>
        createInstanceTile(presenter, p, i)
      }.bind(po)
      objectTile.addComponent(instancesList)
      objectTile
    }

  def createInstanceTile(presenter: ObjectsPanelPresenter, p: PropertyWithParent[SomeObject], i: PropertyWithParent[ObjectInstance]): Comp =
    Comp.dynamic { implicit po: PropertyObservers =>
      val instance = new VerticalLayout()
      instance.setSpacing(true)
      instance.addComponent(Binder.dynLabel(i.map(instance => s"Instance ${instance.id}"), ValoTheme.LABEL_H3).bind(po))
      val resourceName = new TextField("resource name")
      val resourceValue = new TextField("resource value")
      val addResourceButton = new Button("add resource")
      addResourceButton.addClickListener(_ => presenter.addResource(p.read.name, i.read.id, resourceName.getValue, resourceValue.getValue)(po))
      instance.addComponent(new HorizontalLayout(resourceName, resourceValue, addResourceButton))
      val resourcesList = Binder.dynLayout(i.getField(_.resources).structObs, Form()) { r =>
        r.getCase[SingleResource].map { s =>
          Binder.textField(s.read.name, s.getField(_.value).read, Sin(
            s.getField(_.formValue).set.rMap(Val(_)),
            s.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, "")))))
        }.orElse[Comp] {
          r.getCase[MultiResource].map { m =>
            createMultiResource(presenter, p.read.name, i.read.id, m)
          }
        }.get
      }.bind(po)
      instance.addComponent(resourcesList)
      val button = new Button("Save")
      button.addClickListener { _ =>
        val resourcesSnap = i.getField(_.resources).read
        resourcesSnap.values.foreach {
          case s: SingleResource =>
            s.formValue.toOpt.foreach(v =>
              presenter.dmService.setValue(List(p.read.name, i.read.id.toString, s.name), v))
          case m: MultiResource =>
            m.value.values.foreach(ri =>
              ri.formValue.toOpt.foreach(v =>
                presenter.dmService.setValue(List(p.read.name, i.read.id.toString, m.name, ri.idx.toString), v)))
        }
        presenter.model.refresh.push(())
      }
      instance.addComponent(button)
      instance
    }

  def createMultiResource(presenter: ObjectsPanelPresenter, o: String, instance: Int, m: PropertyWithParent[MultiResource]): Comp =
    Binder.dynLayout(m.getField(_.value).structObs, Form(baseSettings = BaseSettings(m.read.name))) { ri =>
      Binder.textField(ri.read.idx.toString, ri.getField(_.value).read, Sin(
        ri.getField(_.formValue).set.rMap(Val(_)),
        ri.getField(_.lastAction).set.rMap(_ => Val(Action(ActionStatus.Draft, ""))))
      )
    }
}
