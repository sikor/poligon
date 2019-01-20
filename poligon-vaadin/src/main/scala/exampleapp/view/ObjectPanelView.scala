package exampleapp.view

import com.vaadin.ui._
import com.vaadin.ui.themes.ValoTheme
import exampleapp.properties.Binder.{BaseSettings, LayoutDescription}
import exampleapp.properties.{Binder, Comp}
import exampleapp.view.ObjectsPanelPresenter.{MultiResource, ObjectInstance, SingleResource, SomeObject}
import poligon.polyproperty.Property
import poligon.polyproperty.PropertyObserver.PropertyObservers

//TODO: styling: https://github.com/vaadin/framework/tree/master/uitest/src/main/java/com/vaadin/tests/themes/valo
/*
Plan:
0. Revert Form returning value approach, rewrite form to standard approach
0. Implement MapPropertyCodec
0. SubProperty Macro
1. Handle recursive listeners calls
2. Update action statuses after callback from backend (show memory leaks handling)
3. Move propertyobservers to presenters constructors
4. rename propertywithparent.
6. Nice styling of demo
7. implicit propertyobservers or some more high level way to compose components
8. Class for handling form data with validation
 */
object ObjectPanelView {

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Comp[Unit] = Comp.dynamicUnit { po: PropertyObservers =>
    val objects = new VerticalLayout()
    objects.setSpacing(true)
    val objectName = new TextField("object name")
    val addObjectButton = new Button("add object")
    addObjectButton.addClickListener(_ => presenter.addObject(objectName.getValue)(po))
    val objectsLabel = new Label("Objects")
    objectsLabel.addStyleName(ValoTheme.LABEL_H1)
    objects.addComponent(objectsLabel)
    objects.addComponent(new HorizontalLayout(objectName, addObjectButton))
    val objectsList = Binder.layout(presenter.getModel) { p =>
      createObjectTile(presenter, p)
    }.bind(po)
    objects.addComponent(objectsList.comp)
    objects
  }

  private def createObjectTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject]): Comp[Unit] =
    Comp.dynamicUnit { po: PropertyObservers =>
      val objectTile = new VerticalLayout()
      objectTile.setSpacing(true)
      val removeObjectButton = new Button("remove")
      removeObjectButton.addClickListener(_ => presenter.removeObject(p.get.name)(po))
      val instanceNum = new Slider("instance number")
      val addInstanceButton = new Button("add instance")
      val objectName = Binder.label(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), ValoTheme.LABEL_H2).bind(po)
      objectTile.addComponent(new HorizontalLayout(objectName.comp, removeObjectButton))
      addInstanceButton.addClickListener(_ => presenter.addInstance(p.get.name, instanceNum.getValue.toInt)(po))
      objectTile.addComponent(new HorizontalLayout(instanceNum, addInstanceButton))
      val instancesList = Binder.layout(p.getField(_.ref(_.instances))) { i =>
        createInstanceTile(presenter, p, i)
      }.bind(po)
      objectTile.addComponent(instancesList.comp)
      objectTile
    }

  private def createInstanceTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject], i: Property[ObjectInstance]): Comp[Unit] =
    Comp.dynamicUnit { po: PropertyObservers =>
      val instance = new VerticalLayout()
      instance.setSpacing(true)
      instance.addComponent(Binder.label(i.map(instance => s"Instance ${instance.id}"), ValoTheme.LABEL_H3).bind(po).comp)
      val resourceName = new TextField("resource name")
      val resourceValue = new TextField("resource value")
      val addResourceButton = new Button("add resource")
      addResourceButton.addClickListener(_ => presenter.addResource(p.get.name, i.get.id, resourceName.getValue, resourceValue.getValue)(po))
      instance.addComponent(new HorizontalLayout(resourceName, resourceValue, addResourceButton))
      val resourcesList = Binder.layout(i.getField(_.ref(_.resources)), LayoutDescription.Form()) { r =>
        r.getCase[SingleResource].map { s =>
          Binder.textField(s.get.name, s.map(_.value), newValue => presenter.setSingleResourceValue(p.get.name, i.get.id, s.get.name, newValue)(po))
        }.orElse[Comp[Unit]] {
          r.getCase[MultiResource].map { m =>
            createMultiResource(presenter, p.get.name, i.get.id, m)(po)
          }
        }.get
      }.bind(po)
      instance.addComponent(resourcesList.comp)
      val button = new Button("Save")
      button.addClickListener { _ =>
        presenter.saveResources()
      }
      instance.addComponent(button)
      instance
    }

  private def createMultiResource(presenter: ObjectsPanelPresenter, o: String, instance: Int, m: Property[MultiResource])(po: PropertyObservers): Comp[Unit] =
    Binder.layout(m.getField(_.ref(_.value)), LayoutDescription.Form(BaseSettings(m.get.name))) { ri =>
      Binder.textField(ri.get.idx.toString, ri.map(_.value), newValue => presenter.setMultiResourceValue(o, instance, m.get.name, ri.get.idx, newValue)(po))
    }

}
