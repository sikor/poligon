package databinding

import com.vaadin.ui._
import com.vaadin.ui.themes.ValoTheme
import databinding.ObjectsPanelPresenter._
import databinding.properties.{Binder, Comp}
import poligon.polyproperty.Property
import poligon.polyproperty.PropertyObserver.PropertyObservers

//TODO: styling: https://github.com/vaadin/framework/tree/master/uitest/src/main/java/com/vaadin/tests/themes/valo
object ObjectPanelView {

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Comp = Comp.dynamic { po: PropertyObservers =>
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
    objects.addComponent(objectsList)
    objects
  }

  private def createObjectTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject]): Comp = Comp.dynamic { po: PropertyObservers =>
    val objectTile = new VerticalLayout()
    objectTile.setSpacing(true)
    val removeObjectButton = new Button("remove")
    removeObjectButton.addClickListener(_ => presenter.removeObject(p.get.name)(po))
    val instanceNum = new Slider("instance number")
    val addInstanceButton = new Button("add instance")
    val objectName = Binder.label(p.map(o => s"Object ${o.name} (status: ${o.lastAction})"), ValoTheme.LABEL_H2).bind(po)
    objectTile.addComponent(new HorizontalLayout(objectName, removeObjectButton))
    addInstanceButton.addClickListener(_ => presenter.addInstance(p.get.name, instanceNum.getValue.toInt)(po))
    objectTile.addComponent(new HorizontalLayout(instanceNum, addInstanceButton))
    val instancesList = Binder.layout(p.getSubProperty(_.ref(_.instances))) { i =>
      createInstanceTile(presenter, p, i)
    }.bind(po)
    objectTile.addComponent(instancesList)
    objectTile
  }

  private def createInstanceTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject], i: Property[ObjectInstance]): Comp = Comp.dynamic { po: PropertyObservers =>
    val resources = new VerticalLayout()
    resources.setSpacing(true)
    resources.addComponent(Binder.label(i.map(instance => s"Instance ${instance.id}"), ValoTheme.LABEL_H3).bind(po))
    val resourceName = new TextField("resource name")
    val resourceValue = new TextField("resource value")
    val addResourceButton = new Button("add resource")
    addResourceButton.addClickListener(_ => presenter.addResource(p.get.name, i.get.id, resourceName.getValue, resourceValue.getValue)(po))
    resources.addComponent(new HorizontalLayout(resourceName, resourceValue, addResourceButton))
    val resourcesList = Binder.layout(i.getSubProperty(_.ref(_.resources))) { r =>
      createResourceTile(presenter, p, i, r)
    }.bind(po)
    resources.addComponent(resourcesList)
    resources
  }

  private def createResourceTile(
                                  presenter: ObjectsPanelPresenter,
                                  p: Property[SomeObject],
                                  i: Property[ObjectInstance],
                                  r: Property[Resource]): Comp = Comp.dynamic { po: PropertyObservers =>
    val prop = r.map {
      case SingleResource(name, value, status) =>
        s"${p.get.name}/${i.get.id}/$name = $value (status: $status)"
      case MultiResource(name, values, status) =>
        s"${p.get.name}/${i.get.id}/$name = $values (status: $status)"
    }
    val resource = Binder.label(prop).bind(po)
    val field = new TextField("new value")
    val button = new Button("set")

    def resourceId: Option[Int] = {
      r.get match {
        case _: SingleResource => None
        case m: MultiResource => Some(m.value.headOption.map(_.idx).getOrElse(0))
      }
    }

    button.addClickListener(_ => presenter.setResourceValue(p.get.name, i.get.id, r.get.name, resourceId, field.getValue)(po))
    new HorizontalLayout(resource, field, button)
  }
}
