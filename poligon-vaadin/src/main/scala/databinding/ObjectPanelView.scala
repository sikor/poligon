package databinding

import com.vaadin.ui._
import databinding.ObjectsPanelPresenter._
import databinding.properties.{Binder, Comp}
import poligon.polyproperty.Property
import poligon.polyproperty.PropertyObserver.PropertyObservers

object ObjectPanelView {

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Comp = Comp.dynamic { po: PropertyObservers =>
    val objects = new VerticalLayout()
    val objectName = new TextField("object name")
    val addObjectButton = new Button("add object")
    addObjectButton.addClickListener(_ => presenter.addObject(objectName.getValue)(po))
    objects.addComponent(objectName)
    objects.addComponent(addObjectButton)
    val objectsList = Binder.layout(presenter.getModel) { p =>
      createObjectTile(presenter, p)
    }.bind(po)
    objects.addComponent(objectsList)
    objects
  }

  private def createObjectTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject]): Comp = Comp.dynamic { po: PropertyObservers =>
    val instances = new VerticalLayout()
    val removeObjectButton = new Button("remove")
    removeObjectButton.addClickListener(_ => presenter.removeObject(p.get.name)(po))
    val instanceNum = new Slider("instance number")
    val addInstanceButton = new Button("add instance")
    instances.addComponent(Binder.label(p.map(o => s"${o.name} (status: ${o.lastAction})")).bind(po))
    instances.addComponent(removeObjectButton)
    addInstanceButton.addClickListener(_ => presenter.addInstance(p.get.name, instanceNum.getValue.toInt)(po))
    instances.addComponent(instanceNum)
    instances.addComponent(addInstanceButton)
    val instancesList = Binder.layout(p.getSubProperty(_.ref(_.instances))) { i =>
      createInstanceTile(presenter, p, i)
    }.bind(po)
    instances.addComponent(instancesList)
    instances
  }

  private def createInstanceTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject], i: Property[ObjectInstance]): Comp = Comp.dynamic { po: PropertyObservers =>
    val resources = new VerticalLayout()
    resources.addComponent(Binder.label(i.map(_.id.toString)).bind(po))
    val resourceName = new TextField("resource name")
    val resourceValue = new TextField("resource value")
    val addResourceButton = new Button("add resource")
    addResourceButton.addClickListener(_ => presenter.addResource(p.get.name, i.get.id, resourceName.getValue, resourceValue.getValue)(po))
    resources.addComponent(resourceName)
    resources.addComponent(resourceValue)
    resources.addComponent(addResourceButton)
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
    val label = Binder.label(prop).bind(po)
    val field = new TextField("new value")
    val button = new Button("set")

    def resourceId: Option[Int] = {
      r.get match {
        case _: SingleResource => None
        case m: MultiResource => Some(m.value.headOption.map(_.idx).getOrElse(0))
      }
    }

    button.addClickListener(_ => presenter.setResourceValue(p.get.name, i.get.id, r.get.name, resourceId, field.getValue)(po))
    new HorizontalLayout(label, field, button)
  }
}
