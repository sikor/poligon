package databinding

import com.vaadin.ui._
import databinding.ObjectsPanelPresenter._
import databinding.properties.{Binder, Comp}
import io.udash.properties.single.{CastableProperty, ReadableProperty}
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
    val objectsList = new VerticalLayout()
    Binder.bindLayout(presenter.getModel, objectsList) { p =>
      createObjectTile(presenter, p)
    }.bind(po)
  }

  private def createObjectTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject]): Comp = Comp.dynamic { po: PropertyObservers =>
    val instances = new VerticalLayout()
    val removeObjectButton = new Button("remove")
    removeObjectButton.addClickListener(_ => presenter.removeObject(p.get.name)(po))
    val instanceNum = new Slider("instance number")
    val addInstanceButton = new Button("add instance")
    instances.addComponent(Binder.bindSimple(p.map(o => s"${o.name} (status: ${o.lastAction})"), new Label()).bind(po))
    instances.addComponent(removeObjectButton)
    addInstanceButton.addClickListener(_ => presenter.addInstance(p.get.name, instanceNum.getValue.toInt)(po))
    instances.addComponent(instanceNum)
    instances.addComponent(addInstanceButton)
    Binder.bindLayout(p.getSubProperty(_.ref(_.instances)), instances) { i =>
      createInstanceTile(presenter, p, i)
    }.bind(po)
  }

  private def createInstanceTile(presenter: ObjectsPanelPresenter, p: Property[SomeObject], i: Property[ObjectInstance]): Comp = Comp.dynamic { po: PropertyObservers =>
    val resources = new VerticalLayout()
    resources.addComponent(Binder.bindSimple(i.map(_.id.toString), new Label()).bind(po))
    val resourceName = new TextField("resource name")
    val resourceValue = new TextField("resource value")
    val addResourceButton = new Button("add resource")
    addResourceButton.addClickListener(_ => presenter.addResource(p.get.name, i.get.id, resourceName.getValue, resourceValue.getValue))
    resources.addComponent(resourceName)
    resources.addComponent(resourceValue)
    resources.addComponent(addResourceButton)
    Binder.bindLayoutStructure(i.transformToSeq((oi: ObjectInstance) => oi.resources), resources) { r =>
      createResourceTile(presenter, p, i, r)
    }
  }

  private def createResourceTile(
                                  presenter: ObjectsPanelPresenter,
                                  p: CastableProperty[SomeObject],
                                  i: ReadableProperty[ObjectInstance],
                                  r: ReadableProperty[Resource]) = {
    val prop = r.transform {
      case SingleResource(name, value, status) =>
        s"${p.get.name}/${i.get.id}/$name = $value (status: $status)"
      case MultiResource(name, values, status) =>
        s"${p.get.name}/${i.get.id}/$name = $values (status: $status)"
    }
    val label = Binder.bindVaadinProperty(prop, new Label())
    val field = new TextField("new value")
    val button = new Button("set")
    button.addClickListener(_ => presenter.setResourceValue(p.get.name, i.get.id, r.get.name, r.get match {
      case _: SingleResource => None
      case m: MultiResource => Some(m.value.headOption.map(_._1).getOrElse(0))
    }, field.getValue))
    new HorizontalLayout(label, field, button)
  }
}
