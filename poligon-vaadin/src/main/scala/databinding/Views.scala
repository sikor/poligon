package databinding

import com.vaadin.ui._
import databinding.ObjectsPanelPresenter._
import databinding.properties.Binder

object Views {

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Component = {
    val objects = new VerticalLayout()
    val objectName = new TextField("object name")
    val addObjectButton = new Button("add object")
    addObjectButton.addClickListener(_ => presenter.addObject(objectName.getValue))
    objects.addComponent(objectName)
    objects.addComponent(addObjectButton)
    Binder.bindLayoutStructure(presenter.getModel, objects) { p =>
      val instances = new VerticalLayout()
      instances.addComponent(Binder.bindVaadinProperty(p.transform(_.name), new Label()))
      val instanceNum = new Slider("instance number")
      val addInstanceButton = new Button("add instance")
      addInstanceButton.addClickListener(_ => presenter.addInstance(p.get.name, instanceNum.getValue.toInt))
      instances.addComponent(instanceNum)
      instances.addComponent(addInstanceButton)
      Binder.bindLayoutStructure(p.transformToSeq((i: SomeObject) => i.instances), instances) { i =>
        val resources = new VerticalLayout()
        resources.addComponent(Binder.bindVaadinProperty(i.transform(_.id.toString), new Label()))
        val resourceName = new TextField("resource name")
        val resourceValue = new TextField("resource value")
        val addResourceButton = new Button("add resource")
        addResourceButton.addClickListener(_ => presenter.addResource(p.get.name, i.get.id, resourceName.getValue, resourceValue.getValue))
        resources.addComponent(resourceName)
        resources.addComponent(resourceValue)
        resources.addComponent(addResourceButton)
        Binder.bindLayoutStructure(i.transformToSeq((oi: ObjectInstance) => oi.resources), resources) { r =>
          val prop = r.transform {
            case SingleResource(name, value) =>
              s"${p.get.name}/${i.get.id}/$name = $value"
            case MultiResource(name, values) =>
              s"${p.get.name}/${i.get.id}/$name = $values"
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
    }
  }


}
