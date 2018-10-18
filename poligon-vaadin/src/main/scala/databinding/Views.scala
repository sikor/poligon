package databinding

import com.vaadin.ui._
import databinding.ObjectsPanelPresenter._
import databinding.properties.Binder
import io.udash.properties.single.ReadableProperty

object Views {

  def createObjectPanelView(presenter: ObjectsPanelPresenter): Component = {

    val objects = new VerticalLayout()
    Binder.bind(presenter.getModel, objects, { p: ReadableProperty[SomeObject] =>
      val instances = new VerticalLayout()
      Binder.bind(p.transformToSeq[ObjectInstance]((i: SomeObject) => i.instances), instances, { i: ReadableProperty[ObjectInstance] =>
        val resources = new VerticalLayout()
        Binder.bind(i.transformToSeq((oi: ObjectInstance) => oi.resources), resources, { r: ReadableProperty[Resource] =>
          val label = r.get match {
            case SingleResource(name, value) =>
              new Label(s"${p.get.name}/${i.get.id}/$name = $value")
            case MultiResource(name, values) =>
              new Label(s"${p.get.name}/${i.get.id}/$name = $values")
          }
          val field = new TextField("new value")
          val button = new Button("set")
          button.addClickListener(l => presenter.setResourceValue(p.get.name, i.get.id, r.get.name, None, field.getValue))
          val l = new HorizontalLayout(label, field, button)
          l.setSpacing(true)
          l
        })
        resources
      })
      instances
    })
    objects
  }


}
