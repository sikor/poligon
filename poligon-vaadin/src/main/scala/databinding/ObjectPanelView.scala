package databinding

import com.vaadin.ui._
import com.vaadin.ui.themes.ValoTheme
import databinding.ObjectsPanelPresenter._
import databinding.properties.Binder.LayoutDescription
import databinding.properties.{Binder, Comp}
import poligon.polyproperty.Property
import poligon.polyproperty.PropertyObserver.PropertyObservers

//TODO: styling: https://github.com/vaadin/framework/tree/master/uitest/src/main/java/com/vaadin/tests/themes/valo
/*
Plan:
0. Handle forms - use case: form divided to many components, submit button is in one of them
1. Use Union property for single/multi resources
2. Update action statuses after callback from backend (show memory leaks handling)
3. Move propertyobservers to presenters constructors
4. Move methods for property listening to Obs, rename propertywithparent.
5. Components builders
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
      val instancesList = Binder.layout(p.getSubProperty(_.ref(_.instances))) { i =>
        createInstanceTile(presenter, p, i)
      }.bind(po)
      objectTile.addComponent(instancesList.comp)
      objectTile
    }

  case class ResourceAction(o: String, instance: Int, resource: String, resourceInstance: Option[Int], value: String)

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
      val resourcesList = Binder.layout(i.getSubProperty(_.ref(_.resources)), LayoutDescription.Form) { r =>
        r.getCase[SingleResource].map { s =>
          createSingleResource(s).map(value => Seq(ResourceAction(p.get.name, i.get.id, s.get.name, None, value)))
        }.orElse {
          r.getCase[MultiResource].map { m =>
            createMultiResource(m).map(value => value.map(ri => ResourceAction(p.get.name, i.get.id, m.get.name, Some(ri.idx), ri.value)))
          }
        }.get
      }.bind(po)
      instance.addComponent(resourcesList.comp)
      val button = new Button("Save")
      button.addClickListener { _ =>
        resourcesList.get.flatten.foreach(ra =>
          presenter.setResourceValue(ra.o, ra.instance, ra.resource, ra.resourceInstance, ra.value)(po)
        )
      }
      instance.addComponent(button)
      instance
    }

  private def createSingleResource(s: Property[SingleResource]): Comp[String] = Comp.dynamic[String] { po: PropertyObservers =>
    val field = new TextField()
    var wasOverwritten: Boolean = false
    field.addValueChangeListener(_ => wasOverwritten = true)
    s.listen(r => {
      field.setCaption(r.name)
      //overwrite field value only if not modified via form
      if (!wasOverwritten) {
        field.setValue(r.value)
      }
    }, init = true)(po)
    Comp.bound(field.getValue, field)
  }

  private def createMultiResource(m: Property[MultiResource]): Comp[Seq[ResourceInstance]] =
    Binder.layout(m.getSubProperty(_.ref(_.value)), LayoutDescription.Form) { ri =>
      Comp.dynamic[ResourceInstance] { po: PropertyObservers =>
        val field = new TextField()
        var wasOverwritten = false
        field.addValueChangeListener(_ => wasOverwritten = true)
        field.setCaption(s"${m.get.name}/${ri.get.idx}")
        ri.getSubProperty(_.ref(_.value)).listen(v => if (!wasOverwritten) {
          field.setValue(v)
        }, init = true)(po)
        Comp.bound(ResourceInstance(ri.get.idx, field.getValue), field)
      }
    }

}
