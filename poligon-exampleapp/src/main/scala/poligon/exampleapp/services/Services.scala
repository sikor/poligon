package poligon.exampleapp.services

import poligon.polyproperty.PropertyWithParent
import poligon.tran.{HasLanguageSupport, Translator}

class Services(val translator: Translator,
               val executeTasksService: ExecuteTasksService,
               val dmService: DmService,
               val currentTimeService: CurrentTimeService) extends HasLanguageSupport {

  val language: PropertyWithParent[String] = PropertyWithParent("en")
}