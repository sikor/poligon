package poligon.exampleapp.services

import poligon.tran.Translator

import scala.concurrent.Future

class Services(val translator: Translator[Future],
               val executeTasksService: ExecuteTasksService,
               val dmService: DmService,
               val currentTimeService: CurrentTimeService)