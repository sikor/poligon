package poligon.exampleapp.services

import poligon.tran.Translator

class Services(val translator: Translator,
               val executeTasksService: ExecuteTasksService,
               val dmService: DmService,
               val currentTimeService: CurrentTimeService)