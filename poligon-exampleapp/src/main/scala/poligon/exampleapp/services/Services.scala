package poligon.exampleapp.services

import monix.execution.Scheduler

class Services(val executeTasksService: ExecuteTasksService,
               val dmService: DmService,
               val currentTimeService: CurrentTimeService,
               val scheduler: Scheduler)