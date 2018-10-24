package databinding


object ExecuteTasksPresenter {

  sealed trait ExecuteTasksStatus

  object ExecuteTasksStatus {

    case object NotStarted extends ExecuteTasksStatus

    case object InProgress extends ExecuteTasksStatus

    case object Success extends ExecuteTasksStatus

    case object Failed extends ExecuteTasksStatus

  }

}

class ExecuteTasksPresenter {

}
