import java.util.concurrent.Executor

object SameThreadExecutor extends Executor {
  override def execute(command: Runnable) = command.run()
}
