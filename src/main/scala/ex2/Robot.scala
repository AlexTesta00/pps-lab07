package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, initialBattery: Int, costPerAction: Int) extends Robot:
  require(initialBattery >= 0, "Initial battery level must be non-negative")
  require(costPerAction > 0, "Cost per action must be positive")

  private var currentBattery = initialBattery

  def batteryLevel: Int = currentBattery

  export robot.{position, direction, turn}

    override def act(): Unit =
      if currentBattery > 0 then {
        robot.act()
        currentBattery = math.max(0, currentBattery - costPerAction)
      }

  override def toString: String = s"${robot.toString} (battery=$currentBattery)"

class RobotCanFail(val robot: Robot, failureProbability: Double) extends Robot:
  require(failureProbability >= 0.0 && failureProbability <= 1.0, "Failure probability must be between 0 and 1")

  export robot.{position, direction, turn}

  override def act(): Unit =
    val failed = scala.util.Random.nextDouble() < failureProbability
    if !failed then robot.act()

  override def toString: String = s"${robot.toString} (failure probability=$failureProbability)"

class RobotRepeated(val robot: Robot, times: Int) extends Robot:

  require(times >= 0, "times must be non-negative")

  export robot.{position, direction, turn}

  override def act(): Unit = for _ <- 1 to times do robot.act()

  override def toString: String = s"${robot.toString} (repeated=$times)"

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
