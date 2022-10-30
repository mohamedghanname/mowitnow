/**
 *  Orientation trait represents the orientation of the mower and the move function
 */
sealed trait Orientation {
  def move(x: Int, y: Int): (Int, Int)

  def turnLeft: Orientation

  def turnRight: Orientation
}

/**
 *  North orientation object
 */
case object North extends Orientation {
  override def move(x: Int, y: Int): (Int, Int) = (x, y + 1)

  override def turnLeft: Orientation = West

  override def turnRight: Orientation = East

  override def toString: String = "N"
}

/**
 *  East orientation object
 */
case object East extends Orientation {
  override def move(x: Int, y: Int): (Int, Int) = (x + 1, y)

  override def turnLeft: Orientation = North

  override def turnRight: Orientation = South

  override def toString: String = "E"
}

/**
 *  South orientation object
 */
case object South extends Orientation {
  override def move(x: Int, y: Int): (Int, Int) = (x, y - 1)

  override def turnLeft: Orientation = East

  override def turnRight: Orientation = West

  override def toString: String = "S"
}

/**
 *  West orientation object
 */
case object West extends Orientation {
  override def move(x: Int, y: Int): (Int, Int) = (x - 1, y)

  override def turnLeft: Orientation = South

  override def turnRight: Orientation = North

  override def toString: String = "W"
}

/**
 *  Orientation companion object
 */
object Orientation {
  def apply(orientation: String): Orientation = orientation match {
    case "N" => North
    case "E" => East
    case "S" => South
    case "W" => West
  }
}