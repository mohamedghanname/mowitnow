object MowItNow extends App {
  val inputStream = getClass.getResourceAsStream("/input.txt")
  val instructions = scala.io.Source.fromInputStream(inputStream).getLines.toList
  val lawn = Lawn(instructions.head)

  // create a list of mowers with their instructions and their initial position and orientation
  val mowers = instructions.tail.grouped(2).map { case List(position, instructions) =>
    Mower(position, instructions, lawn)
  }.toList

  // print the final position of each mower with its orientation and the mower number
  mowers.zipWithIndex.foreach { case (mower, index) =>
    println(s"Tondeuse ${index + 1} : ${mower.toString()}")
  }

}

/**
 * Lawn class represents the lawn where the mowers will move
 * @param x the x coordinate of the top right corner of the lawn
 * @param y the y coordinate of the top right corner of the lawn
 */
case class Lawn(x: Int, y: Int) {
  def contains(x: Int, y: Int): Boolean = x >= 0 && x <= this.x && y >= 0 && y <= this.y
}

/**
 *  Lawn companion object
 */
object Lawn {
  def apply(instructions: String): Lawn = {
    val List(x, y) = instructions.split(" ").map(_.toInt).toList
    Lawn(x, y)
  }
}

/**
 * @param x the x coordinate of the position
 * @param y the y coordinates of the mower
 * @param orientation the orientation of the mower is one of the following : N, E, W, S
 * @param lawn the lawn where the mower is
 */
case class Mower(x: Int, y: Int, orientation: Orientation, lawn: Lawn) {

  def move: Mower = {
    val (x, y) = orientation.move(this.x, this.y)
    if (lawn.contains(x, y)) Mower(x, y, orientation, lawn) else this
  }

  def turnLeft: Mower = Mower(x, y, orientation.turnLeft, lawn)

  def turnRight: Mower = Mower(x, y, orientation.turnRight, lawn)

  override def toString(): String = s"$x $y $orientation"
}

/**
 * Mower companion object
 */
object Mower {
  def apply(position: String, instructions: String, lawn: Lawn): Mower = {
    val List(x, y, orientation) = position.split(" ").toList
    val mower = Mower(x.toInt, y.toInt, Orientation(orientation), lawn)
    instructions.foldLeft(mower) { (mower, instruction) =>
      instruction match {
        case 'A' => mower.move
        case 'G' => mower.turnLeft
        case 'D' => mower.turnRight
      }
    }
  }
}




