
case class MarsRover(lat: Int, long: Int, direction: Compass, gps: Grid, targetLocation: (Int, Int)) {
  require(lat <= gps.rows && lat > -1, "Latitude position not on grid")
  require(long <= gps.columns && long > -1, "Longitude position not on grid")

  def moveForward(distance: Int): MarsRover = {
    direction match {
      case North => copy(long = positionUpdater(long + distance))
      case South => copy(long = positionUpdater(long - distance))
      case East => copy(lat = positionUpdater(lat + distance))
      case West => copy(lat = positionUpdater(lat - distance))
    }
  }

  private def positionUpdater(distance: Int): Int = {
    direction match {
      case North if distance > gps.columns => distance - gps.columns - 1
      case South if distance < 0 => distance + gps.columns + 1
      case East if distance > gps.rows => distance - gps.rows - 1
      case West if distance < 0 => gps.rows + distance + 1
      case _ => distance
    }
  }

  def rotateClockwise: MarsRover =
    direction match {
      case North => copy(direction = East)
      case East => copy(direction = South)
      case South => copy(direction = West)
      case West => copy(direction = North)
    }

  def rotateAntiClockwise: MarsRover =
    direction match {
      case North => copy(direction = West)
      case East => copy(direction = North)
      case South => copy(direction = East)
      case West => copy(direction = South)
    }

  def autoPilot(targetReached: Boolean): MarsRover =
    if (targetReached) this else MarsRover(0,0, East, gps, targetLocation)
}

case class Grid(rows: Int, columns: Int)
