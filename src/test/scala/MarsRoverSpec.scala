import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MarsRoverSpec extends AnyWordSpec with Matchers {

  "MarsRover" should {
    "fail to start when latitude is invalid" in {
      intercept[IllegalArgumentException](MarsRover(11, 10, East, Grid(10, 10), (0,0))).getMessage shouldBe
        "requirement failed: Latitude position not on grid"
    }

    "fail to start when longitude is invalid" in {
      intercept[IllegalArgumentException](MarsRover(10, -1, East, Grid(10, 10), (0,0))).getMessage shouldBe
        "requirement failed: Longitude position not on grid"
    }
  }

  "moveforward" should {
    "increase the Rover'e long when direction is North" in {
      MarsRover(0, 0, North, Grid(5, 5), (0,0)).moveForward(3).long shouldBe 3
    }

    "Move the Rover to the opposite side of the map when distance to move exceeds bounds of the grid (North)" in {
      MarsRover(0, 51, North, Grid(10, 100), (0,0)).moveForward(50).long shouldBe 0
    }

    "decrease the Rover's long when direction is South" in {
      MarsRover(0, 5, South, Grid(5, 5), (0,0)).moveForward(3).long shouldBe 2
    }

    "Move the Rover to the opposite side of the map when distance to move exceeds bounds of the grid (South)" in {
      MarsRover(5, 50, South, Grid(100, 100), (0,0)).moveForward(51).long shouldBe 100
    }

    "increase the Rover's lat when direction is East"in {
      MarsRover(0, 0, East, Grid(4, 3), (0,0)).moveForward(2).lat shouldBe 2
    }

    "Move the Rover to the opposite side of the map when distance to move exceeds bounds of the grid (East)" in {
      MarsRover(50, 50, East, Grid(100, 100), (0,0)).moveForward(52).lat shouldBe 1
    }

    "decrease the Rover's lat when direction is West" in {
      MarsRover(12, 6, West, Grid(13, 13), (0,0)).moveForward(8).lat shouldBe 4
    }

    "Move the Rover to the opposite side of the map when distance to move exceeds bounds of the grid (West)" in {
      MarsRover(50, 50, West, Grid(100, 100), (0,0)).moveForward(52).lat shouldBe 99
    }
  }

  "rotateClockWise" should {
    List(
      (North, East),
      (East, South),
      (South, West),
      (West, North)
    ).foreach {
      case(originalDirection, newDirection) =>
        s"point $newDirection when starting Direction is $originalDirection" in {
          MarsRover(0,0, originalDirection, Grid(10, 11), (0,0)).rotateClockwise.direction shouldBe newDirection
        }
    }
  }

  "rotateAntoClockWise" should {
    List(
      (North, West),
      (East, North),
      (South, East),
      (West, South)
    ).foreach {
      case(originalDirection, newDirection) =>
        s"point $newDirection when starting Direction is $originalDirection" in {
          MarsRover(0,0, originalDirection, Grid(10, 11), (0,0)).rotateAntiClockwise.direction shouldBe newDirection
        }
    }
  }

  "autoPilot" should {
    "return MarsRover where lat and long match the target location" when {
      "MarsRover is already at it's target Location" in {
        val rover = MarsRover(11, 64, North, Grid(100, 100), (11, 64))
        rover.autoPilot(true).lat shouldBe rover.targetLocation._1
        rover.autoPilot(true).long shouldBe rover.targetLocation._2
      }
    }

    "return default MarsRover where Rover hasn't yet reached its destination" in {
      MarsRover(6, 4, West, Grid(10, 10), (0, 0)).autoPilot(false) shouldBe MarsRover(0,0, East, Grid(10, 10), (0,0))
    }
  }

}
