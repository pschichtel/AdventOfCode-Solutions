package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration

object Day12 extends AoCApp {

    case class Body(x: Int, y: Int, z: Int, vx: Int, vy: Int, vz: Int) {
        lazy val potentialEnergy: Int = math.abs(x) + math.abs(y) + math.abs(z)
        lazy val kineticEnergy: Int = math.abs(vx) + math.abs(vy) + math.abs(vz)
        lazy val energy: Int = potentialEnergy * kineticEnergy

        lazy val translated: Body =
            Body(x + vx, y + vy, z + vz, vx, vy, vz)

        def accelerate(x: Int, y: Int, z: Int): Body =
            Body(this.x, this.y, this.z, vx + x, vy + y, vz + z)
    }

    val bodyDef = "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>".r

    val initialBodies = asLines(Input2019.Day12).map {
        case bodyDef(x, y, z) => Body(x.toInt, y.toInt, z.toInt, 0, 0, 0)
    }

    val finalState = simulate(initialBodies).drop(1000).head
    part(1, finalState.map(_.energy).sum)

    def component(b: Body => Int): Body => Int = b

    val componentRepeatsAt = Await.result(Future.sequence(Seq(component(_.x), component(_.y), component(_.z)).map { d =>
        Future(simulateUntilRepeat(initialBodies, d, Set.empty, 0))
    }), Duration.Inf)
    part(2, lcm(componentRepeatsAt))

    @tailrec
    def gcd[T](a: T, b: T)(implicit int: Integral[T]): T =
        if (b == int.zero) int.abs(a)
        else gcd(b, int.rem(a, b))

    def lcm[T](numbers: Seq[T])(implicit int: Integral[T]): T =
        numbers.foldLeft(int.one)((a, b) => int.times(int.quot(a, gcd(a, b)), b))

    @tailrec
    def simulateUntilRepeat(bodies: Seq[Body], dim: Body => Int, seen: Set[Seq[Int]], i: Int): Int = {
        val dimValues = bodies.map(dim)
        if (seen.contains(dimValues)) i
        else simulateUntilRepeat(simulateStep(bodies), dim, seen + dimValues, i + 1)
    }

    def simulate(bodies: Seq[Body]): LazyList[Seq[Body]] = {
        bodies #:: simulate(simulateStep(bodies))
    }

    def simulateStep(bodies: Seq[Body]): Seq[Body] = {
        updatePositions(updateVelocities(bodies))
    }

    def printState(name: String, bodies: Seq[Body]): Unit = {
        println(s"State $name")
        for (body <- bodies) {
            println(s"pos=<x=${body.x}, y=${body.y}, z=${body.z}>, vel=<x=${body.vx}, y=${body.vy}, z=${body.vz}>, energy=${body.energy}")
        }
        println(s"Total energy: ${bodies.map(_.energy).sum}")
        println()
    }

    def updateVelocities(bodies: Seq[Body]): Seq[Body] = {
        for (updatee <- bodies) yield {
            bodies.filter(_ != updatee).foldLeft(updatee) { (updatee, other) =>
                val dx = clamp(other.x - updatee.x, -1, 1)
                val dy = clamp(other.y - updatee.y, -1, 1)
                val dz = clamp(other.z - updatee.z, -1, 1)

                updatee.accelerate(dx, dy, dz)
            }
        }
    }

    def clamp(value: Int, lower: Int, upper: Int): Int =
        if (value < lower) lower
        else if (value > upper) upper
        else value

    def updatePositions(bodies: Seq[Body]): Seq[Body] = {
        for (updatee <- bodies) yield {
            updatee.translated
        }
    }

}
