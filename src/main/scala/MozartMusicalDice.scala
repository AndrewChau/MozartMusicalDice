/**
  * Kwan Yin Andrew Chau 2018
  */

import java.text.{DecimalFormat, NumberFormat}

import scala.io.Source
import scala.util.Random

object MozartMusicalDice {

    val mozartSystem = Vector(
        Vector(96, 32, 69, 40, 148, 104, 152, 119, 98, 3, 54),
        Vector(22, 6, 95, 17, 74, 157, 60, 84, 142, 87, 130),
        Vector(141, 128, 158, 113, 163, 27, 171, 114, 42, 165, 10),
        Vector(41, 63, 13, 85, 45, 167, 53, 50, 156, 61, 103),
        Vector(105, 146, 153, 161, 80, 154, 99, 140, 75, 135, 28),
        Vector(122, 46, 55, 2, 97, 68, 133, 86, 129, 47, 37),
        Vector(11, 134, 110, 159, 36, 118, 21, 169, 62, 147, 106),
        Vector(30, 81, 24, 100, 107, 91, 127, 94, 123, 33, 5),
        Vector(70, 117, 66, 90, 25, 138, 16, 120, 65, 102, 35),
        Vector(121, 39, 136, 176, 143, 71, 155, 88, 77, 4, 20),
        Vector(26, 126, 15, 7, 64, 150, 57, 48, 19, 31, 108),
        Vector(9, 56, 132, 34, 125, 29, 175, 166, 82, 164, 92),
        Vector(112, 174, 73, 67, 76, 101, 43, 51, 137, 144, 12),
        Vector(49, 18, 58, 160, 136, 162, 168, 115, 38, 59, 124),
        Vector(109, 116, 145, 52, 1, 23, 89, 72, 149, 173, 44),
        Vector(14, 83, 79, 170, 93, 151, 172, 111, 8, 78, 131)
    )

    val beats: List[BeatLine] = Source.fromFile(s"${System.getProperty("user.dir")}/mozart-dice-starting.txt").getLines
            .toList.map(s => {
        val line = s.split(" ")
        BeatLine(line(0), line(1).toDouble, line(2).toDouble)
    })

    def main(args: Array[String]): Unit = {
        val measuresToUse = mozartSystem.indices map(i => {
            val diceRoll = rollDices
            println(s"Rolled: $diceRoll")
            mozartSystem(i)(diceRoll - 2)
        })
        printComposition(measuresToUse)
    }

    def getComposition(measuresToUse: Seq[Int]): Seq[String] = {
        measuresToUse.zipWithIndex.flatMap {
            case (measure, i) => getBeatsForMeasure(measure, i)
        }
    }

    private def getBeatsForMeasure(measure: Int, index: Int): List[String] = {
        val lastBeat = measure * 3
        val firstBeat = lastBeat - 3
        beats.filter(b => b.startBeat >= firstBeat && b.startBeat < lastBeat).map(b => {
            b.copy(startBeat = b.startBeat - firstBeat + (3 * index))
        }.toLine)
    }

    private def rollDices: Int = 2 + Random.nextInt(10)

    def printComposition(measuresToUse: Seq[Int]): Unit = {
        println(s"Measures to use: $measuresToUse")
        val composition = getComposition(measuresToUse)
        composition.foreach(println(_))
    }
}

case class BeatLine(note: String, startBeat: Double, duration: Double) {
    def toLine: String = {
        val nf: NumberFormat = new DecimalFormat("##.##")
        s"$note ${nf.format(startBeat)} ${nf.format(duration)}"
    }
}