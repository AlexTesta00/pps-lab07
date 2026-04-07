package ex3

object Solitaire extends App:

  private type Pos = (Int, Int)

  private def render(solution: Seq[Pos], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for
        y <- 0 until height
        row =
          for
            x <- 0 until width
            number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private val deltas: Seq[Pos] =
    Seq(
      (-3, 0), (3, 0), (0, -3), (0, 3),
      (-2, -2), (2, 2), (-2, 2), (2, -2)
    )

  private def inside(p: Pos, width: Int, height: Int): Boolean =
    p._1 >= 0 && p._1 < width && p._2 >= 0 && p._2 < height

  private def nextPositions(from: Pos, width: Int, height: Int): Seq[Pos] =
    deltas
      .map((dx, dy) => (from._1 + dx, from._2 + dy))
      .filter(p => inside(p, width, height))

  private def placeMarks(width: Int, height: Int): LazyList[List[Pos]] =
    val start = (width / 2, height / 2)
    val targetSize = width * height

    def place(solution: List[Pos]): LazyList[List[Pos]] =
      if solution.size == targetSize then
        LazyList(solution)
      else
        val current = solution.head
        val occupied = solution.toSet

        val possibleMoves =
          nextPositions(current, width, height)
            .filterNot(occupied.contains)

        possibleMoves.to(LazyList).flatMap { next =>
          place(next :: solution)
        }

    place(List(start))

  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))
  println()

  private val solutions5x5 = placeMarks(5, 5)
  println(s"Number of solution founded for 5x5: ${solutions5x5.size}")

  solutions5x5.headOption.foreach { sol =>
    println()
    println(render(sol, 5, 5))
  }