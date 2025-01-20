import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

object MinesweeperSolver {

  case class Cell(row: Int, col: Int)

  def main(args: Array[String]): Unit = {
    val inputFilePath = "C:\\Users\\pablo\\Documents\\Uni\\4rto\\ProgDecl\\ScalATPractica\\src\\main\\instancies\\buscamines350.txt"  // Ruta al fitxer d'entrada
    val outputFilePath = "output.txt" // Ruta al fitxer de sortida

    val lines = Source.fromFile(inputFilePath).getLines().toList
    val Array(n, m, nmines) = lines.head.split(" ").map(_.toInt)
    val grid = lines.tail.map(_.split(" ").toList)

    val solver = new ScalAT("minesweeper")

    val variables = Array.tabulate(n, m)((i, j) => solver.newVar())

    // Afegeix les restriccions per a cada cel·la del taulell
    // Afegeix les restriccions per a cada cel·la del taulell
    for (i <- 0 until n; j <- 0 until m) {
      grid(i)(j) match {
        case "-" => // Cel·la desconeguda, pot ser mina o no. No cal afegir res específic aquí.
        case "X" => // Cel·la sense mina
          solver.addClause(List(-variables(i)(j))) // La variable ha de ser falsa (no és mina)
        case num if num.forall(_.isDigit) => // Cel·la amb un número (pista)
          val adjacentCells = getAdjacentCells(i, j, n, m) // Obté les cel·les adjacents
          val adjacentVars = adjacentCells.map { case Cell(row, col) => variables(row)(col) }

          if (num.toInt == 0) {
            adjacentVars.foreach(v => solver.addClause(List(-v))) // Marca totes les cel·les adjacents com a segures
          }
          else if (adjacentVars.size == num.toInt) {
            adjacentVars.foreach(v => solver.addClause(List(v))) // Marca totes les cel·les adjacents com a mines
          }
          else {
            solver.addEK(adjacentVars, num.toInt) // Afegeix restricció que hi ha exactament 'num' mines adjacents
          }
          // Assegura que aquesta cel·la no pot ser mina (és segura)
          solver.addClause(List(-variables(i)(j)))
        case _ => throw new IllegalArgumentException("Entrada no vàlida")
      }
    }

    // Si es coneix el nombre total de mines
    if (nmines >= 0) {
      val allVariables = variables.flatten.toList
      solver.addEK(allVariables, nmines)
    }


    // Resol el problema
    val result = solver.solve()

    if (result.satisfiable) {
      val model = Array.tabulate(n, m)((i, j) => solver.getValue(variables(i)(j)))

      // Converteix el model en un format amigable
      val solution = model.map(row => row.map(cell => if (cell) "o" else "X").mkString(" ")).mkString("\n")

      // Escriu la solució al fitxer de sortida
      val writer = new BufferedWriter(new FileWriter(new File(outputFilePath)))
      writer.write(solution)
      writer.close()

      println("Solució trobada:")
      println(solution)
    } else {
      println("No hi ha solució possible.")
    }
  }

  // Retorna les coordenades de les cel·les adjacents
  def getAdjacentCells(row: Int, col: Int, n: Int, m: Int): List[Cell] = {
    (for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0) // Evita la mateixa cel·la
      newRow = row + i
      newCol = col + j
      if newRow >= 0 && newRow < n && newCol >= 0 && newCol < m
    } yield Cell(newRow, newCol)).toList
  }
}
