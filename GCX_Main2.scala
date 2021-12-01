object main {
  class Game(val row: Int = 3, val col: Int = 3, val connected_points_to_win: Int = 3, val search_depth: Int = 3) {
    val pos_number_min = 0
    val pos_number_max = row * col - 1
    private var state = Array.ofDim[Int](row, col)

    def get_state(): Array[Array[Int]] = {
      state
    }

    def set_state(new_state: Array[Array[Int]]): Unit = {
      state = new_state
    }

    //turn=2 => AI,turn=1 => Human
    def set_state(row: Int, col: Int, turn: Int): Unit = {
      state(row)(col) = turn
    }
  }

  def begin(game: Game): Unit = {
    print_state(game)
    while (!is_end(game)) {
      println("it's your turn! please choose a number as a position:")
      player_turn(game)
      print_state(game)
      if (!is_end(game)) {
        ai_turn(game)
      }
      print_state(game)
    }
  }

  def print_state(game: Game): Unit = {
    for (i <- 0 until game.row) {
      for (j <- 0 until game.col) {
        print(state_to_char(game.get_state()(i)(j)))
      }
      println()
    }
    println("=========================")
  }

  def is_pos_empty(game: Game, pos_row: Int, pos_col: Int): Boolean = {
    game.get_state()(pos_row)(pos_col) == 0
  }

  def is_pos_legal(game: Game, pos_row: Int, pos_col: Int): Boolean = {
    pos_row >= 0 && pos_row < game.row && pos_col >= 0 && pos_col < game.col
  }

  def connect_check(game: Game, flag: Int, pos_row: Int, pos_col: Int, dire: (Int, Int), points: Int): Boolean = {
    (dire._1 != 0 || dire._2 != 0) &&
      ((is_pos_legal(game, pos_row, pos_col) && game.get_state()(pos_row)(pos_col) == flag) &&
        (points == 1 || connect_check(game, flag, pos_row + dire._1, pos_col + dire._2, dire, points - 1)))
  }

  def is_end(game: Game): Boolean = {
    var is_end = false
    for (i <- 0 until game.row) {
      for (j <- 0 until game.col) {
        for (row_dire <- -1 to 1) {
          for (col_dire <- -1 to 1) {
            is_end = is_end ||
              (game.get_state()(i)(j) != 0 &&
                connect_check(game, game.get_state()(i)(j), i, j, (row_dire, col_dire), game.connected_points_to_win))
          }
        }
      }
    }
    is_end
  }

  def player_turn(game: Game): Unit = {
    var pos = scala.io.StdIn.readInt()
    var (row_pos: Int, col_pos: Int) = (pos / game.row, pos % game.col)
    while (!(is_pos_legal(game, row_pos, col_pos) && is_pos_empty(game, row_pos, col_pos))) {
      println("please choose a legal position!")
      pos = scala.io.StdIn.readInt()
      row_pos = pos / game.row
      col_pos = pos % game.col
    }
    game.set_state(row_pos, col_pos, turn = 1)
  }

  def random_pos(virtual_game: Game): (Int, Int) = {
    for (i <- 0 until virtual_game.row) {
      for (j <- 0 until virtual_game.col) {
        if (virtual_game.get_state()(i)(j) == 0) {
          return (i, j)
        }
      }
    }
    (-1, -1)
  }

  def min_pos_find(pos_res: ((Int, Int), Double), row: Int, col: Int, pos_value: Double): ((Int, Int), Double) = {
    if (pos_res._2 > pos_value) ((row, col), pos_value) else pos_res
  }

  def max_pos_find(virtual_game: Game, turn: Int, depth: Int): ((Int, Int), Double) = {
    if (is_end(virtual_game)) {
      return ((-1, -1), 0)
    }
    if (depth > virtual_game.search_depth) {
      return (random_pos(virtual_game), 0.5)
    }
    var pos_res: ((Int, Int), Double) = ((-1, -1), 100.0)
    for (i <- 0 until virtual_game.row) {
      for (j <- 0 until virtual_game.col) {
        if (is_pos_empty(virtual_game, i, j)) {
          virtual_game.set_state(i, j, turn)
          pos_res = min_pos_find(pos_res, i, j, max_pos_find(virtual_game, 3 - turn, depth + 1)._2)
          virtual_game.set_state(i, j, 0)
        }
      }
    }
    (pos_res._1, 1 - pos_res._2)
  }

  def ai_turn(game: Game): Unit = {
    println("ai's turn !")
    var next_pos: ((Int, Int), Double) = max_pos_find(game, turn = 2, depth = 0)
    game.set_state(next_pos._1._1, next_pos._1._2, 2)
  }

  def state_to_char(num: Int): String = {
    num match {
      case 0 => " . "
      case 1 => " x "
      case 2 => " o "
    }
  }

  def main(args: Array[String]): Unit = {
    var game: Game = new Game(row = 3, col = 3, connected_points_to_win = 3, search_depth = 3)
    begin(game)
  }
}