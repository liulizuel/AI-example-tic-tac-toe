//人类玩家
class HumanPlayer {
  private var symbol = new Symbol()
  private var state = new State()
  private val keys = List('q', 'w', 'e', 'a', 's', 'd', 'z', 'x', 'c')

  def reset(): Unit = {
    symbol = new Symbol()
    state = new State()
  }

  def set_state(state: State): Unit = {
    this.state = state
  }

  def set_symbol(symbol: Symbol): Unit = {
    this.symbol = symbol
  }

  def backup(): Unit = {

  }

  def act(): (Int,Int,Symbol) = {
    this.state.print()
    println("please choose your position")
    var position = scala.io.StdIn.readChar()
    while (!keys.contains(position)) {
      println("please rechoose your position")
      position = scala.io.StdIn.readChar()
    }
    val index = keys.indexOf(position)
    val i = index / BOARD_COLS
    val j = index % BOARD_COLS
    return (i,j,this.symbol)
  }

}

//训练
class Train {
  def train(epochs: Int = 1, epsilon: Float = 0.1): Unit = {
    val player_1 = Player(epsilon)
    val player_2 = Player(epsilon)
    val judger = Judger(player_1, player_2)
    var player_1_win = 0.0
    var player_2_win = 0.0
    for (i <- 1 to epochs) {
      val winner = judger.play(print = false)
      if (winner == 1) {
        player_1_win = player_1_win + 1.0
      }
      if (winner == -1) {
        player_2_win = player_2_win + 1.0
      }
      print("epoch:", i, "  player_1 win:", player_1_win / i, "  player_2 win:", player_2_win / i)
      player_1.backup()
      player_2.backup()
      judger.reset()
    }
    player_1.save_policy()
    player_2.save_policy()
  }
}

