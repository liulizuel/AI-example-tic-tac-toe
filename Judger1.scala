package com.xxx

//裁判
class Judger(player1: Player, player2: Player) {
  val p1: Player = player1
  val p2: Player = player2
  var curren_player: Player = p1
  val p1_symbol = 1
  val p2_symbol = -1
  p1.set_symbol(p1_symbol)
  p2.set_symbol(p2_symbol)
  var current_state = new State()
  var round = 0


  def reset(): Unit = {
    p1.reset()
    p2.reset()
    round = 0
  }

  def alternate(): Unit = {
    if (round % 2 == 0)
      curren_player = p1
    else
      curren_player = p2
    round += 1
  }

  def play(print_state: Boolean = false): Int = {
    reset()
    current_state = new State()
    p1.set_state(current_state)
    p2.set_state(current_state)
    var i, j, symbol, next_state_hash = 0
    var p = new Array[Int](3)
    var is_end = true
    while (true) {
      alternate()
      if (print_state)
        current_state.print()
      p = curren_player.act()
      i = p(0)
      j = p(1)
      symbol = p(2)
      next_state_hash = current_state.next_state(i, j, symbol).hash()
      is_end = current_state.all_stats(next_state_hash)._2
      current_state = current_state.all_stats(next_state_hash)._1
      p1.set_state(current_state)
      p2.set_state(current_state)
      if (is_end) {
        if (print_state)
          current_state.print()
        return current_state.winner
      }


    }
    return current_state.winner

  }


}


//电脑
class Player(epsilonx: Float) {
  var symbol = 0
  val epsilon = epsilonx


  def set_symbol(x: Int): Unit = {
    symbol = x
  }

  def reset(): Unit = {

  }

  def set_state(x: State): Unit = {

  }


  def act(): Array[Int] = {
    Array[Int](3)
  }

  def load_policy(): Unit = {

  }

}


//人类
class HumanPlayer(a: Float = 0) extends Player(a) {

}

//状态

import scala.collection.mutable.Map

class State() {

  var winner = 1


  def print(): Unit = {

  }

  def hash(): Int = {
    1
  }

  def next_state(i: Int, j: Int, symbol: Int): State = {
    val new_state = new State()
    new_state
  }

  def is_end(): Boolean = {
    return true
  }

  def get_all_states() = {
    val all_stats: Map[Int, (State, Boolean)] = Map()
    all_stats
  }


  val all_stats = get_all_states()
}


object Tic {
  def main(args: Array[String]): Unit = {

    // AI自测
    def compete(turns: Int) {
      val player1 = new Player(0)
      val player2 = new Player(0)
      val judger = new Judger(player1, player2)
      player1.load_policy()
      player2.load_policy()
      var player1_win, player2_win = 0.0
      for (i <- 1 to turns) {
        val winner = judger.play()
        if (winner == 1)
          player1_win += 1
        if (winner == (-1))
          player2_win += 1
        judger.reset()
      }
      println(f"$turns turns,player1 win ${player1_win / turns}%.2f,player2 win ${player2_win / turns}%.2f")

    }

    //  人机对战
    def play(): Unit = {
      while (true) {
        val player1 = new HumanPlayer()
        val player2 = new Player(0)
        val judger = new Judger(player1, player2)
        player2.load_policy()
        val winner = judger.play()
        if (winner == player2.symbol)
          println("You lose!")
        if (winner == player1.symbol)
          println("You win!")
        else
          println("It is a tie!")

      }
    }
  }


}
