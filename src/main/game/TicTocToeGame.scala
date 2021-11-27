import scala.collection.mutable.ListBuffer
object TicTocToeGame {
  //global variables
  var players: Array[String] = Array(" o " ," x ")
  var current = 0
  val AI = 1
  var winner: Int = -1
  def can_place_label = " . "
  def row: Array[String] = Array(can_place_label, can_place_label, can_place_label)
  var board: Array[Array[String]] = Array(row, row, row)
  var pos_list: ListBuffer[(Int, Int)] = ListBuffer[(Int, Int)]()

  def main(args: Array[String]): Unit ={
    for(i<-0 to 2; j<-0 to 2) pos_list.append((i, j))
    while(winner == -1){
      print_board(board)
      if (current == AI) ai_player() else human_player()
      check_for_win()
      switch_player()
    }
  }

  //this function prints the board after every move
  def print_board(board: Array[Array[String]]): Unit ={
    println("-----------------------")
    for (i <- 0 to 2; j <- 0 to 2){
      print(board(i)(j))
      if (j == 2) println()
    }
  }
  //this function checks if any player has won
  def check_for_win(): Unit ={
    //横竖向遍历
    for(i<- 0 to 2) if(the_same(Array((i, 0), (i, 1), (i, 2)))) game_end()
    //↖↘遍历↙↗遍历
    if(the_same(Array((0,0), (1,1), (2,2)))||the_same(Array((2,0), (1,1), (0,2)))) game_end()
    if (is_full(board) && winner == -1) game_end()
  }
  //this function switches the players' players
  def switch_player():Unit = {current = (current + 1) % 2}

  //  def best_pos_for(player:Int, index: Int, max_value: Int){
//    if(index == pos_list.length) return null
//  }
  def ai_player(): Unit={
    //best choice
    val randomGen = new util.Random(System.currentTimeMillis)
    val index = randomGen.nextInt(pos_list.length)
    val pos = pos_list(index)
    board(pos._1)(pos._2) = players(current)
    pos_list.remove(index)
  }
  def human_player(): Unit={
    val pos_input = scala.io.StdIn.readLine(s"Please place your %s? ".format(players(current)))
    val pos = pos_input.split(",").map(_.toInt)
    if (pos(0) != -1 && board(pos(0))(pos(1)) != can_place_label){
      println("Illegal move! Try again!")
      human_player()
    }else{
      board(pos(0))(pos(1)) = players(current)
      for(index<-0 to pos_list.length) {
        if (pos_list(index) == (pos(0), pos(1))) {
          pos_list.remove(index)
          return
        }
      }
    }
  }

  def the_same(positions: Array[(Int, Int)]): Boolean ={
    var count_row = 0
    var count_col = 0
    for (pos <- 0 to positions.length-1) {
      var (i, j) = positions(pos)
      if(board(i)(j) == players(current)) count_row += 1
      if(board(j)(i) == players(current)) count_col += 1
      if(count_col == 3 || count_row ==3){
        winner = current
        return true
      }
    }
    false
  }

  def is_full(board: Array[Array[String]]): Boolean={
    board.foreach(line=>line.foreach(c=>if(c==can_place_label) return false))
    true
  }

  def game_end(): Unit ={
    print_board(board)
    println("GAME OVER")
    if(winner == -1) println("It is a tie!")
    else println("%s is the winner!".format(players(winner)))
  }
}