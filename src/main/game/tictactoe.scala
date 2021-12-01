object tictactoe {
  var board = Array.ofDim[Int](3, 3)
  val man = 1
  val com = -1
  val n = 3
  val step = 9
  val searchdepth = 9
  var current_player = 0
  var curdepth = 0
  var isEnd = false

  class pos(xx:Int,yy:Int){
    var x=xx
    var y=yy
  }

  var BestPos = new pos(0,0)

  def Init(): Unit = {
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        board(i)(j) = 0
      }
    }
    current_player = man
    isEnd = false
    curdepth = 0
  }

  def drawboard(): Unit = {
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        print(board(i)(j))
      }
      println()
    }
  }

  def evaluate(): Int = {
    val value = is_end()
    if (value == man) return 100
    if (value == com) return -100
    return value
  }

  def is_end(): Int = {
    for (i <- 0 until n) {
      var count = 0
      for (j <- 0 until n) {
        count += board(i)(j)
      }
      if (count == 3 || count == -3)
        return (count / 3)
    }
    for (i <- 0 until n) {
      var count = 0
      for (j <- 0 until n) {
        count += board(j)(i)
      }
      if (count == 3 || count == -3)
        return (count / 3)
    }
    var count = 0
    count = board(0)(0) + board(1)(1) + board(2)(2)
    if (count == 3 || count == -3)
      return count / 3
    count = board(0)(2) + board(1)(1) + board(2)(0)
    if (count == 3 || count == -3)
      return count / 3
    return 0
  }

  def search(depth: Int): Int = {
    var value = 0
    if (current_player == man) value = -100
    if (current_player == com) value = 100
    if(is_end()!=0){
      return evaluate()
    }
    if(depth==searchdepth){
      value=evaluate()
      return value
    }
    for(i<- 0 until n){
      for(j<- 0 until n){
        if(board(i)(j)==0){
          if(current_player==man){
            board(i)(j)=man
            current_player=com
            var nextvalue = search(depth+1)
            current_player=man
            if(value<nextvalue){
              value=nextvalue
              if(depth==curdepth){
                BestPos.x=i
                BestPos.y=j
              }
            }
          }
          else{
            board(i)(j)=com
            current_player=man
            var nextvalue = search(depth+1)
            current_player=com
            if(value>nextvalue){
              value=nextvalue
              if(depth==curdepth){
                BestPos.x=i
                BestPos.y=j
              }
            }
          }
          board(i)(j)=0
        }
      }
    }
    return value
  }

  def complay(): Unit ={
    search(curdepth)
    board(BestPos.x)(BestPos.y)=com
    curdepth+=1
    current_player=man
  }

  def manplay(x:Int,y:Int): Unit ={
    board(x)(y)=man
    curdepth+=1
      current_player=com
  }

  def input(): Unit ={
    var pos=scala.io.StdIn.readInt()
    if(!isEnd){
      if(pos>=1&&pos<=9){
        val posnum=pos-1
        val posx=posnum/3
        val posy=posnum%3
        manplay(posx,posy)
        if(is_end()==0&&curdepth<8){
          complay()
          if(is_end()!=0){
            isEnd=true
          }
        }
        else {
          isEnd=true
        }
      }
      if(isEnd){
        if(pos==0){
          Init()
        }
      }
    }
  }

  def logic(): Unit ={
    if(isEnd){
      if(is_end()==man){
        println("human win!")
      }
      else if(is_end()==com){
        println("AI win!")
      }
      else {
        println("It is a tie!")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("按1-9出棋")
    Init()
    while(true){
      input()
      logic()
      drawboard()

    }
  }
}
