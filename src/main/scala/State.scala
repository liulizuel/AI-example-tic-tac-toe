import java.util.Dictionary

class State() {

  val BOARD_ROWS = 4
  val BOARD_COLS = 4
  private var data = Array.ofDim[Int](BOARD_ROWS, BOARD_COLS)
  private var winner : Int = 0
  private var hash_val : Int = 0
  private var end : Boolean = false
  //private var all_states:Map[Int, (State.type, Boolean)] = Map()

  def getWinner(): Int = this.winner
  def getEnd(): Boolean = this.end
  def getData(): Array[Array[Int]] = this.data


  def hash(): Int ={
      if (this.hash_val == None){
        this.hash_val = 0
      }
      val it = Iterator(this.data)
      var a = 0
      while(it.hasNext){
        this.hash_val = this.hash_val*3+a+1
        a += 1
      }
      this.hash_val
  }

  def sum(array: Array[Int]): Int = {
    var Sum: Int = 0
    for(a <- array){
      Sum += a
    }
    Sum
  }

  def is_end(): Boolean ={
    if (this.end != None){
      return this.end
    }
    //横向检测，纵向检测，两个对角线检测，共8个数据
    //有总和为3或者为-3的就是说明有赢家
    var results = new Array[Int](this.BOARD_ROWS + this.BOARD_COLS + 2)
    var index = 0
    //计算每一行的数值
    for(a <- 0 until this.BOARD_ROWS){
      //将二维数组某一行传到函数sum里面去
      var z = new Array[Int](this.BOARD_ROWS)
      for(b <- 0 until this.BOARD_ROWS){
        z(b) = this.data(a)(b)
      }
      results(index) = sum(z)
      index += 1
    }
    //计算每一列的数值
    for(a <- 0 until this.BOARD_COLS){
      //将二维数组某一列传到函数sum里面去
      var z = new Array[Int](this.BOARD_COLS)
      for(b <- 0 until this.BOARD_COLS){
        z(b) = this.data(b)(a)
      }
      results(index) = sum(z)
      index += 1
    }
    //计算主对角线上的数值和
    for(a <- 0 until this.BOARD_ROWS){
      results(index)+=this.data(a)(a)
    }
    index += 1
    //计算副对角线上的数值和
    for(a <- 0 until this.BOARD_ROWS){
      results(index)+=this.data(a)(this.BOARD_ROWS-1-a)
    }

    //遍历result列表
    for(a <- results){
      //若有3，则表示人类获胜
      if(a == 3){
        this.winner = 1
        this.end = true
        return this.end
      }
      //若有-3，则表示机器获胜
      if(a == -3){
        this.winner = -1
        this.end = true
        return this.end
      }
    }
    //遍历整个二维数组，计算1和-1的个数，若总和与棋盘格子数量相同，则为平局
    for(a <- 0 until this.BOARD_ROWS){
      for(b <- 0 until this.BOARD_COLS){
        if(this.data(a)(b)==1 || this.data(a)(b)==-1){
          tie += 1
        }
      }
    }
    if(tie == this.BOARD_COLS*this.BOARD_ROWS){
      this.winner = 0
      this.end = true
      return this.end
    }

    this.end = false
    return this.end
  }

  def print_state(): Unit ={
    var i, j = 0
    for(i <- 0 until this.BOARD_ROWS){
      println("-------------")
      var out = "| "
      var token = " "
      for(j <- 0 until this.BOARD_COLS){
        if(this.data(i)(j) == 1){
          token="*"
        }
        if(this.data(i)(j) == 0){
          token="0"
        }
        if(this.data(i)(j) == -1){
          token="x"
        }
        out += token + " | "
      }
      println(out)
    }
    println("-------------")
  }

  def next_state(i: Int, j: Int, symbol: Int): State.type  ={
    var new_state = new State()
    //复制二维数组
    for(i <- 0 until this.BOARD_ROWS){
      for(j <- 0 until this.BOARD_COLS){
          new_state(i)(j) = this.data(i)(j)
      }
    }
    new_state(i)(j) = symbol
    return new_state
  }

  def get_all_states_impl(current_state: State.type , current_symbol: Int, all_states: (Int, (State.type, Boolean))): Unit ={
    var isEnd: Boolean
    var newState = new State()
    for(i <- 0 until this.BOARD_ROWS){
      for(j <- 0 until this.BOARD_COLS){}
      if(current_state.data(i)(j) == 0){
        newState = current_state.next_state(i, j, current_symbol)
        newHash = newState.hash()
        if(all_states.keys().contains(newHash)){
          isEnd = newState.is_end()
          all_states += (newHash -> (newState, isEnd))
          if(!isEnd){
            get_all_states_impl(newState, -current_symbol, all_states)
          }
        }
      }


    }

  }

  def get_all_states(): Map[Int, (State.type , Boolean)] = {
    val current_symbol = 1
    val current_state =new State()

    var all_states : Map[Int, (State.type , Boolean)] = Map()
    all_states += (current_state.hash() -> (current_state, current_state.is_end()))
    get_all_states_impl(current_state, current_symbol, all_states)
    all_states
  }

}
