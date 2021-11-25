import java.io.{FileInputStream, _}
import scala.collection.mutable.Map


object TicTocToe{


  val BOARD_ROWS = 3
  val BOARD_COLS = 3
  var all_states: Map[Int, (State, Boolean)] = get_all_states()

  class State() {
    private var data = Array.ofDim[Int](BOARD_ROWS, BOARD_COLS)
    private var winner: Int = 0
    private var hash_val: Int = -1
    private var end: Boolean = false
    //private var all_states:Map[Int, (State, Boolean)] = Map()

    def getWinner(): Int = this.winner

    def getEnd(): Boolean = this.end

    def getData(): Array[Array[Int]] = this.data


    def hash(): Int = {
      if (this.hash_val == -1) {
        this.hash_val = 0
      }
      var i, j = 0
      for(i <- 0 until BOARD_ROWS){
        for(j <- 0 until BOARD_COLS){
          if(this.getData()(i)(j) == -1){
            this.hash_val += this.hash_val * 3 + 2
          }
          else{
            this.hash_val += this.hash_val * 3 + this.getData()(i)(j)
          }
        }
      }
      this.hash_val
    }

    def sum(array: Array[Int]): Int = {
      var Sum: Int = 0
      for (a <- array) {
        Sum += a
      }
      Sum
    }

    def is_end(): Boolean = {
      if (this.end) {
        return this.end
      }
      //横向检测，纵向检测，两个对角线检测，共8个数据
      //有总和为3或者为-3的就是说明有赢家
      var results = new Array[Int](BOARD_ROWS + BOARD_COLS + 2)
      var index = 0
      //计算每一行的数值
      for (a <- 0 until BOARD_ROWS) {
        //将二维数组某一行传到函数sum里面去
        var z = new Array[Int](BOARD_ROWS)
        for (b <- 0 until BOARD_ROWS) {
          z(b) = this.data(a)(b)
        }
        results(index) = sum(z)
        index += 1
      }
      //计算每一列的数值
      for (a <- 0 until BOARD_COLS) {
        //将二维数组某一列传到函数sum里面去
        var z = new Array[Int](BOARD_COLS)
        for (b <- 0 until BOARD_COLS) {
          z(b) = this.data(b)(a)
        }
        results(index) = sum(z)
        index += 1
      }
      //计算主对角线上的数值和
      for (a <- 0 until BOARD_ROWS) {
        results(index) += this.data(a)(a)
      }
      index += 1
      //计算副对角线上的数值和
      for (a <- 0 until BOARD_ROWS) {
        results(index) += this.data(a)(BOARD_ROWS - 1 - a)
      }

      //遍历result列表
      for (a <- results) {
        //若有3，则表示人类获胜
        if (a == 3) {
          this.winner = 1
          this.end = true
          return this.end
        }
        //若有-3，则表示机器获胜
        if (a == -3) {
          this.winner = -1
          this.end = true
          return this.end
        }
      }
      var tie = 0
      //遍历整个二维数组，计算1和-1的个数，若总和与棋盘格子数量相同，则为平局
      for (a <- 0 until BOARD_ROWS) {
        for (b <- 0 until BOARD_COLS) {
          if (this.data(a)(b) == 1 || this.data(a)(b) == -1) {
            tie += 1
          }
        }
      }
      if (tie == BOARD_COLS * BOARD_ROWS) {
        this.winner = 0
        this.end = true
        return this.end
      }

      this.end = false
      return this.end
    }

    def print_state(): Unit = {
      var i, j = 0
      for (i <- 0 until BOARD_ROWS) {
        println("-------------")
        var out = "| "
        var token = " "
        for (j <- 0 until BOARD_COLS) {
          if (this.data(i)(j) == 1) {
            token = "*"
          }
          if (this.data(i)(j) == 0) {
            token = "0"
          }
          if (this.data(i)(j) == -1) {
            token = "x"
          }
          out += token + " | "
        }
        println(out)
      }
      println("-------------")
    }

    def next_state(i: Int, j: Int, symbol: Int): State = {
      var new_state = new State()
      //复制二维数组

      for (a <- 0 until BOARD_ROWS) {
        for (b <- 0 until BOARD_COLS) {
          new_state.data(a)(b) = this.data(a)(b)
        }
      }
      new_state.data(i)(j) = symbol
      new_state
    }



  }
  def get_all_states_impl(current_state: State, current_symbol: Int, all_states: Map[Int, (State, Boolean)]): Unit = {
    var isEnd: Boolean = false
    var newState = new State()
    var newHash = 0
    for (i <- 0 until BOARD_ROWS) {
      for (j <- 0 until BOARD_COLS) {
        if (current_state.getData()(i)(j) == 0) {
          newState = current_state.next_state(i, j, current_symbol)
          newHash = newState.hash()
          if (all_states.keySet.contains(newHash)) {
            isEnd = newState.is_end()
            all_states += (newHash -> (newState, isEnd))
            if (!isEnd) {
              get_all_states_impl(newState, -current_symbol, all_states)
            }
          }
        }
      }
    }
  }
  def get_all_states(): Map[Int, (State, Boolean)] = {
    val current_symbol = 1
    val current_state = new State()

    var all_states: Map[Int, (State, Boolean)] = Map()
    all_states += (current_state.hash() -> (current_state, current_state.is_end()))
    get_all_states_impl(current_state, current_symbol, all_states)
    all_states
  }
  //人类玩家
  class HumanPlayer {
    def get_symbol() = symbol

    private var symbol = -1
    private var state = new State()
    private val keys = List('q', 'w', 'e', 'a', 's', 'd', 'z', 'x', 'c')

    def reset(): Unit = {
      symbol = 1
      state = new State()
    }

    def set_state(state: State): Unit = {
      this.state = state
    }

    def set_symbol(symbol: Int): Unit = {
      this.symbol = symbol
    }

    def backup(): Unit = {

    }

    def act(): (Int, Int, Int) = {
      this.state.print_state()
      println("please choose your position")
      var position = scala.io.StdIn.readChar()
      while (!keys.contains(position)) {
        println("please rechoose your position")
        position = scala.io.StdIn.readChar()
      }
      val index = keys.indexOf(position)
      val i = index / BOARD_COLS
      val j = index % BOARD_COLS
      (i, j, this.symbol)
    }

  }

  //训练
  class Train {
    def train(epochs: Int = 1, epsilon: Float = 0.1f): Unit = {
      val player_1 = new Player(epsilon)
      val player_2 = new Player(epsilon)
      val judger = new Judger(player_1, player_2, null)
      var player_1_win = 0.0
      var player_2_win = 0.0
      for (i <- 1 to epochs) {
        val winner = judger.play(print_state = false)
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


  class Player(epsilon: Float = 0.2f) {

    var stepSize: Float = 0.01f //step_size: 更新估计的步长
    private var states: Array[State] = Array()
    private var greedy = Array[Boolean]()
    private var estimations: Map[Int, Float] = Map() //estimations是字典类型
    private var symbol: Int = 0

    def reset(): Unit = {
      this.greedy = Array()
      this.states = Array()
    }

    def set_state(state: State): Unit = {
      this.states :+ state
      this.greedy.appended(true)
    }

    def get_symbol() = symbol


    //  # all possible board configurations



    def set_symbol(symbol: Int): Unit = {
      this.symbol = symbol
      //  # 对状态分值初始化，最终赢了得1分，输了不得分，平局0.5分，
      //  # 未到终局设置为0.5分

      val it = all_states.keysIterator
      while (it.hasNext) {
        var hash_val = it.next()
        val is_end = all_states(hash_val)._2
        val state = all_states(hash_val)._1
        if (is_end) {
          if (state.hash() == this.symbol) {
            this.estimations += (hash_val -> 1.0f)
          } else if (state.getWinner() == 0) {
            //验证是平局还是输了
            this.estimations += (hash_val -> 0f)
          } else {
            //平局
            this.estimations += (hash_val -> 0.5f)
          }
        }
      }
    }


    //  update value estimation
    //  将在贪心动作之后得到的状态对应的价值”回溯更新“到动作之前的状态上。（对早先的状态的价值进行调整，使其更接近于后面的状态所对应的价值）
    //  更新状态分值，如果下一状态分值更高，那么当前状态分值也要提高，即将长远的结果反作用到现在
    def backup(): Unit = {
      val states: Array[Int] = Array()
      this.states.foreach(state => states :+ state.hash())
      // 更新 estimation
      // 顺序更新
      // 反转的迭代器
      var a = 0
      states.foreach(state => {
        if (this.greedy(a)) {
          this.estimations(state) += this.stepSize * (this.estimations(states(a + 1)) - this.estimations(state))
        }
        a += 1
      })
    }

    //  获取下一步坐标
    def act(): Array[Int] = {
      val state = this.states.last
      //下一步可能的状态的hash
      var next_states = Array()
      var next_positions = Array.ofDim[Int](BOARD_COLS, BOARD_ROWS)
      for (i <- 1 to BOARD_ROWS; j <- 1 to BOARD_COLS) {
        if (state.getData()(i)(j) == 0) {
          next_positions :+ (i, j)
          next_states :+ state.next_state(i, j, symbol).hash()
        }
      }
      var action: Array[Int] = Array()
      //小概率随机探索
      if (scala.util.Random.nextFloat() < epsilon) {
        val step = scala.util.Random.nextInt(next_positions.size)
        action :+ next_positions(step)
        action :+ symbol
        //表示随机动作不参与价值更新
        greedy(greedy.size - 1) = false
        return action
      }
      //大概率按照奖励最高行动
      val values = Map[Float, Array[Int]]()
      val it1 = next_states.iterator
      val it2 = next_positions.iterator
      while (it1.hasNext && it2.hasNext) {
        values += (estimations(it1.next()) -> it2.next())
      }
      values.toList.sortWith((_, _) => scala.util.Random.nextBoolean())
      values.toList.sortBy(_._1)
      action = values(0)
      action :+ symbol
      action
    }

    var filename = (i: Int) => if (i == 1) {
      "first"
    } else {
      "second"
    }

    def save_policy(): Unit = {
      //基于磁盘文件流的序列化
      val oos = new ObjectOutputStream(new FileOutputStream("policy_%s.bin".format(filename(this.symbol))))
      oos.writeObject(this.estimations)
      oos.close()
    }

    def load_policy(): Unit = {
      val ois = new ObjectInputStream(new FileInputStream("policy_%s.bin".format(filename(this.symbol))))
      this.estimations = ois.readObject.asInstanceOf[Map[Int, Float]]
    }
  }



  //裁判
  class Judger(player1: Player, player2: Player, human: HumanPlayer) {
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
          current_state.print_state()
        p = curren_player.act()
        i = p(0)
        j = p(1)
        symbol = p(2)
        next_state_hash = current_state.next_state(i, j, symbol).hash()
        is_end = all_states(next_state_hash)._2
        current_state = all_states(next_state_hash)._1
        p1.set_state(current_state)
        p2.set_state(current_state)
        if (is_end) {
          if (print_state)
            current_state.print_state()
          return current_state.getWinner()
        }


      }
      current_state.getWinner()

    }


  }


  def main(args: Array[String]): Unit = {
    println("游戏开始")
    compete(1)
  }
    // AI自测
  def compete(turns: Int): Unit = {
    val player1 = new Player(0)
    val player2 = new Player(0)
    val judger = new Judger(player1, player2, null)
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
      val judger = new Judger(null, player2, player1)
      player2.load_policy()
      val winner = judger.play()
      if (winner == player2.get_symbol())
        println("You lose!")
      if (winner == player1.get_symbol())
        println("You win!")
      else
        println("It is a tie!")

    }
  }

}
