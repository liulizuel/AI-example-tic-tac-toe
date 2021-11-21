import java.io.{FileInputStream, _}

object Player{
  val BOARD_ROWS = 4
  val BOARD_COLS = 4
  var stepSize: Float = 0.01f //step_size: 更新估计的步长
  var epsilon: Float = 0.2f //epsilon: 贪婪策略的小概率

  private var states: Array[State.type ] = Array()
  private var greedy = Array[Boolean]()
  private var estimations: Map[Int, Float] = Map()//estimations是字典类型
  private var symbol = 0
  def reset(): Unit ={
    this.greedy = Array()
    this.states = Array()
  }

  def set_state(state: State.type ): Unit ={
    this.states :+ state
    this.greedy.appended(true)
  }


  //  # all possible board configurations
  var all_states = State.get_all_states()

  def set_symbol(symbol: Int): Unit ={
    this.symbol = symbol
    //  # 对状态分值初始化，最终赢了得1分，输了不得分，平局0.5分，
    //  # 未到终局设置为0.5分
    val it = all_states.keysIterator
    while (it.hasNext){
      var hash_val = it.next()
      val t: (State.type , Boolean) = all_states.get(hash_val)[0]
      val state: State.type = t[0]
      val is_end: Boolean = t[1]
      if(is_end){
        if (state.hash() == this.symbol){
          this.estimations ++ (hash_val -> 1.0)
        }else if (state.getWinner() == 0){
          //验证是平局还是输了
          this.estimations ++ (hash_val -> 0)
        }else {
          //平局
          this.estimations ++ (hash_val -> 0.5)
        }
      }
    }
  }


  //  update value estimation
  //  将在贪心动作之后得到的状态对应的价值”回溯更新“到动作之前的状态上。（对早先的状态的价值进行调整，使其更接近于后面的状态所对应的价值）
  //  更新状态分值，如果下一状态分值更高，那么当前状态分值也要提高，即将长远的结果反作用到现在
  def backup(): Unit ={
    val states:Array[Int] = Array()
    this.states.foreach(state => states :+ state.hash())
    // 更新 estimation
    // 顺序更新
    // 反转的迭代器
    var a = 0
    states.foreach(state =>{
      if(this.greedy(a)) {
        this.estimations(state) += this.stepSize * (this.estimations(states(a + 1)) - this.estimations(state))
      }
      a += 1
    })
  }

  //  获取下一步坐标
  def act(): Array[Int] ={
    val state = this.states.last
    //下一步可能的状态的hash
    var next_states = Array()
    var next_positions = Array[Array[Int]]()
    for(i <- 1 to BOARD_ROWS; j <- 1 to BOARD_COLS){
        if(state.getData()(i)(j) == 0) {
          next_positions :+ (i, j)
          next_states :+ state.next_state(i, j, symbol).hash()
        }
    }
    var action = Array[Int]()
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
    val values = Array[(Float, Int)]()
    val it1 = next_states.iterator
    val it2 = next_positions.iterator
    while(it1.hasNext && it2.hasNext){
      values :+ (estimations.get(it1.next()), it2.next())
    }
    values.sortWith((_, _) => scala.util.Random.nextBoolean())
    values.toList.sortBy(_._1)
    action = values[0][1]
    action :+ symbol
    action
  }
  var filename =  (i: Int) => if(i == 1){ "first" }else{ "second"}
  def save_policy(): Unit ={
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
