# AI-example-tic-tac-toe
AI-example-tic-tac-toe functional programming
## 小组成员
组员：谷晨曦，卢柄屹，冯琦
组长：刘丽
## 分工
>● 【卢柄屹】state状态类：每个状态用自定义hash值描述，主要方法为get_all_states和next_state
>● 【冯琦】AI自测+人机对战+裁判：监督选手轮流下棋。主要方法为alternate（轮流选手），play（监督游戏执行，play里重要的为选手的act方法）
>● 【刘丽】AI选手：estimations表示不同状态下的分值，用以进行下一状态的选择，greedy区分随机行为，即随机行为不参与更新状态的分值主要方法为set_symbol（设置对于每个选手各状态分值的初始值），backup（更新状态分值，如果下一状态分值更高，那么当前状态的分值也要提高，即将长远的结果反作用到现在），act（获取下一步坐标）
>● 【谷晨曦】训练+人类玩家；
