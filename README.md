"# dispatch" 

my_client可以用:
  MyClient.send(Test, :test0, [1,2,3]) 进行尝试
  MyClient.tcp_connect/0, tcp_connect/2,连接
  MyClient.close(delay) 断开

  Msg.send(:login, :player, [123])
  Msg.send(:chat, :channel, [1, "hwmw1"])
  Msg.send(:chat, :channel, [2, "hwmw2"])
  Msg.send(:chat, :channel, [11, "hwmw11"])
  Msg.send(:chat, :people, [124, "hwmw124"])
  Msg.send(:chat, :people, [125, "hwmw125"])
  Msg.send(:chat, :people, [1244, "hwmw1244"])