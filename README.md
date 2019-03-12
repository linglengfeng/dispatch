"# dispatch" 

my_client可以用:
  MyClient.send(Test, :test0, [1,2,3]) 进行尝试
  MyClient.tcp_connect/0, tcp_connect/2,连接
  MyClient.close(delay) 断开

