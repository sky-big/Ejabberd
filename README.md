# EjabberdEjabberd-16.0.3中文完全注释一.在bin目录下，需要将erl.ini改为对应你存储的目录(同时bin目录的名字不能变化，不然会报错，error_logger貌似写死了bin的名字，呵呵)    [erlang]    Bindir=C:/github/Ejabberd/bin    Progname=erl    Rootdir=C:/github/Ejabberd二.目录介绍    1.bin目录下能够对Ejabberd开源代码进行编译，运行。make.cmd只编译文件有变化的源代码文件，make_all.cmd将Ejabberd的源代码全部重新编译，run.cmd运行单个Ejabberd节点。    2.src目录下是Ejabberd 16.0.3的全部源代码    3.option目录是配置文件，run.option是用来配置启动的Ejabberd节点的日志路径，节点名字等相关信息的文件