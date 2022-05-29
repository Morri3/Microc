#### **1、解释器部分（12条命令）**

```sh
dotnet restore interpc.fsproj #可选
dotnet clean interpc.fsproj #可选
dotnet build interpc.fsproj #构建./bin/Debug/net6.0/interpc.exe，并查看详细生成过程
./bin/Debug/net6.0/interpc.exe zyq_example/preinc.c #查看运行结果
dotnet "C:\Users\82444\.nuget\packages\fslexyacc\10.2.0\build\/fslex/netcoreapp3.1\fslex.dll" -o "CLex.fs" --module CLex --unicode CLex.fsl #生成扫描器
dotnet "C:\Users\82444\.nuget\packages\fslexyacc\10.2.0\build\/fsyacc/netcoreapp3.1\fsyacc.dll" -o "CPar.fs" --module CPar CPar.fsy #生成分析器
dotnet fsi #进入命令行
					 #注：以下代码在终端的fsi中运行
#r "nuget: FsLexYacc";; //添加包引用
#load "Absyn.fs" "Debug.fs" "CPar.fs" "CLex.fs" "Parse.fs" "Interp.fs" "ParseAndRun.fs" ;;
open ParseAndRun;;
fromFile "zyq_example/preinc.c";; #查看preinc.c语法树
run (fromFile "zyq_example/preinc.c") [];; #解释执行preinc.c
```

#### **2、编译器部分（12条命令）**

```sh
gcc -o machine.exe machine.c #生成c虚拟机
dotnet restore microc.fsproj #可选
dotnet clean microc.fsproj #可选
dotnet build microc.fsproj #构建./bin/Debug/net6.0/microc.exe
dotnet run --project microc.fsproj zyq_example/preinc.c #查看运行结果
.\machine.exe -trace zyq_example/preinc.out 0 #追踪查看运行栈
dotnet fsi #启动fsi
				#注：以下代码在终端的fsi中运行
#r "nuget: FsLexYacc";;
#load "Absyn.fs" "CPar.fs" "CLex.fs" "Debug.fs" "Parse.fs" "Machine.fs" "Backend.fs" "Comp.fs" "ParseAndComp.fs";;
open ParseAndComp;;
Debug.debug <- true;; #打开调试
compileToFile (fromFile "zyq_example/preinc.c") "preinc";; #观察变量在环境上的分配
```

