(* File MicroC/Interp.c
   Interpreter for micro-C, a fraction of the C language
   sestoft@itu.dk * 2010-01-07, 2014-10-18

   值是一个整数；它可以表示整数或指针，其中指针只是存储区中的地址（变量或指针或数组的基址）。
   env将变量映射到地址（位置），store将位置映射到整数。这可以自由地使用指针算法，
   就像在真正的C语言中一样。表达式可能有副作用。函数接受一系列类型化参数，并可以选择返回结果。

   目前，数组只能是一维的。为简单起见，我们将数组表示为一个变量，该变量保存第一个数组元素的地址。
   这与C中处理数组类型参数的方式（以及B语言中处理数组类型变量的方式）一致，但与C中处理数组类型
   变量的方式不一致。

   store的行为就像一个堆栈，所以所有数据都是堆栈分配的：变量、函数参数和数组。

   return语句没有实现（为了简单起见），因此所有函数都应该具有返回类型void。但目前还没有
   类型检查，所以要小心。
*)

module Interp

open Absyn
open Debug

(* 简单的环境操作 *)
// 多态类型 env
// 环境 env 是 元组 ("name",data) 的列表 ，名称是字符串 string 值 'data 可以是任意类型
//  名称 ---> 数据 名称与数据绑定关系的 键-值 对 ，即key-value pairs
// [("x",9);("y",8)]: int env
type 'data env = (string * 'data) list //表明环境类型可以是任意类型，环境env是列表；值也可以是任意类型

//环境查找函数
//在 环境env 上查找名称为 x 的值
let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr -> if x = y then v else lookup yr x //x和找到的y名称相同，就返回y的值v，否则继续查找

(* 局部变量环境知道下一个未使用的存储位置⭐ *)

// ([("x",9);("y",8)],10)
// x 在位置9, y 在位置8, 10表示 下一个空闲空间位置在10
type locEnv = int env * int

(* 函数环境将函数名映射到参数列表和主体 *)
//函数参数例子:
//void func (int a , int *p)
// 参数声明列表为: [(TypI,"a");(TypP(TypI) ,"p")]
type paramdecs = (typ * string) list //参数声明列表

(* 函数环境列表
  [("函数名", ([参数元组(类型,"名称")的列表],函数体AST)), ...]

  //main (i){
  //  int r;
  //    fac (i, &r);
  //    print r;
  // }
  [ ("main",
   ([(TypI, "i")],
    Block
      [Dec (TypI,"r");
       Stmt (Expr (Call ("fac",[Access (AccVar "i"); Addr (AccVar "r")])));
       Stmt (Expr (Prim1 ("printi",Access (AccVar "r"))))]))]

  函数环境 是 多态类型 'data env ---(string * 'data ) list 的一个具体类型 ⭐⭐⭐
    类型变量 'data 具体化为 (paramdecs * stmt)
    即 (string * (paramdecs * stmt)) list
*)

type funEnv = (paramdecs * stmt) env //函数环境

(* 全局环境包括 全局变量环境 和 全局函数环境 

   全局环境是 变量声明环境 和 函数声明环境 两个列表的元组
   ([var declares...],[fun declares..])
   ( [ ("x" ,1); ("y",2) ], [("main",mainAST);("fac",facAST)] )
   其中，mainAST,facAST 分别是 main 与 fac 的抽象语法树  
*)

type gloEnv = int env * funEnv //全局环境

(* store把地址（整数）映射为值（整数）： *)

//地址是store上的的索引值
type address = int

// store存储 是一个 地址到值的映射，是对内存的抽象 ⭐⭐⭐
// store存储 是可更改的数据结构，特定位置的值可以修改，注意与环境的区别
// map{ (0,3);(1,8) }
// 位置 0 保存了值 3
// 位置 1 保存了值 8
// 通过Map实现store
type store = Map<address, int>

//空存储
let emptyStore = Map.empty<address, int>

//保存value到存储store
let setSto (store: store) addr value = store.Add(addr, value)

//根据输入的addr返回存储的值value
let getSto (store: store) addr = store.Item addr

// store上从loc开始分配n个值的空间
// 用于数组分配
let rec initSto loc n store =
    if n = 0 then
        store
    else // 默认值 0
        initSto (loc + 1) (n - 1) (setSto store loc 0) //递归调用

(* 结合store和environment的操作 *)

(* 扩展局部变量环境，使其将x映射到nextloc（下一个存储位置），并使store[nextloc]=v。

locEnv结构是元组 : (绑定环境env,下一个空闲地址nextloc)
store结构是Map<string,int>

扩展环境 (x nextloc) :: env ====> 新环境 (env1,nextloc+1)
变更store (nextloc) = v
 *)

// 绑定一个值 x,v 到环境
// 环境是非更改数据结构，只添加新的绑定（变量名称，存储位置），注意与 store 的区别⭐⭐⭐
// 返回新环境 locEnv,更新store,
// nextloc是store上下一个空闲位置
(*

// variable.c
int g ;
int h[3];
void main (int n){
n = 8;
}
上面c程序的解释环境如下：

 环境：locEnv:
    ([(n, 5); (n, 4); (g, 0)], 6)

存储：store:
    (0, 0)  (1, 0)(2, 0)(3, 0)(4, 1)  (5, 8)
     ^^^^    ^^^^^^^^^^^^^^^^^^^^^^    ^^^^
       g               h                n

   变量 地址 值
   n--->5--->8
   h--->4--->1
   g--->0--->0

   下一个待分配位置是 6
*)

//将多个值 xs vs 绑定到环境
//遍历 xs vs 列表,然后调用 bindVar 实现单个值的绑定
let store2str store =
    String.concat "" (List.map string (Map.toList store))

//绑定单个值到环境
let bindVar x v (env, nextloc) store : locEnv * store =
    let env1 = (x, nextloc) :: env //新的环境
    msg $"bindVar:\n%A{env1}\n"

    //返回新环境，新的待分配位置+1，设置当前存储位置为值 v
    let ret = ((env1, nextloc + 1), setSto store nextloc v)
    
    msg $"locEnv:\n {fst ret}\n"
    msg $"Store:\n {store2str (snd ret)}\n"

    ret 

//绑定多个值到环境
let rec bindVars xs vs locEnv store : locEnv * store =
    let res =
        match (xs, vs) with
        | ([], []) -> (locEnv, store)
        | (x1 :: xr, v1 :: vr) ->
            let (locEnv1, sto1) = bindVar x1 v1 locEnv store
            bindVars xr vr locEnv1 sto1
        | _ -> failwith "parameter/argument mismatch"

    msg "\nbindVars:\n"
    msg $"\nlocEnv:\n{locEnv}\n"
    msg $"\nStore:\n"
    store2str store |> msg
    res

(* 分配变量（int、指针或数组）：扩展环境，使其将变量映射到下一个可用的存储位置，并初始化存储位置 *)
//输入: (类型, 变量名)
//输出: 绑定了变量名的环境，和分配了空间的store
//  allocate : typ * string -> locEnv -> store -> locEnv * store
let rec allocate (typ, x) (env0, nextloc) sto0 : locEnv * store =

    let (nextloc1, v, sto1) =
        match typ with
        //数组 调用 initSto 分配 i 个空间
        | TypA (t, Some i) -> (nextloc + i, nextloc, initSto nextloc i sto0)
        // 常规变量默认值是 0
        | _ -> (nextloc, 0, sto0)

    msg $"\nalloc:\n {((typ, x), (env0, nextloc), sto0)}\n"
    bindVar x v (env0, nextloc1) sto1

(* 构建变量和函数的全局环境。对于全局变量，存储位置是保留的；对于全局函数，只需添加到全局函数环境。 *)
//初始化 解释器环境和store
let initEnvAndStore (topdecs: topdec list) : locEnv * funEnv * store =

    //包括全局函数和全局变量
    msg $"\ntopdecs:\n{topdecs}\n"

    //构造全局环境
    let rec addv decs locEnv funEnv store =
        match decs with
        | [] -> (locEnv, funEnv, store)

        // 全局变量声明：调用 allocate 在store上给变量分配空间
        | Vardec (typ, x) :: decr ->
            let (locEnv1, sto1) = allocate (typ, x) locEnv store //分配空间
            addv decr locEnv1 funEnv sto1

        //全局函数：将声明(f,(xs,body))添加到全局函数环境 funEnv
        | Fundec (_, f, xs, body) :: decr -> addv decr locEnv ((f, (xs, body)) :: funEnv) store

        // 全局变量初始化：调用 allocate 在store上给变量分配空间
        | VardecAndAssign (typ, x, expr) :: decr ->
            let (locEnv1, sto1) = allocate (typ, x) locEnv store //分配空间
            addv decr locEnv1 funEnv sto1

    // ([], 0) []  默认全局环境
    // locEnv ([],0) 变量环境 ，变量定义为空列表[],下一个空闲地址为0
    // ([("n", 1); ("r", 0)], 2)  表示定义了 变量 n , r. 下一个可以用的变量索引是 2
    // funEnv [] 函数环境，函数定义为空列表[]
    addv topdecs ([], 0) [] emptyStore

(* ------------------------------------------------------------------- *)
(* 解释micro-C语句 *)
//传入的参数是：语句，局部环境，全局环境，store
//返回的是store
//  exec : stmt -> locEnv -> gloEnv -> store -> store
let rec exec stmt (locEnv: locEnv) (gloEnv: gloEnv) (store: store) : store =
    match stmt with
    | If (e, stmt1, stmt2) ->   //if语句
        let (v, store1) = eval e locEnv gloEnv store

        if v <> 0 then
            exec stmt1 locEnv gloEnv store1 //True分支
        else
            exec stmt2 locEnv gloEnv store1 //False分支

    | While (e, body) ->           //while循环
        //定义while循环的辅助函数 loop
        let rec loop store1 =
            //计算表达式e的值，返回更新过的store2
            let (v, store2) = eval e locEnv gloEnv store1
            // 继续循环
            if v <> 0 then
                loop (exec body locEnv gloEnv store2)
            // 退出循环，返回环境store2
            else
                store2

        loop store

    | Expr e ->                       //表达式
        // _ 表示丢弃e的值,返回 变更后的环境store1
        let (_, store1) = eval e locEnv gloEnv store
        store1

    | Block stmts ->          //语句块
        // 语句块的解释辅助函数 loop
        let rec loop ss (locEnv, store) =
            match ss with
            | [] -> store    //空就返回store存储
            //语句块,解释 第1条语句s1
            // 调用loop 用变更后的环境 解释后面的语句 sr.
            | s1 :: sr -> loop sr (stmtordec s1 locEnv gloEnv store)

        loop stmts (locEnv, store)

    | Return _ -> failwith "return not implemented"      // 解释器暂未实现return

    | For (e1, e2, e3, body) -> //for循环
        let (v1, store1) = eval e1 locEnv gloEnv store //计算初始化表达式的值，得到更新过的store1
        //定义while循环的辅助函数 loop【这里store是更新过的store1】
        let rec loop store1 =
            let (v2, store2) = eval e2 locEnv gloEnv store1 //计算循环条件的值

            if v2 <> 0 then // 如果循环条件不为0，就先执行函数体body，然后计算e3
                let store3 = exec body locEnv gloEnv store2 //exec若是表达式的话，返回的就是一个更新过的store3
                let (v3, store4) = eval e3 locEnv gloEnv store3 //计算表达式e3的值，返回更新过的store4
                loop store4 //循环执行，传入的是store4
            else // 如果循环条件为0，退出循环，返回环境store2
                store2

        loop store1 //循环执行

    // | ForPrimary (stmt, e2, e3, body) -> //for循环
    //     let (store1) = exec stmt locEnv gloEnv store //计算初始化表达式的值，得到更新过的store1
    //     //定义while循环的辅助函数 loop【这里store是更新过的store1】
    //     let rec loop store1 =
    //         let (v2, store2) = eval e2 locEnv gloEnv store1 //计算循环条件的值

    //         if v2 <> 0 then // 如果循环条件不为0，就先执行函数体body，然后计算e3
    //             let store3 = exec body locEnv gloEnv store2 //exec若是表达式的话，返回的就是一个更新过的store3
    //             let (v3, store4) = eval e3 locEnv gloEnv store3 //计算表达式e3的值，返回更新过的store4
    //             loop store4 //循环执行，传入的是store4
    //         else // 如果循环条件为0，退出循环，返回环境store2
    //             store2

    //     loop store1 //循环执行

    | ForInExpr (acc,e1,e2, e3, body) -> //forin函数
        let (loc, store1) = access acc locEnv gloEnv store //取变量acc1的地址和环境
        let (v1, store2) = eval e1 locEnv gloEnv store1 //计算表达式e1，返回更新过的store2
        let (v2, store3) = eval e2 locEnv gloEnv store2 //计算表达式e2，返回更新过的store3
        let (v3, store4) = eval e3 locEnv gloEnv store3 //计算表达式e3，返回更新过的store4
        
        let rec loop step store5 = //step为步进值
            let store6 = exec body locEnv gloEnv (setSto store5 loc step)//每次刚进入循环就执行一遍函数体body

            //循环条件：当步进值为正，结束下标-(当前开始下标+步进值)大于0；当步进值为负，(当前开始下标+步进值)-结束下标大于0
            if (v2-(step+v3) > 0 && v1<v2 && v3>0) || (step+v3-v2 > 0 && v1>v2 && v3<0) then //如果循环条件大于0，表示还在循环中
                loop (step+v3) store6 //循环执行，传入的是store5，步进值为 (旧步进值+v3)
            else //如果循环条件大于等于0，表示退出循环，返回环境store4
                store6

        if (v1<v2 && v3>0) || (v1>v2 && v3<0) then //进入循环，变量的初值为v1，步进值为正或负
            loop v1 store4
        else //不满足初始循环条件，就返回store4
            store4

    | IfWithoutElse (e, stmt1) ->   //if语句
        let (v, store1) = eval e locEnv gloEnv store

        if v <> 0 then
            exec stmt1 locEnv gloEnv store1 //True分支
        else
            exec (Expr(Prim2("<",CstI 1,CstI 0))) locEnv gloEnv store1 //else分支
    
    | DoWhile (stmt1, e) -> //dowhile循环
        let store1=exec stmt1 locEnv gloEnv store//先执行一遍函数体body

        //定义dowhile循环的辅助函数 loop
        let rec loop store1 =
            //计算表达式e的值，返回更新过的store2
            let (v, store2) = eval e locEnv gloEnv store1
            // 继续循环
            if v <> 0 then
                loop (exec stmt1 locEnv gloEnv store2)
            // 退出循环，返回环境store2
            else
                store2

        loop store1

    | DoUntil (stmt1, e) -> //dountil循环
        let store1=exec stmt1 locEnv gloEnv store//先执行一遍函数体body

        //定义dountil循环的辅助函数 loop
        let rec loop store1 =
            //计算表达式e的值，返回更新过的store2
            let (v, store2) = eval e locEnv gloEnv store1
            // 退出循环，返回环境store2
            if v <> 0 then
                store2
            // 继续循环
            else
                loop (exec stmt1 locEnv gloEnv store2)

        loop store1

    | Switch (e,stmt1) -> //switchcase
        let (v,store1)=eval e locEnv gloEnv store//执行switch表达式

        //定义switch辅助函数body
        let rec body list = 
            match list with
            | Case(e2, stmt2) :: stmts -> //e2是常量表达式，stmt2是当前匹配的case语句，stmts是未匹配的case语句列表
                let (v2,store2)=eval e2 locEnv gloEnv store1//执行语句case语句中的表达式

                //和switch表达式进行比较
                if v = v2 then//匹配case语句的条件
                    exec stmt2 locEnv gloEnv store2//执行
                else//不匹配case语句的条件
                    body stmts//继续进行匹配

            | _ -> store1 //未匹配

        body stmt1
    // | Break -> store

and stmtordec stmtordec locEnv gloEnv store =
    match stmtordec with
    | Stmt stmt -> (locEnv, exec stmt locEnv gloEnv store) //为语句分配空间
    | Dec (typ, x) -> allocate (typ, x) locEnv store //局部变量调用allocate函数分配空间
    | DecAndAssign (typ, x, expr) -> //局部变量初始化
        let (locEnv1 ,store1) = allocate (typ, x) locEnv store //调用allocate函数，
                                                               //为类型为typ的变量x在局部环境和store上分配空间，
                                                               //这里返回的locEnv1就是该变量的局部环境
        let (loc, store2) = access (AccVar x) locEnv1 gloEnv store1 //计算左值变量x的地址和更新过的store
        let (res, store3) = eval expr locEnv gloEnv store2 //计算表达式expr，返回值和更新过的store
        (locEnv1, setSto store3 loc res) //返回局部环境locEnv，把expr的值赋值给store3在loc位置上的变量，也就是赋值给变量x


(* 计算micro-C表达式 *)
//输入: 表达式 expr
//输出: 表达式 expr的值(这个值是int类型)，和，被修改的store
//  eval : expr -> locEnv -> gloEnv -> store -> int * store
and eval e locEnv gloEnv store : int * store =
    match e with
    | Access acc -> //左值
        let (loc, store1) = access acc locEnv gloEnv store //取要求的acc的地址和环境
                                                                        //这里loc就是得到的acc地址（int类型）
                                                                        //这里store1就是acc的环境
        (getSto store1 loc, store1) //返回的是一个元组，左边是在得到的环境store1中根据
                                                  //acc地址loc找到该地址上的值，右边是环境
    | Assign (acc, e) -> //赋值
        let (loc, store1) = access acc locEnv gloEnv store //取要求的acc的地址和环境
        let (res, store2) = eval e locEnv gloEnv store1 //计算表达式e的值，并得到环境
        (res, setSto store2 loc res) //上一行得到的表达式e的值作为元组左边，把该值赋值给acc的地址
                                                   //并设置到store中
    | CstI i -> (i, store) //int类型变量
    | CstC c -> ((int c), store)
    | CstF f -> 
        let bytes = System.BitConverter.GetBytes(float32(f))
        let v = System.BitConverter.ToInt32(bytes, 0)
        (v, store)
    | Addr acc -> access acc locEnv gloEnv store //取要求的acc的地址
    | Prim1 (ope, e1) -> //一元基本算子
        let (i1, store1) = eval e1 locEnv gloEnv store //计算表达式e1的值，并得到环境 

        let res =
            match ope with //模式匹配
            | "!" -> if i1 = 0 then 1 else 0 //取反
            | "printi" ->
                (printf "%d " i1
                 i1)
            | "printc" ->
                (printf "%c" (char i1)
                 i1)
            | "~" -> ~~~i1
            | _ -> failwith ("unknown primitive " + ope)

        (res, store1) //返回模式匹配计算到的值和存储
    | Prim2 (ope, e1, e2) -> //二元基本算子
        let (i1, store1) = eval e1 locEnv gloEnv store //计算表达式e1，第一个参数的store
        let (i2, store2) = eval e2 locEnv gloEnv store1 //计算表达式e2，第二个参数的store

        let res =
            match ope with
            | "*" -> i1 * i2
            | "+" -> i1 + i2
            | "-" -> i1 - i2
            | "/" -> i1 / i2
            | "%" -> i1 % i2
            | "==" -> if i1 = i2 then 1 else 0
            | "!=" -> if i1 <> i2 then 1 else 0
            | "<" -> if i1 < i2 then 1 else 0
            | "<=" -> if i1 <= i2 then 1 else 0
            | ">=" -> if i1 >= i2 then 1 else 0
            | ">" -> if i1 > i2 then 1 else 0
            | "<<" -> i1 <<< i2
            | ">>" -> i1 >>> i2
            | "&" -> i1 &&& i2
            | "|" -> i1 ||| i2
            | "^" -> i1 ^^^ i2
            | _ -> failwith ("unknown primitive " + ope)

        (res, store2)
    | Prim3 (ope, acc, e) -> //复合赋值运算符
        let (loc, store1) = access acc locEnv gloEnv store //取要求的acc的地址和环境store1
        let v1=getSto store1 loc //得到acc地址上的值
        let (v2, store2) = eval e locEnv gloEnv store1 //计算表达式e的值，并得到新环境store2
        
        let res= //匹配五种复合赋值运算符，得到计算结果
            match ope with
            | "+=" -> v1 + v2
            | "-=" -> v1 - v2
            | "*=" -> v1 * v2
            | "/=" -> v1 / v2
            | "%=" -> v1 % v2
            | "<<" -> v1 <<< v2
            | ">>" -> v1 >>> v2
            | _ -> failwith ("unknown primitive " + ope)
        
        (res, setSto store2 loc res) //返回的store是把计算结果存到左值acc地址上后的新store
    | TernaryOperator (e1, e2, e3) -> //三目运算符
        let (v, store1) = eval e1 locEnv gloEnv store //计算表达式e1的值
        if v <> 0 then //表达式e1不为0
            let (v2, store2) = eval e2 locEnv gloEnv store1//计算e2
            (v2,store2) //返回结果和store2
        else //表达式e1为0
            let (v2, store2) = eval e3 locEnv gloEnv store1//计算e3
            (v2,store2) //返回结果和store2
    | PreInc acc -> //前置自增
        let (loc, store1) as res = access acc locEnv gloEnv store //取要求的acc的地址和环境
        let res = getSto store1 loc //得到要求的这个acc在store1的loc位置上的值
        (res + 1, setSto store1 loc (res + 1)) //把值加一后set到store中，元组左边返回的就是这个加一后的值
    | PreDec acc -> //前置自减
        let (loc, store1) as res = access acc locEnv gloEnv store //取要求的acc的地址和环境
        let res = getSto store1 loc //得到要求的这个acc在store1的loc位置上的值
        (res - 1, setSto store1 loc (res - 1)) //把值减一后set到store中，元组左边返回的就是这个减一后的值
    | NextInc acc -> //后置自增
        let (loc, store1) as res = access acc locEnv gloEnv store //取要求的acc的地址和环境
        let res = getSto store1 loc //得到要求的这个acc在store1的loc位置上的值
        (res, setSto store1 loc (res + 1)) //把值加一后set到store中，元组左边返回的是加一前自身的值
    | NextDec acc -> //后置自减
        let (loc, store1) as res = access acc locEnv gloEnv store //取要求的acc的地址和环境
        let res = getSto store1 loc //得到要求的这个acc在store1的loc位置上的值
        (res, setSto store1 loc (res - 1)) //把值减一后set到store中，元组左边返回的是减一前自身的值
    | Andalso (e1, e2) -> //e1 && e2
        let (i1, store1) as res = eval e1 locEnv gloEnv store //计算表达式e1的值和环境

        if i1 <> 0 then //表达式e1值不为0，就计算表达式e2的值和环境
            eval e2 locEnv gloEnv store1
        else //表达式e1值为0，根据&&规则，结果就是res（即表达式e1的值=0）
            res
    | Orelse (e1, e2) -> //e1 || e2
        let (i1, store1) as res = eval e1 locEnv gloEnv store //计算表达式e1的值和环境

        if i1 <> 0 then //表达式e1值不为0，根据||规则，结果就是res（即表达式e1的值<>0）
            res
        else //表达式e1值为0，，就计算表达式e2的值和环境
            eval e2 locEnv gloEnv store1
    | Call (f, es) -> callfun f es locEnv gloEnv store //函数调用

//输入: 待求值的access类型（变量x，指针*p，数组a[4]）
//返回: access 的左值（地址），和，store
//  access : access -> locEnv -> gloEnv -> store -> address * store
and access acc locEnv gloEnv store : int * store =
    match acc with
    | AccVar x -> (lookup (fst locEnv) x, store) //变量
    | AccDeref e -> eval e locEnv gloEnv store   //指针
    | AccIndex (acc, idx) -> //数组索引
        let (a, store1) = access acc locEnv gloEnv store //先计算左值acc的地址和环境
        let aval = getSto store1 a //得到acc的地址上的值
        let (i, store2) = eval idx locEnv gloEnv store1 //计算store1中，索引是idx的值
        (aval + i, store2) //首地址+i（i就是偏移量） 更改后的store

and evals es locEnv gloEnv store : int list * store =
    match es with
    | [] -> ([], store)
    | e1 :: er ->
        let (v1, store1) = eval e1 locEnv gloEnv store
        let (vr, storer) = evals er locEnv gloEnv store1
        (v1 :: vr, storer)

and callfun f es locEnv gloEnv store : int * store =

    msg
    <| sprintf "callfun: %A\n" (f, locEnv, gloEnv, store)

    let (_, nextloc) = locEnv
    let (varEnv, funEnv) = gloEnv
    let (paramdecs, fBody) = lookup funEnv f
    let (vs, store1) = evals es locEnv gloEnv store

    let (fBodyEnv, store2) =
        bindVars (List.map snd paramdecs) vs (varEnv, nextloc) store1

    let store3 = exec fBody fBodyEnv gloEnv store2
    (-111, store3)


//类型检查
let rec gettyp (e:expr) locEnv gloEnv store : typ =
    match e with
    // | Access acc -> //左值
    // | Assign (acc, e) -> //赋值
    | CstI i -> TypI //int类型变量
    | CstC c -> TypC //char
    | CstF f -> TypF //float
    // | Addr acc ->  //取要求的acc的地址
    | Prim1 (ope, e1) -> //一元基本算子
        let t1 = gettyp e1 locEnv gloEnv store
        match (ope,t1) with
        | ("!",TypI) -> TypI
        | ("printi",TypI) -> TypI
        | ("printc",TypC) -> TypC
        | ("~",TypI) -> TypI
        | _ -> failwith ("unknown primitive " + ope)
    | Prim2 (ope, e1, e2) -> //二元基本算子
        let t1 = gettyp e1 locEnv gloEnv store
        let t2 = gettyp e2 locEnv gloEnv store
        match (ope,t1,t2) with
        | ("*",TypI,TypI) -> TypI
        | ("+",TypI,TypI) -> TypI
        | ("-",TypI,TypI) -> TypI
        | ("/",TypI,TypI) -> TypI
        | ("%",TypI,TypI) -> TypI
        | ("=",TypI,TypI) -> TypB
        | ("==",TypI,TypI) -> TypB
        | ("!=",TypI,TypI) -> TypB
        | ("<",TypI,TypI) -> TypB
        | ("<=",TypI,TypI) -> TypB
        | (">=",TypI,TypI) -> TypB
        | (">",TypI,TypI) -> TypB
        | ("<<",TypI,TypI) -> TypI
        | (">>",TypI,TypI) -> TypI
        | ("&",TypI,TypI) -> TypI
        | ("|",TypI,TypI) -> TypI
        | ("^",TypI,TypI) -> TypI
        | _ -> failwith ("unknown primitive " + ope)
    // | Prim3 (ope, acc, e) -> //复合赋值运算符
    | TernaryOperator (e1, e2, e3) -> //三目运算符
        let t1 = gettyp e1 locEnv gloEnv store
        match t1 with
        | TypB -> 
            let t2 = gettyp e2 locEnv gloEnv store
            let t3 = gettyp e3 locEnv gloEnv store
            if t2 = t3 then t2
            else failwith ("branch types differ")
        | _ -> failwith ("condition not boolean")
    // | PreInc acc -> //前置自增
    // | PreDec acc -> //前置自减
    // | NextInc acc -> //后置自增
    // | NextDec acc -> //后置自减
    | Andalso (e1, e2) -> //e1 && e2
        let t1 = gettyp e1 locEnv gloEnv store
        let t2 = gettyp e2 locEnv gloEnv store
        if t1 = t2 then t1
        else failwith ("types differ")
    | Orelse (e1, e2) -> //e1 || e2
        let t1 = gettyp e1 locEnv gloEnv store
        let t2 = gettyp e2 locEnv gloEnv store
        if t1 = t2 then t1
        else failwith ("types differ")
    // | Call (f, es) -> //函数调用
    | _ -> failwith ("unimplemented checktype")
//类型检查例子
let typeCheck e = gettyp e [] [] [];;
let checkex1 = TernaryOperator(Prim2("==", CstI 11, CstI 12), CstI 111, CstI 666);;
let checkex2 = Prim2(">", CstI 11, CstI 12);;
let checkex3 = Prim2("*", CstI 2, CstI 5);;
let res = List.map typeCheck [checkex1; checkex2; checkex3];;


(* 通过初始化存储和全局环境，然后调用其“main”函数来解释完整的micro-C程序。 *)
// run 返回的结果是 代表内存更改的 store 类型
// vs 参数列表 [8,2,...]
// 可以为空 []
let run (Prog topdecs) vs =

    let ((varEnv, nextloc), funEnv, store0) = initEnvAndStore topdecs

    // mainParams 是 main 的参数列表
    let (mainParams, mainBody) = lookup funEnv "main"

    let (mainBodyEnv, store1) =
        bindVars (List.map snd mainParams) vs (varEnv, nextloc) store0


    msg
    <|

    //以ex9.c为例子
    // main的 AST
    sprintf "\nmainBody:\n %A\n" mainBody
    +

    //局部环境
    // 如
    // i 存储在store 位置0,store中下个空闲位置是1
    //([("i", 0)], 1)

    sprintf "\nmainBodyEnv:\n %A\n" mainBodyEnv
    +

    //全局环境 (变量,函数定义)
    // fac 的 AST
    // main 的 AST
    sprintf $"\nvarEnv:\n {varEnv} \nfunEnv:\n{funEnv}\n"
    +

    //当前存储
    // store 中 0号 位置存储值为 8
    // map [(0, 8)]
    sprintf "\nstore1:\n %A\n" store1

    let endstore =
        exec mainBody mainBodyEnv (varEnv, funEnv) store1

    msg $"\nvarEnv:\n{varEnv}\n"
    msg $"\nStore:\n"
    msg <| store2str endstore

    endstore

(* 例子程序在ex1.c, ex2.c等文件中 *)
