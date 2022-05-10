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

let bindVar x v (env, nextloc) store : locEnv * store =
    let env1 = (x, nextloc) :: env //新的环境
    msg $"bindVar:\n%A{env1}\n"

    //返回新环境，新的待分配位置+1，设置当前存储位置为值 v
    let ret = ((env1, nextloc + 1), setSto store nextloc v)
    
    msg $"locEnv:\n {fst ret}\n"
    msg $"Store:\n {store2str (snd ret)}\n"

    ret 


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

    let rec addv decs locEnv funEnv store =
        match decs with
        | [] -> (locEnv, funEnv, store)

        // 全局变量声明：调用 allocate 在store上给变量分配空间
        | Vardec (typ, x) :: decr ->
            let (locEnv1, sto1) = allocate (typ, x) locEnv store
            addv decr locEnv1 funEnv sto1

        //全局函数：将声明(f,(xs,body))添加到全局函数环境 funEnv
        | Fundec (_, f, xs, body) :: decr -> addv decr locEnv ((f, (xs, body)) :: funEnv) store

    // ([], 0) []  默认全局环境
    // locEnv ([],0) 变量环境 ，变量定义为空列表[],下一个空闲地址为0
    // ([("n", 1); ("r", 0)], 2)  表示定义了 变量 n , r. 下一个可以用的变量索引是 2
    // funEnv [] 函数环境，函数定义为空列表[]
    addv topdecs ([], 0) [] emptyStore

(* ------------------------------------------------------------------- *)
(* 解释micro-C语句 *)
//传入的参数是：语句，局部环境，全局环境，store
//返回的是store
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
            //求值 循环条件,注意变更环境 store
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

and stmtordec stmtordec locEnv gloEnv store =
    match stmtordec with
    | Stmt stmt -> (locEnv, exec stmt locEnv gloEnv store)
    | Dec (typ, x) -> allocate (typ, x) locEnv store

(* 计算micro-C表达式 *)

and eval e locEnv gloEnv store : int * store =
    match e with
    | Access acc -> //左值
        let (loc, store1) = access acc locEnv gloEnv store
        (getSto store1 loc, store1)
    | Assign (acc, e) -> //赋值
        let (loc, store1) = access acc locEnv gloEnv store
        let (res, store2) = eval e locEnv gloEnv store1
        (res, setSto store2 loc res)
    | CstI i -> (i, store) //int类型变量
    | Addr acc -> access acc locEnv gloEnv store //取地址
    | Prim1 (ope, e1) -> //一元基本算子
        let (i1, store1) = eval e1 locEnv gloEnv store

        let res =
            match ope with
            | "!" -> if i1 = 0 then 1 else 0 //取反
            | "printi" ->
                (printf "%d " i1
                 i1)
            | "printc" ->
                (printf "%c" (char i1)
                 i1)
            | _ -> failwith ("unknown primitive " + ope)

        (res, store1)
    | Prim2 (ope, e1, e2) -> //二元基本算子
        let (i1, store1) = eval e1 locEnv gloEnv store //第一个参数的store
        let (i2, store2) = eval e2 locEnv gloEnv store1 //第二个参数的store

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
            | _ -> failwith ("unknown primitive " + ope)

        (res, store2)
    | Andalso (e1, e2) -> //&&
        let (i1, store1) as res = eval e1 locEnv gloEnv store

        if i1 <> 0 then
            eval e2 locEnv gloEnv store1
        else
            res
    | Orelse (e1, e2) -> //||
        let (i1, store1) as res = eval e1 locEnv gloEnv store

        if i1 <> 0 then
            res
        else
            eval e2 locEnv gloEnv store1
    | Call (f, es) -> callfun f es locEnv gloEnv store //函数调用

and access acc locEnv gloEnv store : int * store =
    match acc with
    | AccVar x -> (lookup (fst locEnv) x, store) //变量
    | AccDeref e -> eval e locEnv gloEnv store   //指针
    | AccIndex (acc, idx) -> //数组索引
        let (a, store1) = access acc locEnv gloEnv store
        let aval = getSto store1 a
        let (i, store2) = eval idx locEnv gloEnv store1
        (aval + i, store2) //首地址+i

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
    sprintf $"\n varEnv:\n {varEnv} \nfunEnv:\n{funEnv}\n"
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
