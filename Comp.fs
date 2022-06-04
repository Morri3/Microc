(* File MicroC/Comp.fs
   A compiler from micro-C, a sublanguage of the C language, to an
   abstract machine.  Direct (forwards) compilation without
   optimization of jumps to jumps, tail-calls etc.
   sestoft@itu.dk * 2009-09-23, 2011-11-10

   值是整数；它可以表示整数或指针，其中指针只是存储区中的地址（变量或指针或数组的基址）。

   编译时环境将全局变量映射到固定的存储地址，并将局部变量映射到当前堆栈帧相对于其底部的
   偏移量。运行时存储将位置映射为整数。这可以自由地使用指针算法，就像在real C中一样。
   编译时函数环境将函数名映射到代码标签。在生成的代码中，标签被绝对代码地址替换。

   表达式可能有副作用。函数接受类型化参数的列表，并可以选择返回结果。

   数组只能是一维且大小不变的。为简单起见，我们将数组表示为一个变量，该变量保存第一个
   数组元素的地址。这与C中处理数组类型参数的方式一致，但与处理数组类型变量的方式不一致。
   实际上，这就是B（C的前身）表示数组变量的方式。

   存储行为类似于堆栈，因此除了全局变量之外的所有数据都是堆栈分配的：变量、函数参数和数组。
*)

module Comp

open System.IO
open Absyn
open Machine
open Debug
open Backend

(* ------------------------------------------------------------------- *)
(* 简单的环境操作 *)
type 'data Env = (string * 'data) list

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr -> if x = y then v else lookup yr x

(* 全局变量具有绝对地址，局部变量具有偏移量： *)
type Var =
    | Glovar of int (* 堆栈中的绝对地址           *)
    | Locvar of int (* 相对于frame底部的地址 *)

(* 变量环境跟踪全局和局部变量，并跟踪局部变量的下一个可用偏移量 
   
ex1.c下面的的全局声明

int g ;
int h[3] 

构造的环境如下：

h 是整型数组，长度为 3，g是整数，下一个空闲位置是 5

([("h", (Glovar 4, TypA (TypI, Some 3)));
 ("g", (Glovar 0, TypI))], 5)  

实际存储布局如下：
 (0,0)(1,0)(2,0)(3,0) (4,1) ...... 
*)

type VarEnv = (Var * typ) Env * int


(* 函数环境将函数名映射到标签和参数decs *)
type Paramdecs = (typ * string) list

type FunEnv = (label * typ option * Paramdecs) Env
type EndStack = label list
let isX86Instr = ref false


(* 在env中绑定声明的变量并生成代码来分配它： *)
// kind : Glovar / Locvar
let rec allocateWithMsg (kind: int -> Var) (typ, x) (varEnv: VarEnv) = //返回变量环境和汇编指令列表
    let varEnv, instrs = //给 变量环境 和 汇编指令列表 分配空间
        allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv)

    msg
    <| "\nalloc\n"
       + sprintf "%A\n" varEnv
       + sprintf "%A\n" instrs

    (varEnv, instrs) //返回

and allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv) : VarEnv * instr list =
    msg $"allocate called!{(x, typ)}"
 
    // newloc 下个空闲存储位置
    let (env, newloc) = varEnv

    match typ with
    | TypA (TypA _, _) -> raise (Failure "allocate: array of arrays not permitted")
    | TypA (t, Some i) ->
        let newEnv =
            ((x, (kind (newloc + i), typ)) :: env, newloc + i + 1) //数组内容占用 i个位置,数组变量占用1个位置

        let code = [ INCSP i; GETSP; OFFSET(i - 1); SUB ]
        // info (fun () -> printf "new varEnv: %A\n" newEnv)
        (newEnv, code)
    | _ ->
        let newEnv =
            ((x, (kind (newloc), typ)) :: env, newloc + 1)

        let code = [ INCSP 1 ]
        // info (fun () -> printf "new varEnv: %A\n" newEnv) // 调试 显示分配后环境变化
        (newEnv, code)


(* 在env中绑定声明的参数： *)
let bindParam (env, newloc) (typ, x) : VarEnv =
    ((x, (Locvar newloc, typ)) :: env, newloc + 1)

let bindParams paras ((env, newloc): VarEnv) : VarEnv = List.fold bindParam (env, newloc) paras


(* ------------------------------------------------------------------- *)
(* 为全局变量和函数构建环境 *)
// let makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * instr list =
//     let rec addv decs varEnv funEnv =

//         msg $"\nGlobal funEnv:\n{funEnv}\n"

//         match decs with
//         | [] -> (varEnv, funEnv, [])
//         | dec :: decr ->
//             match dec with
//             | Vardec (typ, var) -> //全局变量声明
//                 let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv //调用allocateWithMsg函数为全局变量分配空间
//                 let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv //在上一行得到的新变量环境中返回变量环境和函数环境
//                 (varEnvr, funEnvr, code1 @ coder)
//             | Fundec (tyOpt, f, xs, body) -> //全局函数声明
//                 addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)
//             | VardecAndAssign (typ, var, expr) -> //变量初始化
//                 let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv //调用allocateWithMsg函数为全局变量分配空间
//                 let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv //在上一行得到的新变量环境中返回变量环境和函数环境
//                 let code2 = cAccess (AccVar x) varEnvr funEnvr
//                 (varEnvr, funEnvr, code1 @ coder @ code2)

//     addv topdecs ([], 0) []


(*
    生成 x86 代码，局部地址偏移 *8 ，因为 x86栈上 8个字节表示一个 堆栈的 slot槽位
    栈式虚拟机 无须考虑，每个栈位保存一个变量
*)
let x86patch code =
    if !isX86Instr then
        code @ [ CSTI -8; MUL ] // x86 偏移地址*8
    else
        code 

let popEndStack endStack = 
    match endStack with
        | labend :: labels -> labend
        | []        -> failwith "break 无作用域"
(* ------------------------------------------------------------------- *)
(* 编译micro-C语句:
   * stmt    是要编译的语句
   * varenv  是局部变量环境和全局变量环境
   * funEnv  是全局函数环境
*)
//编译语句
let rec cStmt stmt (varEnv: VarEnv) (funEnv: FunEnv) (endStack: EndStack ): instr list =
    match stmt with
    | If (e, stmt1, stmt2) ->
        let labelse = newLabel () //生成else语句的标签
        let labend = newLabel () //生成end语句的标签

        cExpr e varEnv funEnv //编译表达式e
        @ [ IFZERO labelse ] //如果表达式e等于0，跳到else标签
          @ cStmt stmt1 varEnv funEnv //编译语句stmt1
            @ [ GOTO labend ] //跳转到end标签
              @ [ Label labelse ] //else标签开始的地方
                @ cStmt stmt2 varEnv funEnv @ [ Label labend ] //编译语句stmt2，并连上end标签，编译结束
    | Break ->
        let labend =  endStack lablist
        [GOTO labend]
    | While (e, body) ->
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        [ GOTO labtest; Label labbegin ] //跳转到test标签；begin标签开始的地方
        @ cStmt body varEnv funEnv //编译语句stmt
          @ [ Label labtest ] //test标签
            @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ] //编译表达式e；如果不等于0跳转到begin，实现循环

    | Expr e -> cExpr e varEnv funEnv @ [ INCSP -1 ] //编译表达式

    | Block stmts -> //块

        //定义辅助函数loop
        let rec loop stmts varEnv =
            match stmts with
            | [] -> (snd varEnv, [])
            | s1 :: sr ->
                let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv
                let (fdepthr, coder) = loop sr varEnv1
                (fdepthr, code1 @ coder)

        let (fdepthend, code) = loop stmts varEnv

        code @ [ INCSP(snd varEnv - fdepthend) ]

    | Return None -> [ RET(snd varEnv - 1) ] //直接返回

    | Return (Some e) -> cExpr e varEnv funEnv @ [ RET(snd varEnv) ] //返回某些值

    | For (e1, e2, e3, body) -> //for循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签
        
        // 把for循环转换为while循环进行理解
        cExpr e1 varEnv funEnv//先编译初始化表达式e1
        @ [ INCSP -1 ]//释放空间
          @ [ GOTO labtest; Label labbegin ]//跳转到test标签；begin标签开始的地方
            @ cStmt body varEnv funEnv//编译函数体语句
              @ cExpr e3 varEnv funEnv//编译循环后的操作表达式
                @ [ INCSP -1 ]//释放空间
                  @ [ Label labtest ]//test标签
                    @ cExpr e2 varEnv funEnv//编译条件表达式e2 
                      @ [IFNZRO labbegin]//如果e2不为0，就跳转到begin标签进行循环

    | ForInExpr (acc,e1,e2, e3, body) -> //forin函数

        //把Forinrange函数转为For循环进行理解    
        let expr1=Assign (acc,e1)//左值acc初始值是e1，这也是forinrange的初始化表达式
        let expr21=Prim2("<",Access(acc),e2)//左值acc是否小于e2，就是正序的条件表达式
        let expr22=Prim2(">",Access(acc),e2)//左值acc是否大于e2，就是逆序的条件表达式
        let expr3=Prim3("+=",acc,e3)//循环后的操作表达式

        let judge1=Prim2("<",Access(acc),e2)//左值acc小于e2
        let judge2=Prim2(">",e3,CstI 0)//e3大于0
        let j1=Andalso(judge1,judge2) //正序的情况

        let judge3=Prim2(">",Access(acc),e2)//左值acc大于e2
        let judge4=Prim2("<",e3,CstI 0)//e3小于0
        let j2=Andalso(judge3,judge4) //逆序的情况

        if j1 <> (CstI 0) then
            cStmt (For (expr1, expr21, expr3, body)) varEnv funEnv
        else
            if j2 <> (CstI 0) then
                cStmt (For (expr1, expr22, expr3, body)) varEnv funEnv
            else
                cStmt (Expr(CstI 0)) varEnv funEnv

    | IfWithoutElse (e, stmt1) -> //if语句【不带else】
        let labelse = newLabel () //生成else语句的标签
        let labend = newLabel () //生成end语句的标签

        cExpr e varEnv funEnv //编译表达式e
        @ [ IFZERO labelse ] //如果表达式e等于0，跳到else标签
          @ cStmt stmt1 varEnv funEnv //编译语句stmt1
            @ [ GOTO labend ] //跳转到end标签
              @ [ Label labelse ] //else标签开始的地方
                //这里与if-else区别：这里没有编译语句stmt2
                @ [ Label labend ] //连上end标签，编译结束

    | DoWhile (stmt1, e) -> //dowhile循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        cStmt stmt1 varEnv funEnv //先编译语句stmt
        @ [ GOTO labtest; Label labbegin ] //跳转到test标签；begin标签开始的地方
          @ cStmt stmt1 varEnv funEnv //编译语句stmt
            @ [ Label labtest ] //test标签
              @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ] //编译表达式e；如果不等于0跳转到begin，实现循环

    | DoUntil (stmt1, e) -> //dountil循环
        let labbegin = newLabel () //生成begin标签
        let labtest = newLabel () //生成test标签

        cStmt stmt1 varEnv funEnv //先编译语句stmt
        @ [ GOTO labtest; Label labbegin ] //跳转到test标签；begin标签开始的地方
          @ cStmt stmt1 varEnv funEnv //编译语句stmt
            @ [ Label labtest ] //test标签
              @ cExpr e varEnv funEnv @ [ IFZERO labbegin ] //编译表达式e；如果等于0跳转到begin，实现循环

    | Switch (e, stmt1) -> //switch语句
        
        //定义辅助函数cases
        let rec cases stmt1 =
            match stmt1 with
            | Case(e2, stmt2) :: stmts -> //匹配到case语句
                // 标签要在Case里面，因为每条case的标签是不一样的
                let labend = newLabel () //生成end标签
                let labnext = newLabel () //生成next标签

                [ DUP ]//复制一个栈顶
                @ cExpr e2 varEnv funEnv//编译case常量表达式
                  @ [ EQ ]//判断switch表达式和case常量表达式是否相等
                    @ [ IFZERO labend ]//不相等，就跳转到end标签
                      @ cStmt stmt2 varEnv funEnv //相等，就编译case中的语句
                        @ [ GOTO labnext; Label labend ]//跳转到最后的next标签；end标签
                          @ cases stmts//编译剩下的case语句
                            @ [ Label labnext ]//next标签

            | _ -> [] //未匹配任何case

        cExpr e varEnv funEnv//编译switch表达式
        @ cases stmt1//编译case语句
          @ [ INCSP -1 ]//释放空间（因为复制一个栈顶元素）


//语句 或 声明
and cStmtOrDec stmtOrDec (varEnv: VarEnv) (funEnv: FunEnv) (endStack: EndStack ) : VarEnv * instr list =
    match stmtOrDec with
    | Stmt stmt -> (varEnv, cStmt stmt varEnv funEnv endStack) //语句
    | Dec (typ, x) -> allocateWithMsg Locvar (typ, x) varEnv //调用allocateWithMsg函数为局部变量分配空间
    | DecAndAssign (typ, x, expr) ->
        let (varEnv1,code) = allocateWithMsg Locvar (typ, x) varEnv //调用allocateWithMsg函数为局部变量分配空间
        let (code2) = cExpr (Assign (AccVar x, expr)) varEnv1 funEnv //获取表达式expr给该变量x赋值的汇编指令
        let res = code @ code2 @ [INCSP -1] //返回varEnv1这个变量环境 和 两个汇编指令列表的拼接，最后释放空间
        (varEnv1, res)//返回环境变量和汇编指令列表


(* 编译micro-C表达式:

   * e       是要编译的表达式
   * varEnv  是局部变量环境和全局变量环境
   * funEnv  是全局函数环境

   净效应原理：如果表达式e的编译（cExpr e varEnv funEnv）返回
   指令序列instrs，则instrs的执行将使表达式e的右值留在堆栈顶部
   （从而用一个元素扩展当前堆栈帧).
*)
//编译右值表达式
and cExpr (e: expr) (varEnv: VarEnv) (funEnv: FunEnv) : instr list = //参数：表达式e，变量环境varEnv，函数环境funEnv，返回汇编指令列表
    match e with
    | Access acc -> cAccess acc varEnv funEnv @ [ LDI ] //左值
    | Assign (acc, e) -> //赋值
        cAccess acc varEnv funEnv //计算左值acc
        @ cExpr e varEnv funEnv @ [ STI ] //把表达式e的值写入栈顶
    | CstI i -> [ CSTI i ] //int类型变量
    | Addr acc -> cAccess acc varEnv funEnv //左值acc的地址
    | Prim1 (ope, e1) -> //一元表达式
        cExpr e1 varEnv funEnv
        @ (match ope with //操作符模式匹配
           | "!" -> [ NOT ]
           | "printi" -> [ PRINTI ]
           | "printc" -> [ PRINTC ]
           | "~" -> [ BITNOT ]
           | _ -> raise (Failure "unknown primitive 1"))
    | Prim2 (ope, e1, e2) -> //二元表达式
        cExpr e1 varEnv funEnv //计算e1表达式
        @ cExpr e2 varEnv funEnv //计算e2表达式
          @ (match ope with //匹配操作符
             | "*" -> [ MUL ]
             | "+" -> [ ADD ]
             | "-" -> [ SUB ]
             | "/" -> [ DIV ]
             | "%" -> [ MOD ]
             | "==" -> [ EQ ]
             | "!=" -> [ EQ; NOT ]
             | "<" -> [ LT ]
             | ">=" -> [ LT; NOT ]
             | ">" -> [ SWAP; LT ]
             | "<=" -> [ SWAP; LT; NOT ] //指令顺序：从左往右
             | "<<" -> [ BITLEFT ]
             | ">>" -> [ BITRIGHT ]
             | "&" -> [ BITAND ]
             | "|" -> [ BITOR ]
             | "^" -> [ BITXOR ]
             | _ -> raise (Failure "unknown primitive 2"))
    | Prim3 (ope, acc, e) -> //复合赋值运算符
        cAccess acc varEnv funEnv //计算左值acc
        @ [DUP] @ [LDI] //DUP:复制栈顶的acc地址，现在栈中有两个
                        //LDI:取出栈顶的这个acc地址的值
          @ cExpr e varEnv funEnv //计算e表达式
            @ (match ope with //匹配操作符
              | "+=" -> [ ADD ] @ [STI] //栈顶acc的值+表达式e的结果，然后写入栈顶进行赋值，即set s[s[sp-1]]
              | "-=" -> [ SUB ] @ [STI]
              | "*=" -> [ MUL ] @ [STI]
              | "/=" -> [ DIV ] @ [STI]
              | "%=" -> [ MOD ] @ [STI]
              | _ -> raise (Failure "unknown primitive 3"))

    | TernaryOperator (e1,e2,e3) -> //三目运算符
        let labelse = newLabel () //生成else语句的标签
        let labend = newLabel () //生成end语句的标签
        
        cExpr e1 varEnv funEnv //计算e1表达式
        @ [ IFZERO labelse ] //如果表达式e等于0，跳到else标签
          @ cExpr e2 varEnv funEnv //编译e2表达式
            @ [ GOTO labend ] //跳转到end标签
              @ [ Label labelse ] //else标签开始的地方
                @ cExpr e3 varEnv funEnv @ [ Label labend ] //编译e3表达式，并连上end标签，编译结束

    | PreInc acc -> //前置自增
        cAccess acc varEnv funEnv
        @ [ DUP; LDI; CSTI 1; ADD; STI ]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //ADD:栈顶的acc地址的值+1
                                                        //STI:将 上一步+1后的值 写入栈顶，即set s[s[sp-1]]

    | PreDec acc -> //前置自减
        cAccess acc varEnv funEnv
        @ [ DUP; LDI; CSTI 1; SUB; STI ]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //SUB:栈顶的acc地址的值-1
                                                        //STI:将 上一步-1后的值 写入栈顶，即set s[s[sp-1]]

    | NextInc acc -> //后置自增
        cAccess acc varEnv funEnv
        @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; ADD; STI ; INCSP -1]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:将复制后的栈顶的acc地址的值入栈，即s[sp]=s[s[sp]]
                                                        //SWAP:交换栈顶和复制前的元素，交换后靠栈底的那个是左值acc原来的值
                                                        //DUP:复制栈顶的acc地址
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //ADD:栈顶的acc地址的值+1
                                                        //STI:将 上一步+1后的值 写入栈顶，即set s[s[sp-1]]，因为s[sp]=s[s[sp]]，故也就是把新值赋值给一开始的acc
                                                        //INCSP -1:释放空间

    | NextDec acc -> //后置自减
        cAccess acc varEnv funEnv
        @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; SUB; STI ; INCSP -1]
                                                        //先编译左值表达式acc，得到acc的地址
                                                        //DUP:复制栈顶的acc地址，现在栈中有两个
                                                        //LDI:将复制后的栈顶的acc地址的值入栈，即s[sp]=s[s[sp]]
                                                        //SWAP:交换栈顶和复制前的元素，交换后靠栈底的那个是左值acc原来的值
                                                        //DUP:复制栈顶的acc地址
                                                        //LDI:取出栈顶的这个acc地址的值
                                                        //CSTI 1:int类型变量，值为1
                                                        //SUB:栈顶的acc地址的值-1
                                                        //STI:将 上一步-1后的值 写入栈顶，即set s[s[sp-1]]，因为s[sp]=s[s[sp]]，故也就是把新值赋值给一开始的acc
                                                        //INCSP -1:释放空间

    | Andalso (e1, e2) -> //逻辑与
        let labend = newLabel () //生成end语句的标签
        let labfalse = newLabel () //生成false部分的标签

        cExpr e1 varEnv funEnv //编译e1表达式
        @ [ IFZERO labfalse ] //若等于0跳转到false部分
          @ cExpr e2 varEnv funEnv //编译e2表达式
            @ [ GOTO labend //跳转到end语句
                Label labfalse //false标签
                CSTI 0 //int类型变量0
                Label labend ] //end标签

    | Orelse (e1, e2) -> //逻辑或
        let labend = newLabel () //生成end语句的标签
        let labtrue = newLabel () //生成true部分的标签

        cExpr e1 varEnv funEnv //编译e1表达式
        @ [ IFNZRO labtrue ] //若等于0跳转到true部分
          @ cExpr e2 varEnv funEnv //编译e2表达式
            @ [ GOTO labend //跳转到end语句
                Label labtrue //true标签
                CSTI 1 //int类型变量1
                Label labend ] //end标签

    | Call (f, es) -> callfun f es varEnv funEnv //函数调用

(* ------------------------------------------------------------------- *)
(* 为全局变量和函数构建环境 *)
and makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * instr list =
    let rec addv decs varEnv funEnv =

        msg $"\nGlobal funEnv:\n{funEnv}\n"

        match decs with
        | [] -> (varEnv, funEnv, [])
        | dec :: decr ->
            match dec with
            | Vardec (typ, var) -> //全局变量声明
                let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv //调用allocateWithMsg函数为全局变量分配空间
                let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv //在上一行得到的新变量环境中返回变量环境和函数环境
                (varEnvr, funEnvr, code1 @ coder)
            | Fundec (tyOpt, f, xs, body) -> //全局函数声明
                addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)
            | VardecAndAssign (typ, var, expr) -> //变量初始化
                let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv //调用allocateWithMsg函数为全局变量分配空间
                let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv //在上一行得到的新变量环境中返回变量环境和函数环境
                let code2 = cAccess (AccVar var) varEnvr funEnvr //获得左值变量var的汇编指令列表
                (varEnvr, funEnvr, code1 @ coder @ code2)

    addv topdecs ([], 0) []

(* 生成代码以访问变量、解引用指针或索引数组。编译代码的效果是在堆栈上留下一个左值 *)
//编译左值表达式
and cAccess access varEnv funEnv : instr list = //返回的是汇编指令列表
    match access with
    | AccVar x -> //左值变量
        match lookup (fst varEnv) x with //找变量环境
        // x86 虚拟机指令 需要知道是全局变量 [GVAR addr]
        // 栈式虚拟机Stack VM 的全局变量的地址是 栈上的偏移 用 [CSTI addr] 表示
        // F# ! 操作符 取引用类型的值
        | Glovar addr, _ -> //全局变量
            if !isX86Instr then
                [ GVAR addr ] //是x86虚拟机指令，返回全局变量。[GVAR addr]是x86中全局变量的表示
            else
                [ CSTI addr ] //不是x86虚拟机指令，返回int类型地址
        | Locvar addr, _ -> [ GETBP; OFFSET addr; ADD ] //局部变量
                                  //GETBP：得到当前栈帧基地址
                                  //OFFSET addr：设置偏移量为addr
                                  //ADD：相加，即栈帧基地址+偏移量，就是变量的位置
    | AccDeref e -> //引用
        match e with
        | Access _ -> (cExpr e varEnv funEnv) //左值引用，返回表达式e的汇编指令列表
        | Addr _ -> (cExpr e varEnv funEnv) //地址引用，返回表达式e的汇编指令列表
        | _ ->
            printfn "WARN: x86 pointer arithmetic not support!"
            (cExpr e varEnv funEnv)
    | AccIndex (acc, idx) -> //索引
        cAccess acc varEnv funEnv //先编译左值表达式acc
        @ [ LDI ] //栈帧上acc位置的值入栈
          @ x86patch (cExpr idx varEnv funEnv) @ [ ADD ] //生成x86代码，并相加得到结果


(* 生成代码以计算表达式列表： *)
and cExprs es varEnv funEnv : instr list =
    List.concat (List.map (fun e -> cExpr e varEnv funEnv) es)


(* 生成代码以计算参数es，然后调用函数f： *)
and callfun f es varEnv funEnv : instr list =
    let (labf, tyOpt, paramdecs) = lookup funEnv f //寻找f的函数环境funEnv
    let argc = List.length es

    if argc = List.length paramdecs then
        cExprs es varEnv funEnv @ [ CALL(argc, labf) ]
    else
        raise (Failure(f + ": parameter/argument mismatch"))


(* 编译一个完整的micro-C程序：globals、main调用、函数 *)
let argc = ref 0

let cProgram (Prog topdecs) : instr list =
    let _ = resetLabels ()
    let ((globalVarEnv, _), funEnv, globalInit) = makeGlobalEnvs topdecs

    let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = lookup funEnv f
        let paraNums = List.length paras
        let (envf, fdepthf) = bindParams paras (globalVarEnv, 0)
        let code = cStmt body (envf, fdepthf) funEnv

        [ FLabel (paraNums, labf) ]
        @ code @ [ RET(paraNums - 1) ]

    let functions =
        List.choose
            (function
            | Fundec (rTy, name, argTy, body) -> Some(compilefun (rTy, name, argTy, body)) //函数声明
            | Vardec _ -> None //变量声明
            | VardecAndAssign _ -> None) //变量初始化
            topdecs

    let (mainlab, _, mainparams) = lookup funEnv "main"
    argc := List.length mainparams

    globalInit
    @ [ LDARGS !argc
        CALL(!argc, mainlab)
        STOP ]
      @ List.concat functions


(* 编译一个完整的micro-C，并将生成的指令列表写入fname文件；另外，将程序
   作为指令列表返回。
*)
let intsToFile (inss: int list) (fname: string) =
    File.WriteAllText(fname, String.concat " " (List.map string inss))

let writeInstr fname instrs =
    let ins =
        String.concat "\n" (List.map string instrs)

    File.WriteAllText(fname, ins)
    printfn $"VM instructions saved in file:\n\t{fname}"


let compileToFile program fname =

    msg <|sprintf "program:\n %A" program

    let instrs = cProgram program

    msg <| sprintf "\nStack VM instrs:\n %A\n" instrs

    writeInstr (fname + ".ins") instrs

    let bytecode = code2ints instrs
    msg <| sprintf "Stack VM numeric code:\n %A\n" bytecode
    
    // 面向 x86 的虚拟机指令 略有差异，主要是地址偏移的计算方式不同
    // 单独生成 x86 的指令
    isX86Instr := true
    let x86instrs = cProgram program
    writeInstr (fname + ".insx86") x86instrs

    let x86asmlist = List.map emitx86 x86instrs
    let x86asmbody =
        List.fold (fun asm ins -> asm + ins) "" x86asmlist

    let x86asm =
        (x86header + beforeinit !argc + x86asmbody)

    printfn $"x86 assembly saved in file:\n\t{fname}.asm"
    File.WriteAllText(fname + ".asm", x86asm)

    // let deinstrs = decomp bytecode
    // printf "deinstrs: %A\n" deinstrs
    intsToFile bytecode (fname + ".out")

    instrs

(* Example programs are found in the files ex1.c, ex2.c, etc *)
