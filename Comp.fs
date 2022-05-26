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

let isX86Instr = ref false

(* 在env中绑定声明的变量并生成代码来分配它： *)
// kind : Glovar / Locvar
let rec allocateWithMsg (kind: int -> Var) (typ, x) (varEnv: VarEnv) =
    let varEnv, instrs =
        allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv)

    msg
    <| "\nalloc\n"
       + sprintf "%A\n" varEnv
       + sprintf "%A\n" instrs

    (varEnv, instrs)

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

let makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * instr list =
    let rec addv decs varEnv funEnv =

        msg $"\nGlobal funEnv:\n{funEnv}\n"

        match decs with
        | [] -> (varEnv, funEnv, [])
        | dec :: decr ->
            match dec with
            | Vardec (typ, var) ->
                let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv
                let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv
                (varEnvr, funEnvr, code1 @ coder)
            | Fundec (tyOpt, f, xs, body) -> addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)

    addv topdecs ([], 0) []


(*
    生成 x86 代码，局部地址偏移 *8 ，因为 x86栈上 8个字节表示一个 堆栈的 slot槽位
    栈式虚拟机 无须考虑，每个栈位保存一个变量
*)
let x86patch code =
    if !isX86Instr then
        code @ [ CSTI -8; MUL ] // x86 偏移地址*8
    else
        code 
(* ------------------------------------------------------------------- *)

(* 编译micro-C语句:
   * stmt    是要编译的语句
   * varenv  是局部变量环境和全局变量环境
   * funEnv  是全局函数环境
*)
//编译语句
let rec cStmt stmt (varEnv: VarEnv) (funEnv: FunEnv) : instr list =
    match stmt with
    | If (e, stmt1, stmt2) ->
        let labelse = newLabel ()
        let labend = newLabel ()

        cExpr e varEnv funEnv
        @ [ IFZERO labelse ]
          @ cStmt stmt1 varEnv funEnv
            @ [ GOTO labend ]
              @ [ Label labelse ]
                @ cStmt stmt2 varEnv funEnv @ [ Label labend ]
    | While (e, body) ->
        let labbegin = newLabel ()
        let labtest = newLabel ()

        [ GOTO labtest; Label labbegin ]
        @ cStmt body varEnv funEnv
          @ [ Label labtest ]
            @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ]
    | Expr e -> cExpr e varEnv funEnv @ [ INCSP -1 ]
    | Block stmts ->

        let rec loop stmts varEnv =
            match stmts with
            | [] -> (snd varEnv, [])
            | s1 :: sr ->
                let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv
                let (fdepthr, coder) = loop sr varEnv1
                (fdepthr, code1 @ coder)

        let (fdepthend, code) = loop stmts varEnv

        code @ [ INCSP(snd varEnv - fdepthend) ]

    | Return None -> [ RET(snd varEnv - 1) ]
    | Return (Some e) -> cExpr e varEnv funEnv @ [ RET(snd varEnv) ]

and cStmtOrDec stmtOrDec (varEnv: VarEnv) (funEnv: FunEnv) : VarEnv * instr list =
    match stmtOrDec with
    | Stmt stmt -> (varEnv, cStmt stmt varEnv funEnv)
    | Dec (typ, x) -> allocateWithMsg Locvar (typ, x) varEnv

(* 编译micro-C表达式:

   * e       是要编译的表达式
   * varEnv  是局部变量环境和全局变量环境
   * funEnv  是全局函数环境

   净效应原理：如果表达式e的编译（cExpr e varEnv funEnv）返回
   指令序列instrs，则instrs的执行将使表达式e的右值留在堆栈顶部
   （从而用一个元素扩展当前堆栈帧).
*)
//编译右值表达式
and cExpr (e: expr) (varEnv: VarEnv) (funEnv: FunEnv) : instr list =
    match e with
    | Access acc -> cAccess acc varEnv funEnv @ [ LDI ]
    | Assign (acc, e) ->
        cAccess acc varEnv funEnv
        @ cExpr e varEnv funEnv @ [ STI ]
    | CstI i -> [ CSTI i ]
    | Addr acc -> cAccess acc varEnv funEnv
    | Prim1 (ope, e1) ->
        cExpr e1 varEnv funEnv
        @ (match ope with
           | "!" -> [ NOT ]
           | "printi" -> [ PRINTI ]
           | "printc" -> [ PRINTC ]
           | _ -> raise (Failure "unknown primitive 1"))
    | Prim2 (ope, e1, e2) ->
        cExpr e1 varEnv funEnv
        @ cExpr e2 varEnv funEnv
          @ (match ope with
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
             | "<=" -> [ SWAP; LT; NOT ]
             | _ -> raise (Failure "unknown primitive 2"))
    | PreInc acc -> cAccess acc varEnv funEnv @ [ DUP; LDI; CSTI 1; ADD; STI ]//前置自增
                                                        //先编译左值表达式acc
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //CSTI:int类型变量
                                                        //ADD:值相加
                                                        //STI:将 值 写入栈上某个位置
    | PreDec acc -> cAccess acc varEnv funEnv @ [ DUP; LDI; CSTI 1; SUB; STI ]//前置自减
                                                        //先编译左值表达式acc
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //CSTI:int类型变量
                                                        //SUB:值相减
                                                        //STI:将 值 写入栈上某个位置
    | NextInc acc -> cAccess acc varEnv funEnv @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; ADD; STI ; INCSP -1]//后置自增
                                                        //先编译左值表达式acc
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //SWAP:交换元素
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //CSTI:int类型变量
                                                        //ADD:值相加
                                                        //STI:将 值 写入栈上某个位置
                                                        //INCSP -1:释放空间
    | NextDec acc -> cAccess acc varEnv funEnv @ [ DUP; LDI; SWAP; DUP; LDI; CSTI 1; SUB; STI ; INCSP -1]//后置自减
                                                        //先编译左值表达式acc
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //SWAP:交换元素
                                                        //DUP:复制栈顶的值，并将其压入栈顶
                                                        //LDI:将 栈帧上 某位置的值入栈
                                                        //CSTI:int类型变量
                                                        //SUB:值相减
                                                        //STI:将 值 写入栈上某个位置
                                                        //INCSP -1:释放空间
    | Andalso (e1, e2) ->
        let labend = newLabel ()
        let labfalse = newLabel ()

        cExpr e1 varEnv funEnv
        @ [ IFZERO labfalse ]
          @ cExpr e2 varEnv funEnv
            @ [ GOTO labend
                Label labfalse
                CSTI 0
                Label labend ]
    | Orelse (e1, e2) ->
        let labend = newLabel ()
        let labtrue = newLabel ()

        cExpr e1 varEnv funEnv
        @ [ IFNZRO labtrue ]
          @ cExpr e2 varEnv funEnv
            @ [ GOTO labend
                Label labtrue
                CSTI 1
                Label labend ]
    | Call (f, es) -> callfun f es varEnv funEnv

(* 生成代码以访问变量、解引用指针或索引数组。编译代码的效果是在堆栈上留下一个左值 *)
//编译左值表达式
and cAccess access varEnv funEnv : instr list =
    match access with
    | AccVar x ->
        match lookup (fst varEnv) x with
        // x86 虚拟机指令 需要知道是全局变量 [GVAR addr]
        // 栈式虚拟机Stack VM 的全局变量的地址是 栈上的偏移 用 [CSTI addr] 表示
        // F# ! 操作符 取引用类型的值
        | Glovar addr, _ ->
            if !isX86Instr then
                [ GVAR addr ]
            else
                [ CSTI addr ]
        | Locvar addr, _ -> [ GETBP; OFFSET addr; ADD ]
    | AccDeref e ->
        match e with
        | Access _ -> (cExpr e varEnv funEnv)
        | Addr _ -> (cExpr e varEnv funEnv)
        | _ ->
            printfn "WARN: x86 pointer arithmetic not support!"
            (cExpr e varEnv funEnv)
    | AccIndex (acc, idx) ->
        cAccess acc varEnv funEnv
        @ [ LDI ]
          @ x86patch (cExpr idx varEnv funEnv) @ [ ADD ]

(* 生成代码以计算表达式列表： *)

and cExprs es varEnv funEnv : instr list =
    List.concat (List.map (fun e -> cExpr e varEnv funEnv) es)

(* 生成代码以计算参数es，然后调用函数f： *)

and callfun f es varEnv funEnv : instr list =
    let (labf, tyOpt, paramdecs) = lookup funEnv f
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
            | Fundec (rTy, name, argTy, body) -> Some(compilefun (rTy, name, argTy, body))
            | Vardec _ -> None)
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
