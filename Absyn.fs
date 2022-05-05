(* File MicroC/Absyn.fs
   Abstract syntax of micro-C, an imperative language.
   sestoft@itu.dk 2009-09-25

   Must precede Interp.fs, Comp.fs and Contcomp.fs in Solution Explorer
   必须先于解决方案资源管理器中的Interp.fs, Comp.fs和Contcomp.fs
 *)

module Absyn

// 基本类型
// 注意，数组、指针是递归类型
// 这里没有函数类型，注意与上次课的 MicroML 对比
// 变量出现在赋值表达式的左边 ---> 左值
// 变量出现在赋值表达式的右边 ---> 右值
type typ =                           // 类型
  | TypI                             (* Type int                    *) //int类型
  | TypC                             (* Type char                   *) //char类型
  | TypA of typ * int option         (* Array type                  *) //数组类型
  | TypP of typ                      (* Pointer type                *) //指针类型
                                                                   
and expr =                           // 表达式，右值                                                
  | Access of access                 (* x    or  *p    or  a[e]     *) //访问左值（右值）
  | Assign of access * expr          (* x=e  or  *p=e  or  a[e]=e   *) //赋值语句
  | Addr of access                   (* &x   or  &*p   or  &a[e]    *) //取地址
  | CstI of int                      (* Constant                    *) //int类型常量
  | Prim1 of string * expr           (* Unary primitive operator    *) //一元基本算子
  | Prim2 of string * expr * expr    (* Binary primitive operator   *) //二元基本算子
  | Andalso of expr * expr           (* Sequential and              *) //与
  | Orelse of expr * expr            (* Sequential or               *) //或
  | Call of string * expr list       (* Function call f(...)        *) //函数调用
                                                                   
and access =                         //左值，存储的位置                                            
  | AccVar of string                 (* Variable access        x    *) //变量
  | AccDeref of expr                 (* Pointer dereferencing  *p   *) //指针，左值代表地址p本身
  | AccIndex of access * expr        (* Array indexing         a[e] *) //数组索引
                                                                   
and stmt =                           //语句
  | If of expr * stmt * stmt         (* Conditional                 *) //if语句
  | While of expr * stmt             (* While loop                  *) //while语句
  | Expr of expr                     (* Expression statement   e;   *) //表达式
  | Return of expr option            (* Return from method          *) //返回
  | Block of stmtordec list          (* Block: grouping and scope   *) //语句块
  // 语句块内部，可以是 变量声明 或 语句的列表

and stmtordec =                                                    
  | Dec of typ * string              (* Local variable declaration  *) //局部变量声明
  | Stmt of stmt                     (* A statement                 *) //一个语句

// 顶级声明 可以是函数声明或变量声明
and topdec = 
  | Fundec of typ option * string * (typ * string) list * stmt //函数声明
  | Vardec of typ * string //变量声明

// 程序是顶级声明的列表
and program = 
  | Prog of topdec list
