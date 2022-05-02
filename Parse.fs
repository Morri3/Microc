(* 使用fslex和fsyacc对micro-C程序进行词法分析和解析 *)
module Parse

open System
open System.IO
open System.Text
open FSharp.Text
open Absyn
open Debug

(* 从字符串进行简单解析，错误报告较差 *)
let fromString (str : string) : program =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try 
      CPar.Main CLex.Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s near line %d, column %d\n" 
                  (exn.Message) (pos.Line+1) pos.Column

// 词法分析程序，info 在调试的时候被调用，显示Token
// CLex.Token 词法分析程序入口
let token buf = 
    let res = CLex.Token buf
    msg <|
          match res with
           |CPar.EOF -> sprintf "%A\n" res
           |_ -> sprintf "%A, " res
           
    res
(* 从文件解析 *)
let fromFile (filename : string) =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
      msg "\nToken:\n"
      
      //CPar.Main  语法分析主程序 
      let ast = CPar.Main token lexbuf in
        msg "\nAST:\n";
        ast
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s in file %s near line %d, column %d\n" 
                  (exn.Message) filename (pos.Line+1) pos.Column