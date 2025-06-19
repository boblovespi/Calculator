module Calculator.Program

open System.Numerics
open Avalonia
open Calculator.Parser
open Calculator.MathEngine

let strTk lex =
    match lex with
    | Natural n -> $"%d{n}"
    | Token.Float f -> $"%f{f}"
    | LParen -> "("
    | RParen -> ")"
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Slash -> "/"
    | FIdent s -> s
    | EOF -> "[EOF]"

let strTks = List.fold (fun s tk -> s + strTk tk) ""

let strC (c: Complex) = $"{c.Real}+{c.Imaginary}i"

let rec strAst expr =
    match expr with
    | IntLit n -> $"{n}"
    | FloatLit f -> $"{f}"
    | BinExpr(binOpType, expr, expr1) ->
        match binOpType with
        | Add -> $"(+ {strAst expr} {strAst expr1})"
        | Times -> $"(* {strAst expr} {strAst expr1})"
        | Divide -> $"(/ {strAst expr} {strAst expr1})"
    | AppExpr(s, exprs) -> $"""({s} {List.fold (fun s q -> s + (strAst q)) "" exprs})"""
    | ComplexLit complex -> $"{strC complex}"
    | JuxExpr(expr, expr1) -> $"({strAst expr} {strAst expr1})"

let decimals x =
    match x with
    | Complex c -> $"{strC c}"
    | Integer i -> $"{i}"
    | Fraction f -> $"{float f}"
    | Float f -> $"{f}"


[<EntryPoint>]
let main args =
    // printf "hello, world\n"
    // while true do
    //     let str = System.Console.ReadLine()
    //     let tokens = str |> lex
    //     tokens |> strTks |> printf "%s\n"
    //     let expr = tokens |> parse
    //
    //     if expr.IsSome then
    //         expr |> Option.map strAst |> Option.iter (printf "%s\n")
    //     else
    //         printf "uh oh\n"
    //
    //     if expr.IsSome then
    //         let value = compute expr.Value in printf $"{value}, {decimals value}\n"
    AppBuilder
        .Configure<App.App>()
        .UsePlatformDetect()
        .UseSkia()
        .With(1)
        .StartWithClassicDesktopLifetime(args)
