module Calculator.Parser

open System
open System.Text.RegularExpressions

type Token =
    | Natural of int
    | Float of float
    | FIdent of string
    | LParen
    | RParen
    | Plus
    | Minus
    | Star
    | Slash
    | EOF

type BinOpType =
    | Add
    | Times
    | Divide

type Expr =
    | IntLit of int
    | FloatLit of float
    | BinExpr of BinOpType * Expr * Expr
    | AppExpr of string * Expr list

let private parseHelper (f: string -> bool * 'T) =
    f
    >> function
        | true, item -> Some item
        | false, _ -> None

let private parseInt = parseHelper Int32.TryParse
let private parseFloat = parseHelper Double.TryParse

let (|Nat|_|) str =
    let intRegex = Regex("^[0-9]+") in
    let m = intRegex.Match(str) in

    if m.Success then
        parseInt m.Captures[0].Value
        |> Option.map (fun n -> n, m.Captures[0].Value.Length)
    else
        None

let (|Flt|_|) str =
    let floatRegex = Regex("^[0-9]+\.[0-9]+") in
    let m = floatRegex.Match(str) in

    if m.Success then
        parseFloat m.Captures[0].Value
        |> Option.map (fun n -> n, m.Captures[0].Value.Length)
    else
        None

let (|FIden|_|) str =
    let strRegex = Regex(@"^\\[a-z]+") in
    let m = strRegex.Match(str) in

    if m.Success then
        Some(m.Captures[0].Value, m.Captures[0].Value.Length)
    else
        None

let (|Token|_|) tkStr str =
    let regex = Regex($"^[{tkStr}]") in
    let m = regex.Match(str) in
    if m.Success then Some() else None

let lex1 str =
    match str with
    | Flt(f, m) -> Float(f), m
    | Nat(n, m) -> Natural(n), m
    | Token "(" -> LParen, 1
    | Token ")" -> RParen, 1
    | Token "+" -> Plus, 1
    | Token "-" -> Minus, 1
    | Token "*" -> Star, 1
    | Token "/" -> Slash, 1
    | FIden(id, m) -> FIdent id, m
    | _ -> EOF, 0

let rec lex (expression: string) =
    match expression with
    | "" -> []
    | s when Char.IsWhiteSpace s[0] -> lex (s[1..])
    | s -> let (tk, len) = lex1 s in tk :: lex (s[len..])

let private negate expr = BinExpr(Times, IntLit -1, expr)

let parse (tks: Token list) =
    let rec (|E1|_|) tks =
        match tks with
        | E2(left, Plus :: E1(right, s)) -> Some(BinExpr(Add, left, right), s)
        | E2(left, Minus :: E1(right, s)) -> Some(BinExpr(Add, left, negate right), s)
        | E2(expr, s) -> Some(expr, s)
        | _ -> None

    and (|E2|_|) tks =
        match tks with
        | E3(left, Star :: E2(right, s)) -> Some(BinExpr(Times, left, right), s)
        | E3(left, Slash :: E2(right, s)) -> Some(BinExpr(Divide, left, right), s)
        | E3(expr, s) -> Some(expr, s)
        | _ -> None

    and (|E3|_|) tks =
        match tks with
        | Natural n :: s -> Some(IntLit n, s)
        | Minus :: Natural n :: s -> Some(IntLit -n, s)
        | Float f :: s -> Some(FloatLit f, s)
        | Minus :: Float f :: s -> Some(FloatLit -f, s)
        | LParen :: E1(expr, RParen :: s) -> Some(expr, s)
        | FIdent id :: E3(expr, s) -> Some(AppExpr(id, [expr]), s)
        | _ -> None

    match tks with
    | E1(expr, _) -> Some expr
    | _ -> None
