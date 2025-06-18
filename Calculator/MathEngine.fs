module Calculator.MathEngine

open System.Numerics
open Parser

let private abs n = if n > 0 then n else -n

let rec private gcd n d =
    match (abs n, abs d) with
    | n, 0 -> n
    | n, d -> gcd d (n % d)

let private lcm n d = abs n * (abs d / gcd n d)

let inline private add a b = a + b
let inline private mult a b = a * b
let inline private div a b = a / b

type complex = Complex

type SmallFrac =
    struct
        val n: int
        val d: int
        new(n, d) = { n = n; d = d }
    end

    static let simplify (frac: SmallFrac) =
        let gcd = gcd (if frac.n > 0 then frac.n else -frac.n) frac.d in SmallFrac(frac.n / gcd, frac.d / gcd)

    static member (*)(a: SmallFrac, b: SmallFrac) =
        simplify (SmallFrac(a.n * b.n, a.d * b.d))

    static member (/)(a: SmallFrac, b: SmallFrac) =
        simplify (SmallFrac(a.n * b.d, a.d * b.n))

    static member (+)(a: SmallFrac, b: SmallFrac) =
        let lcm = lcm a.d b.d in SmallFrac(a.n * lcm / a.d + b.n * lcm / b.d, lcm)

    static member (-)(a: SmallFrac, b: SmallFrac) =
        let lcm = lcm a.d b.d in SmallFrac(a.n * lcm / a.d - b.n * lcm / b.d, lcm)

    static member op_Implicit(a: SmallFrac) : float = float a.n / float a.d

    override this.ToString() = $"{this.n}/{this.d}"

let private addi a b = SmallFrac(a, 1) + SmallFrac(b, 1)
let private multi a b = SmallFrac(a, 1) * SmallFrac(b, 1)
let private intDiv a b = SmallFrac(a, 1) / SmallFrac(b, 1)

type k =
    | Integer of int
    | Fraction of SmallFrac
    | Float of float
    | Complex of Complex

    static let coerceCDown (x: complex) : k =
        if complex.IsRealNumber x then Float(x.Real) else Complex(x)

    static let coerceFDown (x: float) : k = Float(x)

    static let coerceFrDown (x: SmallFrac) =
        if x.d = 1 then Integer(x.n) else Fraction(x)

    static let toComplex x =
        match x with
        | Integer i -> complex (i, 0)
        | Fraction f -> complex (float f, 0)
        | Float f -> complex (f, 0)
        | Complex c -> c

    static let toFloat x =
        match x with
        | Integer i -> float i
        | Fraction f -> SmallFrac.op_Implicit f
        | Float f -> f
        | Complex c -> failwith "complex are not floats!"

    static let toFrac x =
        match x with
        | Integer i -> SmallFrac(i, 1)
        | Fraction f -> f
        | Float f -> failwith "floats are not fractions!"
        | Complex c -> failwith "complex are not fractions!"

    static let polyOp opC opF opFr opI (a: k, b: k) : k =
        match (a, b) with
        | Complex a, b
        | b, Complex a -> coerceCDown (opC a (toComplex b))
        | Float a, b
        | b, Float a -> coerceFDown (opF a (toFloat b))
        | Fraction a, b
        | b, Fraction a -> coerceFrDown (opFr a (toFrac b))
        | Integer a, Integer b -> coerceFrDown(opI a b)

    static member (+)(a, b) = polyOp add add add addi (a, b)
    static member (-)(a, b) = polyOp add add add addi (a, b)
    static member (*)(a, b) = polyOp mult mult mult multi (a, b)
    static member (/)(a, b) = polyOp div div div intDiv (a, b)

// let toComplex x =
//     match x with
//     | Integer i -> complex (i, 0)
//     | Fraction f -> complex (float f, 0)
//     | Float f -> complex (f, 0)
//     | Complex c -> c
//
// let toFloat x =
//     match x with
//     | Integer i -> float i
//     | Fraction f -> float f
//     | Float f -> f
//     | Complex c -> failwith "complex are not floats!"
//
// let toFrac x =
//     match x with
//     | Integer i -> SmallFrac(i, 1)
//     | Fraction f -> f
//     | Float f -> failwith "floats are not fractions!"
//     | Complex c -> failwith "complex are not fractions!"
//
// let coerceCDown (x: complex) : k =
//     if Complex.IsRealNumber x then Float(x.Real) else Complex(x)
//
// let coerceFDown (x: float) : k = Float(x)
//
// let coerceFrDown (x: SmallFrac) =
//     if x.d = 1 then Integer(x.n) else Fraction(x)

let funCtx func =
    match func with
    | "\sq" -> fun (x: k list) -> x[0] * x[0]
    | s -> failwith "unknown!"

let rec compute expr =
    match expr with
    | IntLit n -> Integer n
    | FloatLit f -> Float f
    | BinExpr(Add, left, right) -> compute left + compute right
    | BinExpr(Times, left, right) -> compute left * compute right
    | BinExpr(Divide, left, right) -> compute left / compute right
    | AppExpr(func, expr) -> funCtx func [ compute expr[0] ]

let float (frac: SmallFrac) = float frac.n / float frac.d
