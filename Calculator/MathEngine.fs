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
        SmallFrac(a.n * b.n, a.d * b.d) |> simplify

    static member (/)(a: SmallFrac, b: SmallFrac) =
        SmallFrac(a.n * b.d, a.d * b.n) |> simplify

    static member (+)(a: SmallFrac, b: SmallFrac) =
        let lcm = lcm a.d b.d in SmallFrac(a.n * lcm / a.d + b.n * lcm / b.d, lcm) |> simplify

    static member (-)(a: SmallFrac, b: SmallFrac) =
        let lcm = lcm a.d b.d in SmallFrac(a.n * lcm / a.d - b.n * lcm / b.d, lcm) |> simplify

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

    static member coerceCDown (x: complex) : k =
        if complex.IsRealNumber x then Float(x.Real) else Complex(x)

    static member coerceFDown (x: float) : k = Float(x)

    static member coerceFrDown (x: SmallFrac) =
        if x.d = 1 then Integer(x.n) else Fraction(x)

    static member toComplex x =
        match x with
        | Integer i -> complex (i, 0)
        | Fraction f -> complex (float f, 0)
        | Float f -> complex (f, 0)
        | Complex c -> c

    static member toFloat x =
        match x with
        | Integer i -> float i
        | Fraction f -> SmallFrac.op_Implicit f
        | Float f -> f
        | Complex c -> failwith "complex are not floats!"

    static member toFrac x =
        match x with
        | Integer i -> SmallFrac(i, 1)
        | Fraction f -> f
        | Float f -> failwith "floats are not fractions!"
        | Complex c -> failwith "complex are not fractions!"

    static member polyOp opC opF opFr opI (a: k, b: k) : k =
        match (a, b) with
        | Complex a, b
        | b, Complex a -> k.coerceCDown (opC a (k.toComplex b))
        | Float a, b
        | b, Float a -> k.coerceFDown (opF a (k.toFloat b))
        | Fraction a, b
        | b, Fraction a -> k.coerceFrDown (opFr a (k.toFrac b))
        | Integer a, Integer b -> k.coerceFrDown(opI a b)

    static member (+)(a, b) = k.polyOp add add add addi (a, b)
    static member (-)(a, b) = k.polyOp add add add addi (a, b)
    static member (*)(a, b) = k.polyOp mult mult mult multi (a, b)
    static member (/)(a, b) = k.polyOp div div div intDiv (a, b)

let funCtx func =
    match func with
    | "sq" -> fun (x: k list) -> x[0] * x[0]
    | s -> failwith "unknown!"

let rec compute expr =
    match expr with
    | IntLit n -> Integer n
    | FloatLit f -> Float f
    | ComplexLit c -> Complex c
    | BinExpr(Add, left, right) -> compute left + compute right
    | BinExpr(Times, left, right) -> compute left * compute right
    | BinExpr(Divide, left, right) -> compute left / compute right
    | AppExpr(func, expr) -> funCtx func [ compute expr[0] ]
    | JuxExpr(left, right) -> compute left * compute right

let float (frac: SmallFrac) = float frac.n / float frac.d
