module Klang.Config where
import Klang.Type
import Klang.Ops

kFunc1 :: [(String, KFunc1)]
kFunc1 = [
    ("not", notOp),
    ("car", carOp),
    ("cdr", cdrOp)]

kFunc2 :: [(String, KFunc2)]
kFunc2 = [
    ("and", andOp),
    ("or", orOp),
    ("+", addOp),
    ("-", subOp),
    ("*", mulOp),
    ("/", divOp),
    ("=", eqOp),
    ("<", ltOp),
    ("<=", leOp),
    (">", gtOp),
    (">=", geOp),
    ("cons", consOp)
    ]
