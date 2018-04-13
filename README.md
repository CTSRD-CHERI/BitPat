# BitPat
BitPat is a bit-string pattern matching library for Bluespec, inspired by [Morten Rhiger's "Type-Safe Pattern Combinators"](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/type-safe-pattern-combinators/1E3D0890F2ED1B70F80722A732756910).

The library sources are contained in the `BitPat.bsv` file. Two examples, `Example1.bsv` and `Example2.bsv`, are provided and can be built on a system with a working instalation of Bluespec by typing make. They can each be run with `./example1` and `./example2` respectively.

## General working principle
The BitPat Bluespec module provides bit-pattern combinators for composing a pattern to match a tested bit string.
```bsv
typedef function Tuple2#(Bool, t1) f(Bit#(n) x, t0 k)
  BitPat#(numeric type n, type t0, type t1);
```
A `BitPat` object can be created using the `pat` function with a combination of pattern combinators.
For example, `pat(n(8'b00010011))` is a pattern that will match a `Bit#(8)` value of `8'b00010011` using the `n` combinator.
Specific fields of a bit string can be extracted using the `v` combinator: `pat(n(3'b000),v,n(2'b11))`.
See the [Pattern combinators](#Pattern-combinators) section for details on available pattern combinators. The `Guarded` polymorphic type is used to represent the result of applying a bit-pattern to a bit string.
```bsv
typedef struct {
  Bool guard;
  a val;
} Guarded#(type a);
```
The `guard` boolean field will carry the result of a match, and the `val` field will carry the guarded value, typically an `Action`.

The `when` function
```bsv
function Guarded#(a) when(BitPat#(n, t, a) p, t f, Bit#(n) s);
```
is used to apply a `BitPat`. It recieves a `BitPat` `p` together with a function `f` returning the value to be guarded by the pattern, and the bit string `s` to match against.
`p` must be of the same bit witdth as `s`. If `f` recieves arguments, they should be declared in `p` using an appropriate [combinator](#Pattern-combinators). The argument sizes and positions will define the bits in `s` that will be passed to `f`. For example, the function
```bsv
function Action add(Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - add %0d, %0d, %0d", $time, rd, rs1, rs2);
endaction;
```
and the pattern
```bsv
pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011))
```
expect a 32-bit bit string to match against: the 3 `n` pattern combinators respectivelly expect 7, 3 and 7 bits each, and each one of the 3 arguments of `add` are 5 bits wide.

In the following
```bsv
Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0110011;
Guared#(Action) gAct= when(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), add, instr);
```
`gAct` is a `Guarded#(Action)` which will have its `guard` field set to `True` because:

* the `n(7'b0000000)` pattern operating on bits 31 to 25 of the bit string is satisfied: `instr[31:25] == 7'b0000000`
* the `n(3'b000)` pattern operating on bits 14 to 12 of the bit string is satisfied: `instr[14:12] == 3'b000`
* the `n(7'b0110011)` pattern operating on bits 6 to 0 of the bit string is satisfied: `instr[6:0] == 7'b0110011`

Additionally, `gAct` will have its `val` field set to the returned value of `add` with its arguments passed as follows:

* the first `v` pattern combinator corresponds to the first `Bit#(5)` argument `rs2` and will have the value of the bit string at position 24 to 20, that is `instr[24:20]` or `5'b00001`
* the second `v` pattern combinator corresponds to the second `Bit#(5)` argument `rs1` and will have the value of the bit string at position 19 to 15, that is `instr[19:15]` or `5'b00010`
* the third `v` pattern combinator corresponds to the third `Bit#(5)` argument `rd` and will have the value of the bit string at position 11 to 7, that is `instr[11:7]` or `5'b00011`

(That is, `gAct.val` is the same as `add(instr[24:20], instr[19:15], instr[11:7])` or `add(5'b00001, 5'b00010, 5'b00011)`)

## Extra utility functions
* On top of the `gv` combinator described in the [Pattern combinators](#Pattern-combinators) section, a `guarded` function is provided to predicate a whole pattern:
```bsv
function BitPat#(n, t0, t1) guarded(BitPat#(n, t0, t1) p, function Bool g(Bit#(n) x))
```
The function simply wraps a classic pattern (obtained with standard `pat` call) and takes a predicate that will be applied to the bit string in its entirety. The final guard on the pattern is the logical and of this predicate and the standard guard.
* The BitBat library provides a `switch` function to compose `Guarded` types together in a `List`:
```bsv
function Action add(Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - add %0d, %0d, %0d", $time, rd, rs1, rs2);
endaction;
function Action addi(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - addi %0d, %0d, %0d (rd == 5)", $time, rd, rs1, imm);
endaction;
// some bit strings
Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0110011; // maps to add
//Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0010011; // maps to addi
List#(Guarded#(Action)) gActs = switch(instr,
  when(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), add),
  when(pat(v, v, n(3'b000), v, n(7'b0010011)), addi)
);
```
`gActs` is a list of the `Guarded#(Action)` returned by the `add` and `addi` when applied with the provided `instr` according to their respective patterns. Note that it is necessary for the return type of `add` and `addi` to be consistent (i.e. for `switch` to work `add` and `addi` must return the same type).

* The `RulesGenerator` typeclass defines the `genRules` module as follows:
```bsv
typeclass RulesGenerator#(type a);
  module genRules#(List#(Guarded#(a)) xs) (Tuple2#(Rules, PulseWire));
endtypeclass
```
An instance of `RulesGenerator` is defined in the BitPat library for the `Action` and `List#(Action)` Bluespec types. Invoking the `genRules` module will return a set of `Rules` corresponding to the `Action`s or sequences of `Action`s to execute when the guarding conditions are met. The returned `PulseWire` is a done signal indicating that the currently triggered rule/rules is/are done executing. It simply requires to be passed a `List#(Guarded#(Action))` or `List#(Guarded#(List#(Action)))` and can be used as follows:
```bsv
module top();
  match {.allRules, .done} <- genRules(gActs);
  addRules(allRules);
endmodule
```

## Pattern combinators
* The `n` number combinator expects a `Bit#(n)` as argument. It advances the bit string and returns `True` on successfull match, or simply returns `False` on failure.
```bsv
function BitPat#(n, t0, t0) n(Bit#(n) x)
```
* The `v` variable combinator takes no argument. It advances the bit string by the size of the next argument in the continuation which gets partially applied with the value extracted from the bit string, and always returns `True`.
```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) v()
```
* The `sv` sized variable combinator is similar to the variable combinator, but takes an `Integer` as argument to provide more rich compile time errors when there is a size mismatch between the pattern and the continuation's argument.
```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) sv(Integer x)
```
* The `gv` guarded variable combinator is similar to the variable combinator, but take a predicate function `g`. The guard returned by the combinator is the result of `g` applied to the value extracted from the bit string.
```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) gv(function Bool g(Bit#(n) x))
```
