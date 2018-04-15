# BitPat

BitPat is a bit-string pattern matching library for Bluespec, inspired by [Morten Rhiger's "Type-Safe Pattern Combinators"](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/type-safe-pattern-combinators/1E3D0890F2ED1B70F80722A732756910).

## An example

To get a taste of BitPat, here's a simple instruction decoder:

```bsv
function Action add(Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - add %0d, %0d, %0d", $time, rd, rs1, rs2);
endaction;

function Action addi(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - addi %0d, %0d, %0d", $time, rd, rs1, imm);
endaction;

module top ();
  // Example instruction
  Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0110011;

  // Decoder
  genRules(
    switch(instr,
      when(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), add),
      when(pat(               v, v, n(3'b000), v, n(7'b0010011)), addi)
    )
  );
endmodule
```

The **case subject** in the first argument of `switch` is matched
against the **pattern** in the first argument of `when` and guards the
**right-hand-side** function provided in the second argument of `when`.
The `genRules` function  then derives the actual Bluespec rules to
execute the machine described.

The `n` combinator matches a specified *numeric literal*, whereas the
`v` combinator (which stands for *variable*) matches any bit string
and passes that bit string as an argument to the right-hand-side
function.  The width of the bit-string matched by `v` is inferred from
the width of the corresponding function argument on the
right-hand-side.

## Getting started

The library sources are contained in the `BitPat.bsv` file. Two
examples, `Example1.bsv` and `Example2.bsv`, are also provided and can
be built on a system with a working installation of Bluespec by typing
`make`. They can each be run with `./example1` and `./example2`
respectively.

## Library overview

The BitPat Bluespec module provides bit-pattern combinators for
composing a pattern to match a case subject of width `n`.

```bsv
typedef function Tuple2#(Bool, t1) f(Bit#(n) x, t0 k)
  BitPat#(numeric type n, type t0, type t1);
```

A `BitPat` pattern can be created using the `pat` function
with any sequence of pattern combinators as arguments.  For example,
`pat(n(8'b00010011))` is a pattern that will match a case subject
value `8'b00010011` using the `n` combinator.  Specific
fields of the case subject can be extracted using the `v` combinator.
For example, `pat(n(3'b000),v,n(2'b11))` matches a bit string
beginning with `000` and ending with `11`, with anything in between.
See the [Pattern combinators](#pattern-combinators) section for
details on available pattern combinators.

The `Guarded` polymorphic type is used to represent the result of
matching the case subject against a pattern.

```bsv
typedef struct {
  Bool guard;
  a val;
} Guarded#(type a);
```

The `guard` boolean field will carry the success/failure of a match,
and the `val` field will carry the result of the right-hand-side
function, typically an `Action`.

The `when` function

```bsv
function Guarded#(a) when(BitPat#(n, t, a) p, t f, Bit#(n) s);
```

is used to apply a pattern. It recieves a pattern `p` together with a
right-hand-side function `f`, and the case subject `s` to match
against in the form of a bit string.  The pattern `p` must be of the
same bit-width as the case subject `s`. If the right-hand-side `f`
recieves arguments, they should be declared in the pattern `p` using
an appropriate [combinator](#pattern-combinators). The sizes and
positions of the arguments of the right-hand-side `f` will define the
bit-fields in the case subject `s` that will be passed to `f`. For
example, if the right-hand-side function

```bsv
function Action add(Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  $display("time %0t - add %0d, %0d, %0d", $time, rd, rs1, rs2);
endaction;
```
and the pattern
```bsv
pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011))
```

are matched against a 32-bit case subject, then the 3 `n` pattern
combinators respectively match 7, 3 and 7 bits each, and each one of
the 3 arguments of `add` are 5 bits wide.

In the following

```bsv
Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0110011;
Guared#(Action) gAct= when(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), add, instr);
```

`gAct` is a `Guarded#(Action)` which will have its `guard` field set
to `True` because:

* the `n(7'b0000000)` pattern operating on bits 31 to 25 of the case
subject is satisfied: `instr[31:25] == 7'b0000000`

* the `n(3'b000)` pattern operating on bits 14 to 12 of the case
subject is satisfied: `instr[14:12] == 3'b000`

* the `n(7'b0110011)` pattern operating on bits 6 to 0 of the case
subject is satisfied: `instr[6:0] == 7'b0110011`

Additionally, `gAct` will have its `val` field set to the value
returned by a call to `add` with the following arguments:

* the first `v` pattern combinator corresponds to the first `Bit#(5)`
argument `rs2` and will have the value of the case subject at
position 24 to 20, that is `instr[24:20]` or `5'b00001`

* the second `v` pattern combinator corresponds to the second
`Bit#(5)` argument `rs1` and will have the value of the case
subject at position 19 to 15, that is `instr[19:15]` or `5'b00010`

* the third `v` pattern combinator corresponds to the third `Bit#(5)`
argument `rd` and will have the value of the case subject at
position 11 to 7, that is `instr[11:7]` or `5'b00011`

That is, `gAct.val` is the same as `add(instr[24:20], instr[19:15],
instr[11:7])` or `add(5'b00001, 5'b00010, 5'b00011)`.

## Extra utility functions

* The `guarded` function is provided to predicate a whole pattern:

```bsv
function BitPat#(n, t0, t1) guarded(BitPat#(n, t0, t1) p, function Bool g(Bit#(n) x))
```

The function simply wraps a classic pattern (obtained with standard
`pat` call) and takes a predicate that will be applied to the case
subject in its entirety. The final guard is the logical *and* of
this predicate and the standard guard. Note: the `gv` combinator
described in the [Pattern combinators](#pattern-combinators) section
provides a similar but more local functionality.

* The BitBat library provides a `switch` function to compose `Guarded`
types together in a `List`:

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

Here, `gActs` is a list of the `Guarded#(Action)` returned by the
`add` and `addi` when applied with the provided `instr` according to
their respective patterns. Note that it is necessary for the return
type of `add` and `addi` to be consistent (i.e. for `switch` to work,
`add` and `addi` must return the same type).

* The `RulesGenerator` typeclass defines the `rulesGenerator` module
as follows:

```bsv
typeclass RulesGenerator#(type a);
  module rulesGenerator#(List#(Guarded#(a)) xs) (Tuple2#(Rules, PulseWire));
endtypeclass
```

An instance of `RulesGenerator` is defined in the BitPat library for
the `Action` and `List#(Action)` Bluespec types. Invoking the
`rulesGenerator` module will return a set of `Rules` corresponding to
the `Action`s or sequences of `Action`s to execute when the guarding
conditions are met. The returned `PulseWire` is a done signal
indicating that the currently triggered rule/rules is/are done
executing. It simply requires to be passed a `List#(Guarded#(Action))`
or `List#(Guarded#(List#(Action)))` and can be used as follows:

```bsv
module top();
  match {.allRules, .done} <- rulesGenerator(gActs);
  addRules(allRules);
endmodule
```

* The `genRules` module is a wrapper around the `rulesGenerator` that
performs the `addRules` and does not provide explicit handles to the
generated rules or done signal. It can be used as follows:

```bsv
module top();
  genRules(gActs);
endmodule
```

## Pattern combinators

* The `n` numeric literal combinator expects a `Bit#(n)` as argument. It
advances the case subject's bit string and returns `True` on
successfull match, or simply returns `False` on failure.

```bsv
function BitPat#(n, t0, t0) n(Bit#(n) x)
```

* The `v` variable combinator takes no argument. It advances the *case
subject*'s bit string by the size of the next argument in the
continuation which gets partially applied with the value extracted
from the bit string, and always returns `True`.

```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) v()
```

* The `sv` sized variable combinator is similar to the variable
combinator, but takes an `Integer` as argument to provide more rich
compile time errors when there is a size mismatch between the pattern
and the continuation's argument.

```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) sv(Integer x)
```

* The `gv` guarded variable combinator is similar to the variable
combinator, but take a predicate function `g`. The guard returned by
the combinator is the result of `g` applied to the value extracted
from the bit string.

```bsv
function BitPat#(n, function t0 f(Bit#(n) x), t0) gv(function Bool g(Bit#(n) x))
```
