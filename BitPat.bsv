/*-
 * Copyright (c) 2018 Matthew Naylor
 * Copyright (c) 2018 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

// Bit-string pattern matching for Bluespec
// Inspired by Morten Rhiger's "Type-Safe Pattern Combinators"

import List :: *;
import Printf :: *;

// list of exported features
export BitPatCore;
export BitPat;
export Guarded(..);
export GCol;
export RulesGenerator(..);
export genRules;
export pick;
export MkSwitch;
export switch;
export when;
export Pat(..);
export guarded;
export n;
export v;
export sv;
export gv;

// guarded type
typedef struct {
  Bool guard;
  a val;
} Guarded#(type a) deriving (Bits, FShow);

// collection of user type + guard
typedef Guarded#(List#(ut)) GCol#(type ut);

// Continuation combinators
function Tuple2#(GCol#(ut), t0) one(t1 v, function t0 k(t1 v)) =
  tuple2(GCol{guard: True, val: Nil}, k(v));
function Tuple2#(GCol#(ut), t2) app(function Tuple2#(GCol#(ut), t1) m(t0 v),
                               function Tuple2#(GCol#(ut), t2) n(t1 v), t0 k);
  match {.g1, .k1} = m(k);
  match {.g2, .k2} = n(k1);
  return tuple2(GCol{guard: g1.guard && g2.guard, val: append(g1.val, g2.val)}, k2);
endfunction

// Bit patterns (type synonym)
typedef function Tuple2#(GCol#(ut), t1) f(Bit#(n) x, t0 k)
  BitPatCore#(type ut, numeric type n, type t0, type t1);

typedef BitPatCore#(Bit#(0), n, t0, t1) BitPat#(numeric type n, type t0, type t1);

// Bit pattern combinators
// BitPatCore specific numeric value
function Tuple2#(GCol#(ut), t1) numBitPat(Bit#(n) a, Bit#(n) b, t1 k) =
  tuple2(GCol{guard: a == b, val: Nil}, k);
// BitPatCore variable
function Tuple2#(GCol#(ut), t0) varBitPat(Bit#(n) x, function t0 f(Bit#(n) x)) =
  one(x, f);
// BitPatCore variable of an explicit bit size
function Tuple2#(GCol#(ut), t0) sizedVarBitPat(Integer sz, Bit#(n) x,
                                          function t0 f(Bit#(n) x)) =
  (sz == valueOf(n)) ? one(x, f) : error(sprintf("BitPat::sizedVarBitPat - Expecting Bit#(%0d) variable, seen Bit#(%0d) variable", sz, valueOf(n)));
// BitPatCore variable guarded with a predicate
function Tuple2#(GCol#(ut), t0) guardedVarBitPat(function Bool pred(Bit#(n) x),
                                            Bit#(n) x,
                                            function t0 f(Bit#(n) x)) =
  tuple2(GCol{guard: pred(x), val: Nil}, f(x));
// BitPatCore concatenation
function Tuple2#(GCol#(ut), t2) catBitPat(BitPatCore#(ut, n0, t0, t1) f,
                                     BitPatCore#(ut, n1, t1, t2) g, Bit#(n2) n, t0 k)
                                     provisos (Add#(n0, n1, n2)) =
  app(f(truncateLSB(n)), g(truncate(n)), k);

// Bit pattern combinator wrappers
function BitPatCore#(ut, n, t0, t0) n(Bit#(n) x) = numBitPat(x);
function BitPatCore#(ut, n, function t0 f(Bit#(n) x), t0) v() = varBitPat;
function BitPatCore#(ut, n, function t0 f(Bit#(n) x), t0) sv(Integer x) =
  sizedVarBitPat(x);
function BitPatCore#(ut, n, function t0 f(Bit#(n) x), t0) gv(
  function Bool g(Bit#(n) x)) = guardedVarBitPat(g);
function BitPatCore#(ut, n2, t0, t2) cat(
                                 BitPatCore#(ut, n0, t0, t1) p,
                                 BitPatCore#(ut, n1, t1, t2) q)
           provisos(Add#(n0, n1, n2)) = catBitPat(p, q);

// Type class for constructing patterns
//
//   pat(p0, p1, p2, ...) = cat(p0, cat(p1, cat(p2, ...
//
typeclass Pat#(type a, type b) dependencies (a determines b);
  function a pat(b x);
endtypeclass

instance Pat#(BitPatCore#(ut, n, t0, t1), BitPatCore#(ut, n, t0, t1));
  function pat(p) = p;
endinstance

instance Pat#(function a f(BitPatCore#(ut, n1, t1, t2) y), BitPatCore#(ut, n0, t0, t1))
                  provisos(Pat#(a, BitPatCore#(ut, TAdd#(n0, n1), t0, t2)));
  function pat(x, y) = pat(cat(x, y));
endinstance

// Function to guard a pattern based on a predicate on the case subject
function BitPatCore#(ut, n, t0, t1) guarded(BitPatCore#(ut, n, t0, t1) p,
                                    function Bool g(Bit#(n) x));
  function Tuple2#(GCol#(ut), t1) wrapGuard (Bit#(n) s, t0 f);
    match {.b,.r} = p(s, f);
    b.guard = b.guard && g(s);
    return tuple2(b, r);
  endfunction
  return wrapGuard;
endfunction

/////////////////////
// Utility classes //
////////////////////////////////////////////////////////////////////////////////
// when function, applying a pattern to a subject and guarding the return value
// of the passed continuation
function Guarded#(a) when(BitPatCore#(ut, n, t, a) p, t f, Bit#(n) subject);
  Tuple2#(GCol#(ut), a) res = p(subject, f);
  return Guarded { guard: tpl_1(res).guard, val: tpl_2(res) };
endfunction
// switch var args function typeclass
typeclass MkSwitch#(type a, type n, type b) dependencies ((a, n) determines b);
  function a mkSwitch(Bit#(n) subj, List#(b) rhhs);
endtypeclass
instance MkSwitch#(List#(b), n, b);
  function mkSwitch(subj, rhss) = List::reverse(rhss);
endinstance
instance MkSwitch#(function a f(function b f(Bit#(n) subj)), n, b)
         provisos (MkSwitch#(a, n, b));
  function mkSwitch(subj, rhss, f) = mkSwitch(subj, Cons(f(subj), rhss));
endinstance
function a switch(Bit#(n) subj) provisos (MkSwitch#(a, n, b));
  return mkSwitch(subj, Nil);
endfunction
// pick function to wrap switch in combinatorial contexts
function a pick(List#(Guarded#(a)) xs);
  function Bool getGuard(Guarded#(a) x) = x.guard;
  function a getVal(Guarded#(a) x) = x.val;
  return fromMaybe(?, fmap(getVal, find(getGuard, xs)));
endfunction
// RulesGenerator typeclass
typeclass RulesGenerator#(type a);
  module rulesGenerator#(List#(Guarded#(a)) xs) (Tuple2#(Rules, PulseWire));
endtypeclass
// genRules module
module genRules#(List#(Guarded#(a)) xs) (Empty) provisos (RulesGenerator#(a));
  match {.allRules, .done} <- rulesGenerator(xs);
  addRules(allRules);
  /*
  rule stopSim (done);
    $display("time %0t - terminating simulation", $time);
    $finish(0);
  endrule
  */
endmodule

////////////
// Action //
////////////////////////////////////////////////////////////////////////////////
// RulesGenerator instance
instance RulesGenerator#(Action);
  module rulesGenerator#(List#(Guarded#(Action)) xs) (Tuple2#(Rules, PulseWire));
    PulseWire done <- mkPulseWireOR;
    function Rules createRule(Guarded#(Action) x) = rules
      rule guarded_rule (x.guard); x.val; done.send; endrule
    endrules;
    return tuple2(fold(rJoin, map(createRule, xs)), done);
  endmodule
endinstance

///////////////////
// List#(Action) //
////////////////////////////////////////////////////////////////////////////////
`define MaxListDepth 256
typedef TLog#(`MaxListDepth) MaxDepthSz;
// RulesGenerator instance
instance RulesGenerator#(List#(Action));
  module rulesGenerator#(List#(Guarded#(List#(Action))) xs) (Tuple2#(Rules, PulseWire));
    Reg#(Bit#(MaxDepthSz)) step <- mkReg(0);
    PulseWire done <- mkPulseWireOR;
    function Rules createRule(Guarded#(List#(Action)) x);
      Rules allRules = emptyRules;
      for (Integer i = 0; i < length(x.val); i = i + 1)
        allRules = rJoin(allRules, rules
          rule guarded_rule (x.guard && step == fromInteger(i));
            x.val[i];
            if (step == fromInteger(length(x.val) - 1)) begin
              step <= 0;
              done.send();
            end else step <= step + 1;
          endrule
        endrules);
      return allRules;
    endfunction
    return tuple2(fold(rJoin, map(createRule, xs)), done);
  endmodule
endinstance
