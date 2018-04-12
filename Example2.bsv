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

import BitPat :: *;
import List :: *;

// Example use of the BitPat pattern matching library

// Defining some function returning a List#(Action)
// Semantics of add instruction
function List#(Action) add(Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = cons(action
  $display("time %0t - add %0d, %0d, %0d", $time, rd, rs1, rs2);
  endaction, cons(action
  $display("time %0t - let's say there are 2 steps to an add", $time);
  endaction, Nil));
// Semantics of addi instruction with rd == 5
function List#(Action) addi_rd5(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = cons(action
  $display("time %0t - addi %0d, %0d, %0d (rd == 5)", $time, rd, rs1, imm);
endaction, Nil);
// Semantics of addi instruction with rd != 5
function List#(Action) addi_rdnot5(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = cons(action
  $display("time %0t - addi %0d, %0d, %0d (rd != 5)", $time, rd, rs1, imm);
endaction, Nil);

// defining some predicates to use in the pattern definitions
function Bool eq (Bit#(n) x, Bit#(n) y) = x == y;
function Bool neq (Bit#(n) x, Bit#(n) y) = x != y;

function Bool guardEQ5 (Bit#(32) x) = x[19:15] == 5;
function Bool guardNEQ5 (Bit#(32) x) = x[19:15] != 5;

// Note: the BitPat GuardedEntity typeclass has an instance provided for the BSV
// Action type and for the List#(Action) type. Using BitPat can be done with a
// single instance of GuardedEntity, that is, one cannot mix
// Action and List#(Action) in a single genRules(switch(...)) construct.

module top ();
  // some case subjects.
  Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0110011; // maps to add
  //Bit#(32) instr = 32'b0000000_00001_00010_000_00011_0010011; // maps to addi with rd != 5
  //Bit#(32) instr = 32'b0000000_00001_00101_000_00011_0010011; // maps to addi with rd == 5

  // call the GuardedEntity genRules method to create rules based on the List#(Action)
  match {.allRules, .done} <- genRules(
    switch(instr,
      when(pat(n(7'b0), sv(5), sv(5), n(3'b0), sv(5), n(7'b0110011)), add),
      /*
      XXX example of compile time sv error:
      when(pat(n(7'b0), sv(5), sv(8), n(3'b0), sv(5), n(7'b0110011)), add),
      */
      when(pat(v,  gv(eq(5)), n(3'b0), v, n(7'b0010011)), addi_rd5),
      /*
      when(
        guarded(pat(v,  v, n(3'b0), v, n(7'b0010011)), guardEQ5),
        addi_rd5),
      */
      //when(pat(v, gv(neq(5)), n(3'b0), v, n(7'b0010011)), addi_rdnot5)
      when(
        guarded(pat(v, v, n(3'b0), v, n(7'b0010011)), guardNEQ5),
        addi_rdnot5)
    )
  );
  addRules(allRules);
  rule stopSim (done);
    $display("time %0t - terminating simulation", $time);
    $finish(0);
  endrule
endmodule
