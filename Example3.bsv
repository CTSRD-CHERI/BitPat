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

function Bit#(5) bitAnd(Bit#(5) op1, Bit#(5) op2) = op1 & op2;
function Bit#(5) bitOr(Bit#(5) op1, Bit#(5) op2) = op1 | op2;

module top ();

  Reg#(Bit#(5)) cycle <- mkReg(0);
  rule countCycles;
    cycle <= cycle + 1;
  endrule

  Reg#(Bit#(11)) instr <- mkRegU;
  rule instAnd (cycle == 0); instr <= 11'b00101010101; endrule
  rule instOr  (cycle == 1); instr <= 11'b10101010101; endrule

  Bit#(5) res = pick(switch(instr,
    whenPat(pat(n(1'b0), v, v), bitAnd),
    whenPat(pat(n(1'b1), v, v), bitOr)
  ));

  rule doDisplay (cycle > 0);
    $display("time %0t - instr = 0b%011b, res = 0b%05b", $time, instr, res);
  endrule

  rule endSim (cycle > 1);
    $finish(0);
  endrule

endmodule
