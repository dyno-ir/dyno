

# Dyno HW dialect x/z semantics
Whenever x/z behavior gets tricky, like in `if (1'bx) {} else {}`, Dyno generally only maintains 0/1 value behavior, not x/z.

To work around this:

a) Passes can define strict 'x verilog semantics flags to make them use Verilog semantics for certain Dyno instructions.

b) Extra instructions can be inserted to make x's non unknown values, e.g. `ICMP_WEQ(x, 1'b1)` makes x's true, `ICMP_WNE(x, 1'b0)` makes x's false. (Possibly gated behind flags from a)

Note that x/z semantics can't be maintained through all of synthesis. This is a band aid to make x's work for a little bit longer, but useful for sim flows or (early) synthesis correctness checks. When we enter AIG territory all bets are off though.


# Verilog X Semantics

## If Statement
Unknown select executes else.

> If [condition] evaluates to false (that is, has a zero value or the value is x or z), the first
> statement shall not execute. If there is an else statement and the cond_predicate expression is false, the else
> statement shall be executed.

## Ternary
Unknown evals both sides.

> If
> cond_predicate evaluates to an ambiguous value (x or z), then both the first expression and the second
> expression shall be evaluated [...]
> When both the first and second expressions are of integral types, if the cond_predicate evaluates to an
> ambiguous value and the expressions are not logically equivalent, their results shall be combined bit by bit

## Case (default)
Cases executed only on perfect bit match including x and z.

> In a case_expression comparison, the comparison only succeeds when each bit matches exactly with respect
> to the values 0, 1, x, and z.

## CaseX, CaseZ
z's (for casez) or x & z's (for casex) in either side are wildcards.

> Do-not-care values (z values for casez, z and x values for casex) in any bit of either the
> case_expression or the case_items shall be treated as do-not-care conditions during the comparison, and that
> bit position shall not be considered.

## Wildcard Compare
> a ==? b
> a equals b, X and Z values in b act as wildcards

Inequality version is just negated output of ==?.

## Logical &&, ||
Short circuit iff first is 0 or 1 resp. Else regular & or | behavior.

> The && and || operators shall use short circuit evaluation as follows:
> —The first operand expression shall always be evaluated.
> —For &&, if the first operand value is logically false then the second operand shall not be evaluated.
> —
> For ||, if the first operand value is logically true then the second operand shall not be evaluated.
