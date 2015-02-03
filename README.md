proofpeer-metis
===============

A port of Joe Hurd's METIS  to Scala.

Latest version is a working automated prover without fixed models. All our models are randomly generated, with the exception that equality is always interpreted as equality.
  
The exact number of resolution steps needed in proofs differs marginally from Hurd's, possibly due to either of two reasons:

1. the implementation involves passing clauses around as lists, and we do not put any guarantees on the order of elements, which can give an unpredictable search order if these clauses carry equal weight and utility.

2. model generation is random and non-deterministic

The port has been tested on the first ten set theory problems from TPTP, and two problems in grop theory, all of which are theorems, and solvable by METIS. We provide the number of resolution steps needed by Hurd's original prover (with and without random model checking) and our own.

Problem   | Hurd (no mc) | Scala (no mc) | Hurd (mc) | Scala (mc)
----------|--------------|---------------|-----------|-----------
SET001    |           16 |            16 |        17 |         16
SET002    |           48 |            48 |        49 |         51
SET003    |           25 |            25 |        25 |         25
SET004    |           26 |            26 |        28 |         26
SET005    |          494 |           508 |       505 |        522
SET006    |           25 |            25 |        27 |         25
SET007    |         1091 |          1099 |      1485 |       1649
SET008    |           89 |            91 |        94 |         94
SET009    |          181 |           162 |       216 |        134
SET010    |         2250 |          2230 |      2814 |       2242
GRP001-2  |           22 |            21 |        14 |         21
GRP001-4  |           19 |            19 |        17 |         17

