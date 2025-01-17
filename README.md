Multicore tests [![Linux 5.0.0+trunk](https://github.com/jmid/multicoretests/actions/workflows/linux-500-workflow.yml/badge.svg)](https://github.com/jmid/multicoretests/actions/workflows/linux-500-workflow.yml) [![MacOSX 5.0.0+trunk](https://github.com/jmid/multicoretests/actions/workflows/macosx-500-workflow.yml/badge.svg)](https://github.com/jmid/multicoretests/actions/workflows/macosx-500-workflow.yml)
================================================================================

Experimental property-based tests of (parts of) the OCaml multicore compiler.

This project contains
- a randomized testsuite of OCaml 5.0 based on QCheck, packaged up in `multicoretests.opam`
- two reusable testing libraries `Lin` and `STM`, jointly packaged up in `multicorecheck.opam`

We are still experimenting with the interfaces, so consider yourself warned.


Installation instructions, and running the tests
================================================

The package `multicorecheck` can be installed independently from the
testsuite with:
```
opam install ./multicorecheck.opam
```
it exposes the two libraries `lin` and `stm`. To use e.g. `stm` in a Dune
project, you only need to add the following rule to your executable/library
specification:
```
  (libraries multicorecheck.stm)
```

The test suite can be built and run with the following commands (from
the root of this directory):
```
opam install . --deps-only --with-test
dune build
dune runtest -j1 --no-buffer --display=quiet
```

Individual tests can be run by invoking `dune exec`. For example:
```
$ dune exec src/atomic_test.exe -- -v
random seed: 51501376
generated error fail pass / total     time test name
[✓] 1000    0    0 1000 / 1000     0.2s sequential atomic test
[✓] 1000    0    0 1000 / 1000    21.7s parallel atomic test (w/repeat)
================================================================================
success (ran 2 tests)
```


A Parallel State-Machine Testing Library
========================================

`STM` contains a revision of [qcstm](https://github.com/jmid/qcstm)
extended to run parallel state-machine tests akin to [Erlang
QuickCheck, Haskell Hedgehog, ScalaCheck, ...](https://github.com/jmid/pbt-frameworks).

The module is phrased as a functor `STM.Make` expecting a programmatic
description of a model, the tested commands, and how the model changes
across commands. The resulting module contains a function
`agree_test`: it tests whether the model agrees with the
system-under-test (`sut`) across a sequential run of an arbitrary
command sequence.

`agree_test` comes in a parallel variant `agree_test_par`
which tests in parallel by `spawn`ing two domains with `Domain`
(see [lib/STM.ml](lib/STM.ml)).

Repeating a non-deterministic property can currently be done in two
different ways:
 - a `repeat`-combinator lets you test a property, e.g., 50 times
   rather than just 1. (Pro: a failure is found faster, Con: wasted,
   repetitive testing when there are no failures)
 - [a recent `QCheck` PR](https://github.com/c-cube/qcheck/pull/212) extends `Test.make` with a `~retries` parameter causing
   it to only perform repetition during shrinking. (Pro: each test is
   cheaper so we can run more, Con: more tests are required to trigger a race)

A functor `STM.AddGC` inserts calls to `Gc.minor()` at random points
between the executed commands.

We use two examples with known problems to help ensure that concurrency
issues are indeed found as expected (aka. sanity check). For both of
these a counter example is consistently found and shrunk:

 - [src/neg_tests/ref_test.ml](src/neg_tests/ref_test.ml) tests an unprotected global ref.
 - [src/neg_tests/conclist_test.ml](src/neg_tests/conclist_test.ml) tests a buggy concurrent list.


A Linearization Tester
======================

Writing a model and specifying how the model changes across each
command requires a bit of effort. The `Lin` module requires less from
its users. It tests instead for *linearizability*, i.e., that the
results observed during a parallel run is explainable by some
linearized, sequentially interleaved run of the same commands.

The module is phrased as a functor `Lin.Make` expecting a programmatic
description of the tested commands. The output module contains a
function `lin_test` that performs the linearizability test.
Currently `lin_test` supports two modes:
- with argument `Domain` the test is run in parallel by `spawn`ing on multiple cores
- with argument `Thread` the test is run concurrently with `Thread`
  on a single core

A recent combinator-based signature DSL in the style of [Ctypes](https://github.com/ocamllabs/ocaml-ctypes),
[ArtiCheck](https://github.com/braibant/articheck), and [Monolith](https://gitlab.inria.fr/fpottier/monolith)
lowers the required user input to little more than a signature per tested command.
See [test/lin_api.ml](test/lin_api.ml) for an example.

The modules can be used as part of the `multicorecheck.lin` library from the
[`multicorecheck` package](multicorecheck.opam).

Again we use examples with known problems to help ensure that
concurrency issues are indeed found as expected:
- [src/neg_tests/lin_tests_common.ml](src/neg_tests/lin_tests_common.ml) and
- [src/neg_tests/domain_lin_tests.ml](src/neg_tests/domain_lin_tests.ml)

contain "sanity check tests" for `ref` and `CList` over unboxed `int` and
boxed `int64` types.



Current (experimental) PBTs of multicore
========================================

Tests utilizing the parallel STM.ml capability:

 - [src/atomic/atomic_test.ml](src/atomic/atomic_test.ml) contains sequential and
   parallel tests of the `Atomic` module using STM.ml

 - [src/lazy/lazy_stm_test.ml](src/lazy/lazy_lin_test.ml) contains sequential and
   parallel tests of the `Lazy` module using STM.ml

 - [src/lockfree/ws_deque_test.ml](src/lockfree/ws_deque_test.ml) contains sequential
   and parallel tests of [ws_deque.ml](https://github.com/ocaml-multicore/lockfree/blob/main/src/ws_deque.ml)
   from [Lockfree](https://github.com/ocaml-multicore/lockfree).


Tests utilizing the linearizability tests of Lin.ml:

- [src/atomic/lin_tests.ml](src/atomic/lin_tests.ml) contains experimental `Lin`-tests of `Atomic`

- [src/hashtbl/lin_tests.ml](src/hashtbl/lin_tests.ml) contains experimental `Lin`-tests of `Hashtbl`

- [src/queue/lin_tests.ml](src/queue/lin_tests.ml) contains experimental `Lin`-tests of `Queue`

- [src/stack/lin_tests.ml](src/stack/lin_tests.ml) contains experimental `Lin`-tests of `Stack`

- [src/lazy/lazy_lin_test.ml](src/lazy/lazy_lin_test.ml) contains experimental `Lin`-tests of `Lazy`

- [src/kcas/lin_tests.ml](src/kcas/lin_tests.ml) contains experimental
  `Lin`-tests of `Kcas` and `Kcas.W1` (Note: `Kcas` is subsumed by `Stdlib.Atomic`).



Tests of the underlying spawn/async functionality of Domain and
Domainslib.Task (not using STM.ml or Lin.ml which rely on them):

 - [src/domainslib/task_one_dep.ml](src/domainslib/task_one_dep.ml) is a test of `Domainslib.Task`'s `async`/`await`.

 - [src/domainslib/task_more_deps.ml](src/domainslib/task_more_deps.ml) is a variant of the
   above allowing each promise to await on multiple others.

 - [src/domainslib/task_parallel.ml](src/domainslib/task_parallel.ml) test the three `Domainslib.Task.parallel_*` operations.

 - [src/domain/domain_joingraph.ml](src/domain/domain_joingraph.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random dependency graph

 - [src/domain/domain_spawntree.ml](src/domain/domain_spawntree.ml) is a test of `Domain`'s
   `spawn`/`join` based on a random spawn tree


Issues
======

Unsafe Buffer module
--------------------

The tests found that the `Buffer` module implementation is [unsafe under parallel usage](https://github.com/ocaml/ocaml/issues/11279) - initially described in [multicoretests#63](https://github.com/jmid/multicoretests/pull/63).


MacOS segfault
--------------

The tests found an issue causing [a segfault on MacOS](https://github.com/ocaml/ocaml/issues/11226).


Cornercase issue in `domainslib`
--------------------------------

The tests found an issue in `Domainslib.parallel_for_reduce` which
[would yield the wrong result for empty arrays](https://github.com/ocaml-multicore/domainslib/pull/67).


Specification of `ws_deque`
---------------------------

The initial tests of `ws_deque` just applied the parallelism property `agree_prop_par`.
However that is not sufficient, as only the original domain (thread)
is allowed to call `push`, `pop`, ..., while a `spawn`ed domain
should call only `steal`.

A custom, revised property test in
[src/lockfree/ws_deque_test.ml](src/lockfree/ws_deque_test.ml) runs a `cmd` prefix, then
`spawn`s a "stealer domain" with `steal`, ... calls, while the
original domain performs calls across a broder random selection
(`push`, `pop`, ...).

Here is an example output illustrating how `size` may return `-1` when
used in a "stealer domain". The first line in the `Failure` section lists
the original domain's commands and the second lists the stealer
domains commands (`Steal`,...). The second `Messages` section lists a
rough dump of the corresponding return values: `RSteal (Some 73)` is
the result of `Steal`, ... Here it is clear that the spawned domain
successfully steals 73, and then observes both a `-1` and `0` result from
`size` depending on timing. `Size` should therefore not be considered
threadsafe (none of the
[two](https://www.dre.vanderbilt.edu/~schmidt/PDF/work-stealing-dequeue.pdf)
[papers](https://hal.inria.fr/hal-00802885/document) make any such
promises though):

``` ocaml
$ dune exec src/ws_deque_test.exe
random seed: 55610855
generated error  fail  pass / total     time test name
[✗]   318     0     1   317 / 10000     2.4s parallel ws_deque test (w/repeat)

--- Failure --------------------------------------------------------------------

Test parallel ws_deque test (w/repeat) failed (8 shrink steps):

 Seq.prefix:  Parallel procs.:

          []  [(Push 73); Pop; Is_empty; Size]

              [Steal; Size; Size]


+++ Messages ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Messages for test parallel ws_deque test (w/repeat):

Result observations not explainable by linearized model:

  Seq.prefix:  Parallel procs.:

          []  [RPush; (RPop None); (RIs_empty true); (RSize 0)]

              [(RSteal (Some 73)); (RSize -1); (RSize 0)]

================================================================================
failure (1 tests failed, 0 tests errored, ran 1 tests)
```


Segfault in Domainslib
----------------------

Testing [src/domainslib/task_one_dep.ml](src/domainslib/task_one_dep.ml) with 2 work pools
found a [segfault in
domainslib](https://github.com/ocaml-multicore/domainslib/issues/58).





Dead-lock in Domainslib
-----------------------

A reported deadlock in domainslib motivated the development of these tests:
 - https://github.com/ocaml-multicore/domainslib/issues/47
 - https://github.com/ocaml-multicore/ocaml-multicore/issues/670

The tests in [src/domainslib/task_one_dep.ml](src/domainslib/task_one_dep.ml) and
[src/domainslib/task_more_deps.ml](src/domainslib/task_more_deps.ml) are run with a timeout
to prevent deadlocking indefinitely.

[src/domainslib/task_one_dep.ml](src/domainslib/task_one_dep.ml) can trigger one such
deadlock.

It exhibits no non-determistic behaviour when repeating the same
tested property from within the QCheck test.
However it fails (due to timeout) on the following test input:

```ocaml
$ dune exec -- src/task_one_dep.exe -v
random seed: 147821373
generated error fail pass / total     time test name
[✗]   25    0    1   24 /  100    36.2s Task.async/await

--- Failure --------------------------------------------------------------------

Test Task.async/await failed (2 shrink steps):

{ num_domains = 3; length = 6;
  dependencies = [|None; (Some 0); None; (Some 1); None; None|] }
================================================================================
failure (1 tests failed, 0 tests errored, ran 1 tests)
```

This corresponds to the program (available in
[issues/task_issue.ml](issues/task_issue.ml))
with 3+1 domains and 6 promises.
It loops infinitely with both bytecode/native:

```ocaml
...
open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

let pool = Task.setup_pool ~num_additional_domains:3 ()

let p0 = Task.async pool work
let p1 = Task.async pool (fun () -> work (); Task.await pool p0)
let p2 = Task.async pool work
let p3 = Task.async pool (fun () -> work (); Task.await pool p1)
let p4 = Task.async pool work
let p5 = Task.async pool work

let () = List.iter (fun p -> Task.await pool p) [p0;p1;p2;p3;p4;p5]
let () = Task.teardown_pool pool
```


Utop segfault
-------------

Utop segfaults when loading [src/domain/domain_spawntree.ml](src/domain/domain_spawntree.ml)
interactively:

``` ocaml
$ utop
──────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────
                                              │ Welcome to utop version 2.8.0 (using OCaml version 4.12.0+domains)! │                                              
                                              └─────────────────────────────────────────────────────────────────────┘                                              
Findlib has been successfully loaded. Additional directives:
  #require "package";;      to load a package
  #list;;                   to list the available packages
  #camlp4o;;                to load camlp4 (standard syntax)
  #camlp4r;;                to load camlp4 (revised syntax)
  #predicates "p,q,...";;   to set these predicates
  Topfind.reset();;         to force that packages will be reloaded
  #thread;;                 to enable threads


Type #utop_help for help about using utop.

utop # #require "ppx_deriving.show";;
utop # #require "qcheck";;
utop # #use "src/domain_spawntree.ml";;
type cmd = Incr | Decr | Spawn of cmd list
val pp_cmd : Format.formatter -> cmd -> unit = <fun>
val show_cmd : cmd -> string = <fun>
val count_spawns : cmd -> int = <fun>
val gen : int -> int -> cmd Gen.t = <fun>
val shrink_cmd : cmd Shrink.t = <fun>
val interp : int -> cmd -> int = <fun>
val dom_interp : int Atomic.t -> cmd -> unit = <fun>
val t : max_depth:int -> max_width:int -> Test.t = <fun>
random seed: 359528592
Segmentation fault (core dumped)
```

This does not happen when running a plain `ocaml` top-level though, so it
seems `utop`-specific.
