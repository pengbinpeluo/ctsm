.. sectnum::

.. contents::

============================
 Overview and general notes
============================

This document describes some key design aspects of the code under src/agsys.

An important driver of the AgSys design was the desire to have the core science code be
model-independent, so that the same code could be used in CTSM, Noah-MP, etc. Code in the
``science`` directory does not depend at all on CTSM; it only depends on other code in
``science`` and code in the ``ctsm_wrappers`` directory. The wrappers would be replaced by
different wrappers if you plugged this into a different model.

Partly to avoid dependence on CTSM-specific data structures and concepts, code in
``science`` operates on single points. Normally we try to avoid single-point routines for
performance reasons, but in this case, much of what's done just happens once per day, not
every time step, so this seemed less important. And given the heavily-conditional nature
of the code, it wouldn't be vectorizable even if it were written as loops.

===================================================================
 Fundamental data structures: single-point or structures of arrays
===================================================================

We considered whether to have the fundamental data structures be single-point data
structures or structures of arrays as is done elsewhere throughout CTSM. We went with the
latter because we need these structures of arrays to use CTSM infrastructure for
diagnostics (history), restart, accumulators, etc. We could have used arrays of structures
as our fundamental data, which would have simplified the argument lists in the science
code, but this would have required copy-outs of all data in the interface layer for the
sake of diagnostics and restarts, which felt messier and more error-prone.

Note, though, that we *do* copy various input arguments into a single-point structure
before calling the AgSys routines, in order to simplify argument lists, and to allow a
consistent interface between different crops' implementations of a polymorphic subroutine
(without resorting to passing the superset of all inputs needed by any crop's
implementation).

There are arguments for and against storing AgSys's state variables in a similar
single-ponit structure, and doing a copy-out (into CTSM's patch-level arrays) at the end
of the AgSys calls for the sake of diagnostics and restart. An advantage is that it would
simplify the interface to (and possibly within) the science code, and it would make the
science code more usable outside the interface layer (since it would have its own state
variable structures). It also feels more right to have the internal agsys code define and
use its own state variables, rather than having these state variables defined and
individually passed. However, we wouldn't really be able to hide the variables needed
internally in AgSys, since we'd still need duplicates of all of those variables in the
interface layer. And in some ways I like passing output variables one-by-one, so you can
clearly see what's being set by each routine. Also, this would mean having each variable
stored persistently in two places, which is a bit confusing, not to mention wasteful of
memory. So, given the pros and cons in both directions, and the fact that we started with
the implementation of having each inout variable passed individually (so it would take
more work to change it), for now we're sticking with having each inout variable passed
individually and stored fundamentally in a structure of arrays.
