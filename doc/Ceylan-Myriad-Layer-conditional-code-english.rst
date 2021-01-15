
:raw-latex:`\pagebreak`

.. _'code injection':


Support for Code Injection
==========================

It may be useful to decide, at compile-time, whether some code should be added / removed / transformed / generated based on **tokens** defined by the user.

This is done here thanks to the use of **conditional primitives** and associated **compilation defines** (sometimes `designated as "macros" <https://erlang.org/doc/man/erlc.html#generally-useful-flags>`_, and typically specified in makefiles, with the ``-D`` flag).

These conditional primitives are gathered in the ``cond_utils`` module.

As an early example, so that a piece of code prints ``Hello!`` on the console when executed iff (*if and only if*) the ``my_token`` compilation token has been defined (through the ``-Dmy_token`` command-line flag), one may use:

.. code:: erlang

 cond_utils:if_defined(my_token, io:format("Hello!"))


Of course, as such a code injection is done at compilation-time, should compilation defines be modified the modules making use of the corresponding primitives shall be recompiled so that these changes are taken into account.

Let's enter a bit more in the details now.



Defining a token
----------------

A *token* (a compilation-time symbol) may or may not defined.

To define ``my_token``, simply ensure that a ``-Dmy_token`` command-line option is specified to the compiler (ex: refer to ``ERLANG_COMPILER_TOKEN_OPT``, in ``GNUmakevars.inc``, for an example of definition for these flags).

To define ``my_token`` *and* set it to the integer value ``127``, use the ``-Dmy_token=127`` command-line option. Values can also be floats (ex: ``-Dmy_token=3.14``) or atoms (ex: ``-Dmy_token=some_atom``).

A special token is ``myriad_debug_mode``; if it is defined at all (and possibly associated to any value), the debug mode of Myriad is enabled.

We recommend that layers built on top of Myriad define their own token for debug mode (ex: ``foobar_debug_mode``), to be able to finely select appropriate debug modes (of course all kinds of modes and configuration settings can be considered as well).



Defining the code to inject
---------------------------

Based on the defined tokens, code may be injected; this code can be any Erlang expression, and the value to which it will evaluate can be used as any other value in the program.

Injecting a *single* expression (i.e. not multiple ones) is not a limitation: not only this only expression can be a function call (thus corresponding to arbitrarily many expressions), but more significantly a series of expressions can be nested in a ``begin`` / ``end`` block, making them a single expression [#]_.


.. [#] A previous implementation of ``cond_utils`` allowed to specify the code to inject either as an expression or as a *list* of expressions. It was actually a mistake, as a single expression to return can be itself a list (ex: ``["red", "blue"]``), which bears a different semantics and should not be interpreted as a list of expressions to evaluate. For example, the result from the code to inject may be bound to a variable, in which case we expect ``A=["red", "blue"]`` rather than ``A="red", "blue"`` (this latter term being constructed but not used).



Using tokens to enable code injection
-------------------------------------

Various primitives for *code injection* are available in the ``cond_utils`` (mostly pseudo-) module [#]_.

.. [#] Their actual implementation lies in `Myriad's parse transform <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/src/meta/myriad_parse_transform.erl>`_.

There is first ``if_debug/1``, for example used as:

.. code:: erlang

 cond_utils:if_debug(io:format("Hello ~p!",[A]))


or, to illustrate expression blocks:

.. code:: erlang

 cond_utils:if_debug(begin
						 C=B+1,
						 io:format("Goodbye ~p",[C])
					   end)


These constructs will be replaced by the expression they specify for injection, at their location in the program, iff the ``myriad_debug_mode`` token has been defined, otherwise they will be replaced by nothing at all (hence with exactly *no* runtime penalty).

Similarly, ``if_defined/2``, used as:

.. code:: erlang

 cond_utils:if_defined(my_token,EXPR)

will inject ``EXPR`` if ``my_token`` has been defined (regardless of any value associated to this token), otherwise the ``if_defined/2`` call will be removed as a whole [#]_.

.. [#] So ``if_debug(EXPR)`` behaves exactly as: ``if_defined(myriad_debug_mode,EXPR)``.


As for ``if_defined/3``, it supports two expressions:

.. code:: erlang

 cond_utils:if_defined(a_token,FIRST_EXPR,SECOND_EXPR])


If ``a_token`` has been defined, the first expression will be injected, otherwise the second will.

Finally, with ``if_set_to/{3,4}``, the injection will depend not only of a token being defined or not, but also onto the value (if any) to which it is set.

An example with ``if_set_to/3``:

.. code:: erlang

 cond_utils:if_set_to(some_token,42,EXPR)

will inject ``EXPR`` iff ``some_token`` has been defined and set to ``42`` (i.e. ``-Dsome_token=42``). As a result, the specified expression will not be injected if ``some_token`` has been set to another value, or not been defined at all.

As for ``if_set_to/4``, in:

.. code:: erlang

 cond_utils:if_set_to(a_token,a_symbol,FIRST_EXPR,SECOND_EXPR)

``FIRST_EXPR`` will be injected iff ``a_token`` has been defined and set to ``a_symbol``, otherwise (not set or set to a different value) ``SECOND_EXPR`` will be.


Finally, the ``switch_set_to/{2,3}`` primitives allow to generalise these ``if``-like constructs, with any number of code branches selected based on the build-time value of a token, possibly with defaults (should the token not be defined at all, or defined to a value that is not among the ones associated to a code branch).

For that we specify a list of pairs, each made of a value and of the corresponding expression to be injected in the actual token matches that value; for example:

.. code:: erlang

  cond_utils:switch_set_to(my_token, [
	   {my_first_value, io:format("Hello!")},
	   {my_second_value, begin f(), g(debug), h(), end},
	   {some_third_value, a()}])

A compilation-time error will be raised if ``my_token`` is not set, or if it is set to none of the declared values (i.e. not in ``[my_first_value, my_second_value, some_third_value]``).

A variation of this primitive exists that applies a default token value if none was, or if the token was set to a value that is not listed among any of the ones designating a code branch.

For example:

.. code:: erlang

  Value = cond_utils:switch_set_to(some_token,
		[{1, foo },
		 {14, bar},
		 {20, hello}],
		14)


Here, if ``some_token`` is not defined, or defined to a value that is neither ``1``, ``14`` or ``20``, then the ``14`` default value applies, and thus ``Value`` is set to ``bar``.


Refer to `cond_utils_test.erl <https://github.com/Olivier-Boudeville/Ceylan-Myriad/blob/master/test/meta/cond_utils_test.erl>`_ for further usage examples.



Controlling assertions
----------------------

It may be convenient that, depending on a compile-time token (ex: in debug mode, typically triggered thanks to the ``-Dmyriad_debug_mode`` compilation option), *assertions* (expressions expected to evaluate to the atom ``true``) are enabled, whereas they shall be dismissed as a whole should that token not be defined.

To define an assertion enabled in debug mode, use ``assert/1``, like in:

.. code:: erlang

 cond_utils:assert(foo(A,B)=:=10)

Should at runtime the expression specified to ``assert/1`` be evaluated to a value ``V`` that is different from the atom ``true``, a ``{assertion_failed,V}`` exception will be thrown.

More generally, an assertion may be enabled by any token (not only ``myriad_debug_mode``) being defined, like in:

.. code:: erlang

 cond_utils:assert(my_token,bar(C))


Finally, an assertion may be enabled iff a token (here, ``some_token``) has been defined and set to a given value (here, ``42``), like in:

.. code:: erlang

 cond_utils:assert(some_token,42,not baz() andalso A)


This may be useful for example to control, on a per-theme basis, the level of checking performed, like in:

.. code:: erlang

 cond_utils:assert(debug_gui,1,basic_testing()),
 cond_utils:assert(debug_gui,2,more_involved_testing()),
 cond_utils:assert(debug_gui,3,paranoid_testing()),

Note that, in this case, a given level of checking should include the one just below it (ex: ``more_involved_testing()`` should call ``basic_testing()``).



Usage Hints
-----------

For tokens, at least currently they must be defined as immediate values (atoms); even using a mute variable, like for the ``_Default=my_token`` expression, is not allowed.

Note that, for primitives that may not inject code at all (ex: ``if_debug/1``), if their conditions are not fulfilled, the specified conditional code is dismissed as a whole, it is not even replaced for example by an ``ok`` atom; this may matter if this conditional is the only expression in a case clause for example, in which case a compilation failure like "*internal error in core; crash reason: function_clause in function v3_core:cexprs/3 called as v3_core:cexprs[...]*" will be reported (the compiler sees unexpectedly a clause with no expression).

A related issue may happen when switching conditional flags: it will select/deselect in-code expressions at compile time, and may lead functions and/or variables to become unused, and thus may trigger at least warnings [#]_.

.. [#] Warnings that we prefer promoting to errors, as they constitute a *very* convenient safety net.

For functions that could become unused due to the conditional setting of a token, the compiler could certainly be silenced by exporting them; yet a better approach is surely to use:

.. code:: erlang

 -compile({nowarn_unused_function,my_func/3}).

or:

.. code:: erlang

 -compile({nowarn_unused_function,[my_func/3, my_other_func/0]}).


As for variables, should A, B or C be reported as unused if ``some_token`` was not set, then ``basic_utils:ignore_unused/1`` function (mostly a no-op) could be of use:

.. code:: erlang

 [...]
 cond_utils:if_defined(some_token,
					   f(A, B, C),
					   basic_utils:ignore_unused([A, B, C])),
 [...]


Alternatively, ``nowarn_unused_vars`` could be used instead, at least in some modules.



For more information
--------------------

Refer for usage and stubs to the ``cond_utils`` module (defined in `myriad/src/meta <https://github.com/Olivier-Boudeville/Ceylan-Myriad/tree/master/src/meta>`_), knowing that it is actually implemented thanks to the Myriad parse transform.

For examples and testing, see the ``cond_utils_test`` module.
