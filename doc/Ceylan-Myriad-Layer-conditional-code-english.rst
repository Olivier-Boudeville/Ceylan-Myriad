
:raw-latex:`\pagebreak`

.. _'code injection':


Support for Code Injection
==========================

It may be useful to decide, at compile-time, whether some code should be added / removed / transformed / generated based on tokens defined by the user through compilation flags (typically specified in makefiles).



Defining a token
----------------

A *token* (a symbol) may or may not defined.

To define ``my_token``, simply ensure that the a ``-Dmy_token`` command-line option is specified to the compiler (ex: refer to ``ERLANG_COMPILER_TOKEN_OPT``, in ``GNUmakevars.inc``).

To define ``my_token`` and set it to the integer value ``127``, use the ``-Dmy_token=127`` command-line option. Values can also be floats (ex: ``-Dmy_token=3.14``) or atoms (ex: ``-Dmy_token=some_atom``).

A special token is ``debug_mode``; if it is defined at all (and possibly associated to any value), the debug mode of Myriad is enabled.



Using tokens to enable code injection
-------------------------------------

Various primitives for *code injection* are available in the ``cond_utils`` (mostly pseudo-) module.

``if_debug/1``, for example used as:

.. code:: erlang

 cond_utils:if_debug([A=B+1,io:format("Hello ~p",[A])])

will enable the specified code (either an arbitrarily nested expression or a list thereof) iff (if and only if) the ``debug_mode`` token has been defined.

Similarly, ``if_defined/2``, for example used as:

.. code:: erlang

 cond_utils:if_defined(my_token,[EXPR1,EXPR2,...])

will inject ``EXPR1``, ``EXPR2``, etc. if ``my_token`` has been defined (any value associated to this token value will be ignored), otherwise the ``if_defined/2`` call will be removed as a whole [#]_.

.. [#] So ``if_debug([...])`` behaves exactly as: ``if_defined(debug_mode,[...])``.


As for ``if_defined/3``, it supports two lists of expressions:

.. code:: erlang

 cond_utils:if_defined(a_token,FIRST_EXPR_LIST,SECOND_EXPR_LIST])


If ``a_token`` has been defined, the first list will be injected, otherwise the second will be.

Finally, with ``if_set_to/{3,4}``, the injection will depend not only of a token being defined or not, but also onto the value (if any) to which it is associated.

An example with ``if_set_to/3``:

.. code:: erlang

 cond_utils:if_set_to(some_token,42,EXPR_LIST)

will inject ``EXPR_LIST`` iff ``some_token`` has been defined and set to ``42`` (i.e. ``-Dsome_token=42``). As a result, the specified expressions will not be injected if ``some_token`` has been set to another value, or not been defined at all. As usual, instead of a list of expressions, a single expression may be specified.

As for ``if_set_to/4``, in:

.. code:: erlang

 cond_utils:if_set_to(a_token,a_symbol,FIRST_EXPR_LIST,SECOND_EXPR_LIST)

``FIRST_EXPR_LIST`` will be injected iff ``a_token`` has been defined and set to ``a_symbol``, otherwise ``SECOND_EXPR_LIST`` will be.


Finally, the ``switch_set_to/{2,3}`` primitives allow to generalise these ``if``-like constructs, with any number of code branches (expression or list of expressions to be injected) selected based on the build-time value of a token, possibly with defaults (should the token not be defined at all, or defined to a value that is not among the ones associated to a code branch).

For example:

.. code:: erlang

  cond_utils:switch_set_to(my_token, [
	   {my_first_value, io:format("Hello!")},
	   {my_second_value, [f(), g(debug), h()]},
	   {some_third_value, a()}])

A compilation-time error will be raised if ``my_token`` is not set, or set to none of the declared values (i.e. not in ``[my_first_value, my_second_value, some_third_value]``).


A variation of this primitive exists that provides a default token value, if none was, or if it was defined to a value that is not listed among the ones designating a code branch.

For example:

.. code:: erlang

  Value = cond_utils:switch_set_to(some_token,
		[{1, foo },
		 {14, bar},
		 {20, hello}],
		14)


Here, if ``some_token`` is not defined, or defined to a value that is neither ``1``, ``14`` or ``20``, then the ``14`` default value applies, and thus ``Value`` is set to ``bar``.


Refer to ``cond_utils_test.erl`` for further usage examples.




Controlling assertions
----------------------

It may be convenient that, depending on a compile-time token (ex: in debug mode, typically triggered thanks to the ``-Ddebug_mode`` compilation option), *assertions* (expressions expected to evaluate to the atom ``true``) are enabled, whereas they shall be dismissed as a whole should that token not be defined.

To define an assertion enabled in debug mode, use ``assert/1``, like in:

.. code:: erlang

 cond_utils:assert(foo(A,B)=:=10)

Should at runtime the expression specified to ``assert/1`` be evaluated to a value ``V`` that is different from the atom ``true``, a ``{assertion_failed,V}`` exception will be thrown.

More generally, an assertion may be enabled by any token (not ``debug_mode`` only) being defined, like in:

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

Note that if the ``if_*`` conditions (ex: ``if_debug/1``) are not fulfilled, the specified conditional code is dismissed as a whole, it is not even replaced for example by an ``ok`` atom; this may matter if this conditional is the only expression in a case clause for example, in which case a compilation failure like "*internal error in core; crash reason: function_clause in function v3_core:cexprs/3 called as v3_core:cexprs[...]*" will be reported.

Note also that switching conditional flags will select/deselect in-code expressions at compile time, and may lead functions and/or variables to become unused, and thus may trigger warnings [#]_.

.. [#] Warnings that we prefer promoting to errors, as they constitute a *very* convenient safety net.

For functions that could become unused to the conditional setting of a token, the compiler could certainly be silenced by exporting them, yet a better approach is surely to prefer using::

 -compile({nowarn_unused_function,my_func/3}).

or::

 -compile({nowarn_unused_function,[my_func/3, my_other_func/0]}).


As for variables, should A, B or C be reported as unused if ``some_token`` was not set, then ``basic_utils:ignore_unused/1`` (mostly a no-op) could be of use::

 [...]
 cond_utils:if_defined( some_token,
						f(A, B, C),
						basic_utils:ignore_unused([A, B, C])),
 [...]


Alternatively, ``nowarn_unused_vars`` could be used instead, at least in some modules.



For more information
--------------------

Refer for usage and stubs to the ``cond_utils`` module (defined in ``myriad/src/meta``), knowing that it is actually implemented thanks to the Myriad parse transform.

For examples and testing, see the ``cond_utils_test`` module, available at the same location.
