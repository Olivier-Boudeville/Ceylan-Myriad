
:raw-latex:`\pagebreak`

.. _'code injection':


Support for Code Injection
==========================

It may be useful to decide, at compile-time, based on tokens specified on the command-line, whether some code should be enabled.


Defining a token
----------------

A *token* (a symbol) may or may not defined.

To define ``my_token``, simply ensure that the a ``-Dmy_token`` command-line option is specified to the compiler (ex: refer to  ``ERLANG_COMPILER_TOKEN_OPT`` in ``GNUmakevars.inc``).

To define ``my_token`` and set it to the integer value ``127``, use the ``-Dmy_token=127`` command-line option. Values can also be floats (ex: ``-Dmy_token=3.14``) or atoms (ex: ``-Dmy_token=some_atom``).


A special token is ``debug_mode``; if it is defined at all (and possibly associated to any value), the debug mode of Myriad is enabled.



Using tokens to enable code injection
-------------------------------------

Various primitives for *code injection* are available in the ``cond_utils`` (mostly pseudo-) module.

``if_debug/1``, for example used as::

  cond_utils:if_debug([A=B+1,io:format("Hello ~p",[A])])

will enable the specified code (a series of arbitrarily nested expressions) iff (if and only if) the ``debug_mode`` token has been defined.

Similarly, ``if_defined/2``, for example used as::

  cond_utils:if_defined(my_token,[EXPR1,EXPR2,...])

will inject ``EXPR1``, ``EXPR2``, etc. if ``my_token`` has been defined (any value associated to this token value will be ignored), otherwise the ``if_defined/2`` call will be removed as a whole [#]_.

.. [#] So ``if_debug([...])`` behaves exactly as: ``if_defined(debug_mode,[...])``.


As for ``if_defined/3``, it supports two lists of expressions::

  cond_utils:if_defined(a_token,FIRST_EXPR_LIST,SECOND_EXPR_LIST])

If ``a_token`` has been defined, the first list will be injected, otherwise the second will be.

Finally, with ``if_set_to/{3,4}``, the injection will depend not only of a token being defined or not, but also onto the value (if any) to which it is associated.

An example with ``if_set_to/3``::

  cond_utils:if_set_to(some_token,42,EXPR_LIST)

will inject ``EXPR_LIST`` iff ``some_token`` has been defined and set to ``42`` (i.e. ``-Dsome_token=42``). As a result, the specified expressions will not be injected if ``some_token`` has been set to another value, or not been defined at all.

As for ``if_set_to/4``, in::

  cond_utils:if_set_to(a_token,a_symbol,FIRST_EXPR_LIST,SECOND_EXPR_LIST)

``FIRST_EXPR_LIST`` will be injected iff ``a_token`` has been defined and set to ``a_symbol``, otherwise ``SECOND_EXPR_LIST`` will be.



Controlling assertions
----------------------

It may be convenient that, depending on a compile-time token (ex: the debug mode, typically triggered thanks to the ``-Ddebug_mode`` compilation option), *assertions* (expressions expected to evaluate to the atom ``true``) are enabled, whereas they shall be dismissed as a whole should that token not be defined.

To define an assertion enabled in debug mode, use ``assert/1``, like in::

  cond_utils:assert(foo(A,B)=:=10)

Should at runtime the expression specified to ``assert/1`` be evaluated to a value ``V`` that is different from the atom ``true``, a ``{assertion_failed,V}`` exception will be thrown.

More generally, an assertion may be enabled by another token than ``debug_mode`` being defined, like in::

  cond_utils:assert(my_token,bar(C))


Finally, an assertion may be enabled iff a token has been defined and set to a given value, like in::

 cond_utils:assert(some_token,42,not baz() andalso A)



For more information
--------------------

Refer for usage and stubs to the ``cond_utils`` module (defined in ``Ceylan-Myriad/src/meta``), knowing that it is actually implemented thanks to the Myriad parse transform.

For examples and testing, see the ``cond_utils_test`` module, available at the same location.
