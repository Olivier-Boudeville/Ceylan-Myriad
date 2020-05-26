:raw-latex:`\pagebreak`


``Myriad`` Main Conventions
===========================


Execution Targets
-----------------

Two execution target modes have been defined:

- ``development`` (the default): meant to simplify the task of developers and maintainers by reporting as much information and context as possible, even at the expense of some performances and reliability (ex: no retry in case of failure, shorter time-outs not to wait too long in case of errors, more checks, etc.)
- ``production``: mostly the reciprocal of the ``development`` mode, whose purpose is to favor efficient, bullet-proof operations

These execution targets are *compile-time* modes, i.e. they are set once for all when building the layer at hand (probably based, if using OTP, on the rebar corresponding modes - respectively ``dev`` and ``prod``).

See ``EXECUTION_TARGET`` in ``GNUmakevars.inc`` to read and/or set them.

The current execution target is of course available at runtime on a per-layer level, see ``basic_utils:get_execution_target/0`` for more information.

This function shall be compiled once per layer to be accurate, in one of its modules. It is just a matter of adding the following include in such module::

 -include_lib("myriad/utils/basic_utils.hrl").


Other Conventions
-----------------

- for clarity, we tend to use longer variable names, in CamelCase
- we tend to use mute variables to clarify meanings and intents, as in ``_Acc=[]`` (beware, despite being muted, any variable in scope that bears the same name will be matched)
- as there is much list-based recursion, a variable named ``T`` means ``Tail`` (as in ``[Head|Tail]``)

See also the few hints regarding contribution_.
