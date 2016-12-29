#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

% Commented out: -pa ../utils


% Copyright (C) 2016-2017 Olivier Boudeville (olivier.boudeville@esperide.com)

% Released as LGPL software.

% Directly using the module-based version now, in merge_utils.erl, for an easier
% debugging (ex: with proper stack traces with lines).

% This script depends on the 'Common' layer (a.k.a. Ceylan-Myriad), and only on
% that code.



% Entry point of this escript.
%
main( ArgList ) ->
	merge_utils:main( ArgList ).
