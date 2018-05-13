#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

% Commented out: -pa ../utils


% Copyright (C) 2016-2018 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, in merge_utils.erl, for an easier
% debugging (ex: with proper stack traces with lines).

% This script depends on the 'Myriad' layer, and only on that code (that shall
% be recompiled beforehand).



% Entry point of this escript.
%
main( ArgList ) ->
	merge_utils:main( ArgList ).
