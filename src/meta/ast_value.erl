% Copyright (C) 2018-2018 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Sunday, February 4, 2018.



% Module in charge of handling values defined within an AST, including atomic literals.
%
% Refer to the "7.2 Atomic Literals" section of
% http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_value).


% "There are five kinds of atomic literals, which are represented in the same
% way in patterns, expressions, and guards:
%    If L is an atom literal, then Rep(L) = {atom,LINE,L}.
%    If L is a character literal, then Rep(L) = {char,LINE,L}.
%    If L is a float literal, then Rep(L) = {float,LINE,L}.
%    If L is an integer literal, then Rep(L) = {integer,LINE,L}.
%    If L is a string literal consisting of the characters C_1, ..., C_k, then
%    Rep(L) = {string,LINE,[C_1, ..., C_k]}.
%
% Notice that negative integer and float literals do not occur as such; they are
% parsed as an application of the unary negation operator."



% The description of an immediate value (atomic literal) in an AST, with line
% information.
%
% Ex: nil, in {nil,33} for [] at line #33.
%
%
-type ast_atomic_literal() :: { 'atom', line(), atom() }
							| { 'char', line(), char() }
							| { 'float', line(), float() }
							| { 'integer', line(), integer() }
							| { 'string', line(), text_utils:string() }.


-type ast_compound_literal() :: { 'nil', line() }.

-type ast_immediate_value() :: ast_atomic_literal() | ast_compound_literal().



-export_type([ ast_atomic_literal/0, ast_compound_literal/0, 
			   ast_immediate_value/0 ]).



% Forging AST values:
-export([ forge_boolean_value/1, forge_boolean_value/2,
		  forge_atom_value/1, forge_atom_value/2
		  %forge_pid_value/1, forge_pid_value/2,
		  %forge_integer_value/1, forge_integer_value/2,
		  %forge_float_value/1, forge_float_value/2,
		  %forge_tuple_value/1, forge_tuple_value/2,
		  %forge_list_value/1, forge_list_value/2
		]).


% Shorthands:

-type ast_element() :: ast_base:ast_element().
-type line() :: ast_base:line().



% Section for immediate value forging.


% Returns an AST-compliant value designating specified boolean, defined at line
% #0 of the current source file.
%
% Ex: forge_boolean_value( true ) returns: {boolean,0,true}.
%
-spec forge_boolean_value( boolean() ) -> ast_element().
forge_boolean_value( BooleanValue ) ->
	forge_boolean_value( BooleanValue, _Line=0 ).


% Returns an AST-compliant value designating specified boolean, defined at
% specified line of the current source file.
%
% Ex: forge_boolean_value( false, 43 ) returns: {boolean,43,false}.
%
-spec forge_boolean_value( boolean(), line() ) -> ast_element().
forge_boolean_value( BooleanValue, Line ) ->
	{ boolean, Line, BooleanValue }.



% Returns an AST-compliant value designating specified atom, defined at line #0
% of the current source file.
%
% Ex: forge_atom_value( basic_utils ) returns: {atom,0,basic_utils}.
%
-spec forge_atom_value( atom() ) -> ast_element().
forge_atom_value( AtomValue ) ->
	forge_atom_value( AtomValue, _Line=0 ).


% Returns an AST-compliant value designating specified atom, defined at
% specified line of the current source file.
%
% Ex: forge_atom_value( basic_utils, 43 ) returns: {atom,43,basic_utils}.
%
-spec forge_atom_value( atom(), line() ) -> ast_element().
forge_atom_value( AtomValue, Line ) ->
	{ atom, Line, AtomValue }.
