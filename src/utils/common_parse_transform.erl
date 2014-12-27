% Copyright (C) 2014 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Creation date: Friday, December 19, 2014



% Overall parse transform for the common layer.
%
% See meta_utils.erl and meta_utils_test.erl.
%
-module(common_parse_transform).


-type ast() :: meta_utils:ast().



% Implementation notes:
%
% Currently, the 'common' parse transform is in charge of replacing any call to
% the pseudo-module 'table' (a module that does not exist) into a call to the
% default hashtable type we currently use (ex: we have hashtable,
% lazy_hashtable, tracked_hashtable, map_hashtable, etc.).


% The default actual implementation that 'table' will be wired to:
-define( default_hashtable_type, map_hashtable ).


-export([ parse_transform/2 ]).



% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
-spec parse_transform( ast(), list() ) -> ast().
parse_transform( AST, _Options ) ->

	io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	% We will be replacing here all calls to the 'table' pseudo-module by calls
	% to the actual module designated by the default_hashtable_type local macro.

	% This is just a matter of replacing 'table' by its counterpart in elements
	% like:
	%
	% {call,Line1,
	%             {remote,Line2,
	%                               {atom,Line3,table},
	%                               {atom,Line4,FunctionName}},
	%              ListArgs }

	% We will do the same kind of conversion for the type specifications (ex:
	% function specs, type definitions, etc.)

	%io:format( "Input AST:~n~p~n", [ AST ] ),

	OutputAST = replace_table( AST ),

	%io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),

	OutputAST.



% Replaces calls to the table pseudo-module by actual calls to
% default_hashtable_type.
%
% We preserve element order.
%
-spec replace_table( ast() ) -> ast().
replace_table( AST ) ->


	% The Ln variables designate line numbers.

	DesiredTableType = case lookup_table_select_attribute( AST ) of

						   undefined ->
							   ?default_hashtable_type;

						   TableType ->
							   TableType

	end,

	%io:format( "Replacing calls to 'table' by calls to '~s':~n",
	%		   [ DesiredTableType ] ),

	TransformFun = fun

			   ( _Term={ call, L1, { remote, L2,
									 { atom, Line3, table },
									 { atom, Line4, FunctionName } },
						 ListArgs }, UserData ) ->

				   %io:format( " - transforming table:~s call at line ~B~n",
				   %			  [ FunctionName, Line3 ] ),

				   NewTerm = { call, L1, { remote, L2,
								 { atom, Line3, ?default_hashtable_type },
								 { atom, Line4, FunctionName } },
							   ListArgs },

				   { NewTerm, UserData };


				( _Term={ remote_type, L1, [
								  { atom, L2, table},
								  { atom, L3, table }, [] ] }, UserData ) ->

					%io:format( " - transforming table spec at line ~B~n",
					%		  [ L1 ] ),

					NewTerm = { remote_type, L1, [
								  { atom, L2, DesiredTableType },
								  { atom, L3, DesiredTableType }, [] ] },

					{ NewTerm, UserData };


			   ( Term, UserData ) ->
				   { Term, UserData }

	end,

	{ NewAST, _NewUserData } = meta_utils:traverse_term(
								 _TargetTer=AST,
								 _TypeDescription=tuple,
								 _TermTransformer=TransformFun,
								 _UserData=undefined ),

	NewAST.



% Returns any module-level explicit replacement for the table pseudo-type,
% specified thanks to a '-table_type( my_type ).' attribute, with for example
% '-table_type( list_hashtable ).
%
% Only searches through top-level attributes as intended, and checks that the
% table type is defined up to once only.
%
lookup_table_select_attribute( AST ) ->
	lookup_table_select_attribute( AST, _Found=undefined ).


lookup_table_select_attribute( _AST=[], Found ) ->
	Found;

lookup_table_select_attribute(
  _AST=[ { attribute, _L, table_type, Type } | T ],
  _Found=undefined ) ->
	lookup_table_select_attribute( T, Type );

% Here Found has already been set:
lookup_table_select_attribute(
  _AST=[ { attribute, L, table_type, AType } | _T ], Found ) ->
	meta_utils:raise_error( { table_type_defined_more_than_once, { line, L },
				   Found, AType } );

lookup_table_select_attribute( _AST=[ _H | T ], Found ) ->
	lookup_table_select_attribute( T, Found ).
