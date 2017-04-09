% Copyright (C) 2014-2017 Olivier Boudeville
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
% Creation date: Tuesday, December 30, 2014




% A record to store and centralise information gathered about an Erlang
% (compiled) module.
%
% Allows to perform checkings and to reorder and transform the returned version
% of it (for that, located ASTs and forms are used).
%
% We store the located forms verbatim whenever possible (in *_def* counterpart
% fields), notably to preserve line numbers.
%
-record( module_info, {


		% Name of that module:
		module = undefined :: basic_utils:module_name(),


		% Module definition:
		module_def = undefined :: 'undefined' | meta_utils:located_form(),


		% Ex: '{compile, { inline, [ { FunName, Arity } ] } }'):
		%
		compilation_option_defs = [] :: meta_utils:located_ast(),


		% Parse-level attributes (ex: '-my_attribute( my_value ).'), as
		% attribute name/value pairs (ex: { my_attribute, my_value } ).
		%
		parse_attributes = [] :: [ meta_utils:attribute() ],


		% Parse attribute definitions (as located, abstract forms):
		%
		parse_attribute_defs = [] :: meta_utils:located_ast(),


		% Include files (typically *.hrl files).
		%
		% Unlike the raw definitions (in 'include_defs'), this file does not
		% include the filename of the module being compiled (ex: "foobar.erl").
		%
		% (there is no duplicate either in that include list)
		%
		includes = [] :: [ file_utils:file_name() ],


		% Include definitions:
		%
		% (possibly a given file might be included more than once; the module
		% being currently compiled is generally listed here more than once)
		%
		include_defs = [] :: meta_utils:located_ast(),


		% Type definitions:
		%
		% (few information gathered)
		%
		type_definitions = [] :: [ { meta_utils:type_name(),
									 meta_utils:type_arity() } ],


		% The abstract forms corresponding to type definitions:
		%
		type_definition_defs = [] :: meta_utils:located_ast(),


		% All type exports:
		type_exports = [] :: [ { meta_utils:type_name(),
								 meta_utils:type_arity() } ],


		% The type export definitions:
		type_export_defs = [] :: meta_utils:located_ast(),


		% Whether a function (possibly any kind of method) is exported is
		% recorded primarily in its respective function_info record through a
		% boolean, while the forms for the exports of all functions (including
		% methods) are recorded here (better that way, as an export attribute
		% may define any number of exports and we want to record its line):
		%
		function_exports = [] :: meta_utils:located_ast(),


		% All information about the functions defined in that module:
		%
		% (we cannot use the 'table' module here: this meta module is not
		% parse-transformed; we thus use only map_hashtable here, knowing that
		% this module is bootstrapped as well)
		%
		%functions :: map_hashtable:map_hashtable( meta_utils:function_id(),
		%										   meta_utils:function_info() ),
		functions :: map_hashtable:map_hashtable(),


		% The definition of the last line in the original source file:
		%
		% (we keep it as a located form rather than a simple meta_utils:line()
		% to avoid a costly addition in last position)
		%
		last_line :: meta_utils:located_form(),


		% List of all the located forms that are unhandled, which are typically
		% errors, like:
		%
		% '{error,{LineNumber,erl_parse, ["syntax error before: ","')'"]}}''.
		%
		unhandled_forms = [] :: meta_utils:located_ast()


} ).



% Describes a function (generally extracted from a module).
%
-record( function_info, {

		   % The name of that function:
		   name = undefined :: meta_utils:function_name(),

		   % The arity of that function:
		   arity = undefined :: arity(),

		   % Corresponds to the location of the full form for the definition of
		   % this function (not of the spec):
		   %
		   location = undefined :: 'undefined' | meta_utils:location(),

		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is a priori redundant with the one in the first
		   % clause, yet present in the forms, thus kept here)
		   %
		   line = undefined :: 'undefined' | meta_utils:line(),

		   % Function actual definition, a list of the abstract forms of its
		   % definition:
		   %
		   definition = [] :: [ meta_utils:clause_def() ],

		   % The type specification (if any) of that function, as an abstract
		   % form:
		   spec = undefined :: meta_utils:located_function_spec() | 'undefined',

		   % Tells whether this function has been exported:
		   exported = false :: boolean()

} ).
