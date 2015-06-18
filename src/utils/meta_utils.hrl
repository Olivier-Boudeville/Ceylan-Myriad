% Copyright (C) 2014-2015 Olivier Boudeville
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




% A record to store and centralise information gathered about a module.
%
% Allows to perform checkings and to reorder and transform the returned version
% of it.
%
% We preserve the forms whenever possible (in *_def* counterpart fields),
% notably to keep line numbers.
%
-record( module_info, {


		% Name of that module:
		module = undefined :: basic_utils:module_name(),


		% Module definition:
		module_def = undefined :: meta_utils:ast(),


		% Ex: '{compile, { inline, [ { FunName, Arity } ] } }'):
		%
		compilation_option_defs = [] :: [ meta_utils:ast() ],


		% Parse-level attributes (ex: '-my_attribute( my_value ).'):
		%
		parse_attributes = [] :: [ meta_utils:attribute() ],


		% Parse attribute definitions (as abstract forms):
		%
		parse_attribute_defs = [] :: [ meta_utils:ast() ],


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
		include_defs = [] :: [ meta_utils:ast() ],


		% Type definitions:
		%
		% (few information gathered)
		%
		type_definitions = [] :: [ { meta_utils:type_name(),
									 meta_utils:type_arity() } ],


		% The abstract forms corresponding to type definitions:
		%
		type_definition_defs = [] :: [ meta_utils:ast() ],


		% All type exports:
		type_exports = [] :: [ { meta_utils:type_name(),
								 meta_utils:type_arity() } ],


		% The type export definitions:
		type_export_defs = [] :: [ meta_utils:ast() ],


		% Whether a function (possibly any kind of method) is exported is
		% recorded primarily in its respective function_info record through a
		% boolean, while the forms for the exports of all functions (including
		% methods) are recorded here (better that way, as an export attribute
		% may define any number of exports and we want to record its line):
		%
		function_exports = [] :: [ meta_utils:ast() ],


		% All information about the functions defined in that module:
		%
		% (we cannot use table here: this meta module is not parse-transformed;
		% we thus use only map_hashtable here, knowing that this module is
		% bootstrapped as well)
		%
		functions :: map_hashtable:map_hashtable( meta_utils:function_id(),
												  meta_utils:function_info() ),


		% The number of the last line in the original source file:
		%
		last_line :: basic_utils:count()


} ).



% Describes a function (generally extracted from a module).
%
-record( function_info, {

		   % The name of that function:
		   name = undefined :: meta_utils:function_name(),

		   % The arity of that function:
		   arity = undefined :: arity(),

		   % Function actual definition, a list of the abstract form of its
		   % definition:
		   %
		   definition = [] :: [ meta_utils:clause_def() ],

		   % The type definition (if any) of that function, as an abstract form:
		   spec = undefined :: meta_utils:function_spec() | 'undefined',

		   % Tells whether this function has been exported:
		   exported = false :: boolean()

} ).
