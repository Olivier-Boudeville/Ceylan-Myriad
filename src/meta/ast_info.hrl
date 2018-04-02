% Copyright (C) 2014-2018 Olivier Boudeville
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
% Creation date: Saturday, February 3, 2018.


% This header file gathers mostly following info records:
%
% - module_info
% - type_info
% - function_info



% A record to store and centralise information gathered about an Erlang
% (compiled) module.
%
% Allows to perform checkings and to reorder and transform the returned version
% of it (for that, located ASTs and forms are used).
%
% We store the located forms verbatim whenever possible (in *_def* counterpart
% fields), notably to preserve the line numbers within.
%
% As a consequence there are generally two fields per theme:
%
% - the high-level, developer-friendly one (ex: 'module')
% - the raw one in AST form (ex: 'module_def')
%
% It is up to the user to ensure that, if either field is modified, its
% counterpart one is updated accordingly.
%
-record( module_info, {


		% Name of that module:
		module = undefined :: basic_utils:module_name(),


		% Module definition:
		module_def = undefined :: basic_utils:maybe( ast_info:located_form() ),


		% A table, whose keys are compilation options (ex: no_auto_import,
		% inline, etc.) and whose values are aggregated lists of their
		% associated values (ex: [{size,1}] and [{get_bucket_index,2},{f/1}]).
		%
		% Note: for the 'inline' key, if full inlining is enabled ( '-compile(
		% inline ).'), then its associated key is not a list of function
		% identifiers, but 'all'.
		%
		compilation_options :: ast_info:compile_option_table(),


		% A table, as multiple compile attributes can be declared, like:
		%
		% Ex: {attribute,67,compile,{no_auto_import,[{size,1}]}},
		%     {attribute,63,compile,{inline,[{get_bucket_index,2}]}}
		%
		compilation_option_defs = [] :: [ ast_info:located_form() ],


		% Parse-level attributes (ex: '-my_attribute( my_value ).'), as a table
		% associating values to attribute names (its keys).
		%
		% Such attributes, also named "wild attributes", mostly correspond to
		% user-defined ones.
		%
		% Note: must be kept on sync with the 'parse_attribute_defs' field.
		%
		parse_attributes :: ast_info:attribute_table(),


		% Parse attribute definitions (as located, abstract forms):
		%
		% Note: mostly for user-defined attributes, knowing most if not all
		% others are to be collected in specific other fields of this record.
		%
		parse_attribute_defs = [] :: [ ast_info:located_form() ],


		% As remote function specifications can be defined, like:
		% -spec Mod:Name(...) -> ...
		%
		remote_spec_defs = [] :: [ ast_info:located_form() ],


		% Include files (typically *.hrl files).
		%
		% Unlike the raw definitions (in 'include_defs'), this field does not
		% include the filename of the module being compiled (ex: "foobar.erl").
		%
		% (there is no duplicate either in that include list)
		%
		includes = [] :: [ file_utils:bin_file_path() ],


		% Include definitions:
		%
		% (possibly a given file might be included more than once; it is
		% generally the case for the module being currently compiled)
		%
		include_defs = [] :: [ ast_info:located_form() ],


		% Whether a type (possibly any kind of it; ex: opaque or not) is
		% exported is recorded primarily in its own type_info record through a
		% list of locations, while the information sufficient to reconstruct the
		% actual forms for the exports of all types are recorded here.
		%
		% Note: it is better that way, as a type export attribute may define any
		% number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% 'types' field)
		%
		type_exports :: ast_info:type_export_table(),


		% All information, indexed by type identifiers, about all the types
		% defined in that module:
		%
		types :: ast_info:type_table(),


		% All information (notably: field descriptions), indexed by record
		% names, about all the records known of that module:
		%
		records :: ast_info:record_table(),


		% Lists the functions imported by that module, per-module.
		%
		function_imports :: ast_info:function_import_table(),


		% The definitions of the function imports:
		%
		function_imports_defs = [] :: [ ast_info:located_form() ],



		% Whether a function (possibly any kind of it) is exported is recorded
		% primarily in its own function_info record through a list of locations,
		% while the information sufficient to reconstruct the actual forms for
		% the exports of all functions are recorded here.
		%
		% Note: it is better that way, as a function export attribute may define
		% any number of exports, and we need to record its definition line.
		%
		% (this field must be kept synchronised with the table in the
		% 'functions' field)
		%
		function_exports :: ast_info:function_export_table(),


		% All information, indexed by function identifiers, about all the
		% functions defined in that module:
		%
		functions :: ast_info:function_table(),


		% The definitions of the list of optional callbacks:
		%
		optional_callbacks_defs = [] :: [ ast_info:located_form() ],


		% The definition of the last line in the original source file:
		%
		% (we keep it as a located form rather than a simple meta_utils:line()
		% to avoid a costly addition in last position)
		%
		last_line :: ast_info:located_form(),


		% List of all the located forms that are unhandled, which are typically
		% errors, like:
		%
		% '{error,{LineNumber,erl_parse, ["syntax error before: ","')'"]}}''.
		%
		unhandled_forms = [] :: [ ast_info:located_form() ]


} ).




% Describes a type (generally extracted from a module).
%
-record( type_info, {


		   % The name of that type:
		   name = undefined :: meta_utils:type_name(),


		   % The (ordered) list of variable definitions (ex: [ { var, Line, 'X'
		   % } ]) of this type:
		   %
		   variables = [] :: [ ast_utils:ast_variable() ],


		   % Tells whether this type is defined as opaque:
		   opaque = undefined :: boolean(),


		   % Corresponds to the location of the full form for the definition of
		   % this type:
		   %
		   location = undefined :: basic_utils:maybe( ast_info:location() ),


		   % Corresponds to the line where this type is defined (in its source
		   % file):
		   %
		   line = undefined :: basic_utils:maybe( ast_base:line() ),


		   % Type actual definition, a (non-located) abstract form:
		   %
		   definition = undefined :: ast_type:ast_type(),


		   % Tells whether this type has been exported, as a (possibly
		   % empty) list of the location(s) of its actual export(s), knowing
		   % that a type can be exported more than once or never:
		   %
		   exported = [] :: [ ast_info:location() ]

} ).




% Describes a function (generally extracted from a module).
%
-record( function_info, {


		   % The name of that function:
		   name = undefined :: meta_utils:function_name(),


		   % The arity of that function:
		   arity = undefined :: arity(),


		   % Corresponds to the location of the full form for the definition
		   % (first clause) of this function (not of the spec):
		   %
		   location = undefined :: basic_utils:maybe( ast_info:location() ),


		   % Corresponds to the line of the first defined clause (in its source
		   % file):
		   %
		   % (this information is a priori redundant with the one in the first
		   % clause, yet present in the forms, thus kept here)
		   %
		   line = undefined :: basic_utils:maybe( ast_base:line() ),


		   % Function actual definition, a (non-located) list of the abstract
		   % forms of its clauses:
		   %
		   clauses = [] :: [ meta_utils:clause_def() ],


		   % The type specification (if any) of that function, as an abstract
		   % form:
		   spec = undefined ::
			 basic_utils:maybe( ast_info:located_function_spec() ),


		   % Tells whether the function has a mere specification, or if it is a
		   % callback:
		   %
		   callback = undefined :: basic_utils:maybe( boolean() ),


		   % Tells whether this function has been exported, as a (possibly
		   % empty) list of the location(s) of its actual export(s), knowing
		   % that a function can be exported more than once or never:
		   %
		   exported = [] :: [ ast_info:location() ]


} ).
