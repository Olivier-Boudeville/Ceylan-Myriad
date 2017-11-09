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
% Creation date: Friday, December 19, 2014




% Gathering of various convenient meta-related facilities, notably regarding
% metaprogramming, types and parse transforms.
%
% See meta_utils_test.erl for the corresponding test.
%
% Note that this module is a prerequisite of at least most of our parse
% transforms, hence it must be bootstrapped *before* they are built, and cannot
% use them.
%
% So, to compile it, just go to the root of this layer and execute for example
% 'make all'.
%
-module(meta_utils).




% Design notes about types.


% Types may be defined according to three forms, from the most human-focused to
% the most computer-native one:
%
% F1. type-as-a-string, i.e. a textual specification possibly entered from a
% user interface; for example, a type "my_type" may be specified as:
% "foo|bar|[integer]"
%
% F2. type-as-a-contextual-term, i.e. an Erlang term that defines a type, yet
% may still be contextual (i.e. it may depend on other non-builtin types); the
% same example may then be defined as: { union, [ foo, bar, {list,[integer]} ]
% }, where foo and bar are expected to be defined in the context
%
% F3. explicit-type, i.e. a fully explicit, self-standing term defining a type
% (therefore relying only on built-in types and constructs); for example,
% supposing that the type foo is an alias for float, and that the type bar is
% specified as "'hello'|'goodbye'", the same example translates to the following
% explicit type: { union, [ float, {union,[ {atom,hello}, {atom,goodbye} ]},
% {list,[integer]} ] }

% Going from:
%  - form F1 to form F2 is named type parsing
%  - form F2 to form F3 is named type resolution


% On type names and signatures.

% A type T (whether built-in or user-defined) is designated directly by its name
% T, as an atom. Ex: written as "count", refered to as: count.

% There are reserved type-related names (atoms), which correspond to:
%  - built-in types: atom, integer, float, boolean, string, any, none
%  - type constructs: list, union, tuple, table


% A type signature is made from the type name and from a list of the type names
% (if any) it depends upon.

% For monomorphic types (i.e. types that are not parametrised by other types),
% their signature is their sole name. Ex: "foo" ("foo()" is also accepted).

% The signature of polymorphic types (i.e. types that are parametrised by other
% types) is made of their name immediately followed by a list of the names of
% the types they depend upon, enclosed in parentheses.
%
% For example, a polymorphic type T that depends on types T1, T2, ..., Tk may
% have for signature "T( T1, T2, ..., Tk )".


% Let D( type_signature() ) -> type() be a pseudo-function returning the
% explicit type definition (as a term) of a type (designated by its signature).




% On built-in types.


% The type 'atom' designates the set of (possibly user-defined) symbols (ex:
% 'true' or 'foo'). In a type definition, such a symbol consists on the atom
% itself, and is always written enclosed in single quotes ("'foo'"), in order to
% distinguish it from the user-defined types (as one may define a type named
% foo). So 'foo' can be considered here both as a type name and a value.

% The type 'integer' designates an integer value. A value of that type is for
% example 4.

% The type 'float' designates a floating-point value. A value of that type is
% for example 3.14.

% The type 'boolean' designates a truth value, either 'true' or 'false'.

% The type 'string' designates a string of characters (a text). A value of that
% type is for example "Yellow submarine".

% The type 'any' designates a value of any type (hence all values may be seen as
% being of the 'any' type). Of course the actual, most precise type shall be
% preferred wherever possible; this type is defined mostly for formal reasons
% (completeness of the language of types)

% The type 'none' designates a value not having a type, which cannot happen
% operationally (defined also on formal grounds, for completeness).

% Finally, for a built-in type T (designated as a whole - as opposed to defining
% immediate values of it, as discussed in next section), D(T) = T. For example,
% D(atom) = atom, or D(my_type) = my_type.



% On immediate values of a given type.

% We need to be able to specify immediate values even at a type level, as we
% might want to define a type as a set of possible values (such as: [2,3,5,7,11]
% or [ 'orange', 'blue', 'red' ]).

% Let T1 be a type defined from an immediate value V of a type that is named T2
% (hence T1 is a type comprising a single value); T1 is specified as "V"
% (knowing that T2 can be inferred from V), and D(T1) = { T2, V }.
%
% So, for example:
%
% - let A be a type corresponding to an immediate value of type atom; D(A) = {
% atom, A }; for example, D(foo) = { atom, 'foo' }
%
% - let I be a type corresponding to an immediate value of type integer; D(I) =
% { integer, I }; for example, D(4) = { integer, 4 }
%
% - let F be a type corresponding to an immediate value of type float; D(F) = {
% float, F }; for example, D(3.14) = { float, 3.14 }
%
% - let S be a type corresponding to an immediate value of type string; D(S) = {
% string, S }; for example, D("Yellow submarine") = { string, "Yellow submarine"
% }




% On type constructs.
%
% The supported type constructs are:
%  - list
%  - union
%  - tuple
%  - table
%
% Note: they can also be seen as built-in polymorphic types.


% On lists:
%
% Let L be a type corresponding to an (homogeneous, ordered) list (variable-size
% container) whose all elements are of type T.
%
% L is written "[T]" and defined as D([T]) = { list, D(T) }.
%
% For example, if my_integer_list_type is defined as "[integer]", then
% D(my_integer_list_type) = D([integer]) = { list, integer }
%
% A value of that type may be [] or [ 4, 9, 147, 5, 9 ].


% On unions:
%
% Let U be a type corresponding to the union of a set of types T1, T2, Tk; a
% value of type U is thus of at least one of the types of that union.
%
% U is written as "T1|T2|...|Tk" and defined as D(U) = { union,
% [D(T1),D(T2),...,D(Tk)] }.
%
% For example, if my_type is defined as "foo|'kazoo'|[integer]", then D(my_type)
% = { union, [ foo, {atom,'kazoo'}, {list,integer} ] }.
%
% Values of that types may be 'kazoo', [3,3] of any value of type foo (whatever
% it may be).
%
% One can note that the foo type can also be replaced by its actual definition
% in order to fully resolve my_type (i.e. to go from form F2 to form F3)
%
% We can see here that the boolean type is nothing but the 'true'|'false' union
% and is not in an irreducible form (yet it is still considered as being fully
% explicit).


% On tuples:
%
% Let T be a type corresponding to a fixed-size, ordered container whose
% elements are respectively of type T1, T2, Tk.
%
% D(T) = { tuple, [D(T1),D(T2),...,D(Tk)] }.
%
% For example, if my_tuple_type is defined as "{integer,boolean|float,[atom]}"
% then D(my_tuple_type)= {list,[integer,{union,[boolean,float]},{list,atom}]}.
%
% Values of that type may be {1,true,[]} or {42,8.9,[joe,dalton]}.


% On (associative) tables:
%
% Let T be an associative table whose keys are of type Tk and values are of type
% Tv.
%
% D(T) = { table, [D(Tk),D(Tv)] }.
%
% For example, if my_table_type is defined as "table(integer,string)" then
% D(my_table_type)= {table,[integer,string]}.
%
% Values of that type are opaque (their translation as terms should remain
% unbeknownst to the user, as if they were black boxes); such terms are to be
% solely created and handled as a whole by the 'table' pseudo-module.
%
% For example, MyEmptyTable = table:table(), MyTable =
% table:addNewEntry(42,"This is the answer"), MyOtherTable = table:new([ {1,
% "One"}, {2, "Two"}, {5, "Five"} ]).
%
% Note: tables are not yet supported.



% To contrast, here are a few Erlang examples, obtained thanks to
% meta_utils:string_to_form/1 (see http://erlang.org/doc/apps/erts/absform.html
% for more details); a forward slash ("/") separates these Erlang forms from the
% type constructs defined here.
%
% For instance meta_utils:string_to_form("-type my_type() :: 'a'|'b'."). yields:
% {attribute,1,type,{my_type,{type,1,union,[{atom,1,a},{atom,1,b}]},[]}); this
% may be read as the my_type type being defined as
% {type,1,union,[{atom,1,a},{atom,1,b}]}.
%
% We have thus following respective translations of monomorphic types:
% (format of the bullets below: "ERLANG_TYPE_SPEC" / "OUR_SPEC" -> ERLANG_FORM /
% OUR_TERM)
%
% - single-value types:
%   - "4" / "4" -> {integer,1,4} / {integer,4}
%   - "foo" or "'foo'" / "'foo'" -> {atom,1,foo} / {atom,foo}
%
% - alias types:
%    - "float()" / "float" -> {type,1,float,[]} / float

%    - "my_other_type() / "my_other_type" or "my_other_type()" ->
%    {user_type,1,my_other_type,[]} / my_other_type
%
% - union types: "'a'|'b'" / "'a'|'b'" -> {type,1,union,[{atom,1,a},{atom,1,b}]}
% / {union,[{atom,a},{atom,b}]}
%
% - list types : "list(integer())" or "[integer()]" / "[integer]" ->
% {type,1,list,[{type,1,integer,[]}]} / { list, integer }

% - random examples:
%
% - "{integer(),float()}" / "{integer,float}" ->
%        {type,1,tuple, [{type,1,integer,[]},{type,1,float,[]}]} /
%        {tuple,[integer,float]}




% Implementation notes about parse transforms:

% Here are some resources to better understand parse transforms (PT, here):
%
% - generic information about PT: in http://www.erlang-factory.com/ :
% upload/presentations/521/yrashk_parse_transformations_sf12.pdf
%
% - Abstract Format: http://www.erlang.org/doc/apps/erts/absform.html (full spec
% of the AST format)
%
% - http://chlorophil.blogspot.fr/2007/04/erlang-macro-processor-v1-part-i.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-ii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iv.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-v.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vi.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vii.html


% We consider here that an AST is an ordered list of forms.
%
% We often use located counterparts of the standard elements (ex: forms, ASTs)
% so that we can recreate and modify the order of (possibly transformed, added
% or removed) forms in an AST.
%
% See the definition of the location/0 type for further information.


% Standard modules of interest:
%
% - erl_scan ('The Erlang Token Scanner'): functions for tokenizing characters
% into Erlang tokens
%
% - epp ('An Erlang Code Preprocessor'): functions which are used by compile to
% preprocess macros and include files before the actual parsing
%
% - erl_parse ('The Erlang Parser'): basic Erlang parser
%
% - erl_eval ('The Erlang Meta Interpreter'): interpreter for Erlang
% expressions, in the abstract syntax
%
% - erl_pp ('The Erlang Pretty Printer'): to display abstract forms
%
% - erl_lint ('The Erlang Code Linter'): to check Erlang code for illegal
% syntax, bugs, unrecommended coding practices, etc.
%
% - compile ('The Erlang Compiler'): interface to the standard Erlang compiler

% Example of PT: http://www.erlang.org/doc/man/erl_id_trans.html


% Third-party libraries of interest:
%
% - https://github.com/uwiger/parse_trans
% - https://github.com/uwiger/toker


% Useful information:
%
% - how to convert source code into actual code: on
% http://stackoverflow.com/questions/,
% 2160660/how-to-compile-erlang-code-loaded-into-a-string


% Use -P to see the code generated by a parse-transform; ex: 'erlc -P' or in the
% shell as 'c( "X.erl", [ 'P' ] )'.



% This module being a bootstrap one, the 'table' pseudo-module is not available
% (as this module is not processed by the 'Common' parse transform):
%
% Indeed, no table pseudo-module available from meta_utils, as it cannot be
% parse-transformed; only ?table is available here, not the other *_hashtable
% counterparts (once that meta_utils module is compiled, if it relied on
% foo_hashtable, then the parse transform could not operate on any module
% compiled before foo_hashtable):
%
-define( table, map_hashtable ).


% For function_info:
-include("meta_utils.hrl").


% For the file_info record:
-include_lib("kernel/include/file.hrl").



% Type-related section.


% Options specified to a parse transform at runtime, like report_warnings,
% beam,report_errors, {cwd,"X"}, {outdir,"Y"}, {i,"Z"}, {parse_transform,P},
% debug_info,warnings_as_errors, etc.
%
-type parse_transform_options() :: proplists:proplist().


% Line location (i.e. line number) of a form in a source file:
-type line() :: erl_anno:line().


% Line-related location in a source file (either line() or {line(), column()}):
%
-type file_loc() :: erl_anno:location().


% Abstract form, part of an AST (ex: {attribute,40,file,{"foo.erl",40}}):
%
-type form() :: erl_parse:abstract_form().


% Abstract Syntax Tree, standard representation of parse trees for Erlang
% programs as Erlang terms. This representation is known as the abstract
% format.
%
% For more information: http://www.erlang.org/doc/apps/erts/absform.html
%
-type ast() :: [ form() ].



% Location of a form in an AST, so that the order of forms can be recreated.
%
% We use sortable identifiers so that any number of new forms can be introduced
% between two of them, if needed.
%
% Location is relative to the position of a form in a given AST, while the line
% information embedded in forms is relative to the file in which they are
% defined.
%
-type location() :: basic_utils:sortable_id().



% When processing an AST (ex: read from a BEAM file), the order of the forms
% matters (for example to report compile errors, which are relative to a context
% defined by the last '-file' attribute previously encountered, i.e. like
% {attribute,40,file,{"foo.erl",40}}). So even if we store forms in tables
% according to their type, when (re)generating the AST we have to recreate the
% same order.
%
% To do so, instead of managing a list of forms, we manage any sets of located
% forms by including in each form an identifier allowing to recreate the form
% order in the original AST.
%
-type located_form() :: { location(), form() }.


% An AST including location information:
%
-type located_ast() :: [ located_form() ].



% The name of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
%
-type attribute_name() :: atom().


% The value of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
%
-type attribute_value() :: term().



% Parse-level attribute:
%
-type attribute() :: { attribute_name(), attribute_value() }.



% The name of a function:
%
-type function_name() :: basic_utils:function_name().


% Declaration of a function based on a name with an arity (unique function
% signature within a module):
%
-type function_id() :: { function_name(), arity() }.


% The form corresponding to the definition of a clause of a function, typically
% { clause, LINE, Rep(Ps), Rep(Gs), Rep(B) } for '( Ps ) when Gs -> B':
%
-type clause_def() :: form().



% The full type specification (if any) of that function, as an abstract form;
% typically:
%
% { attribute, L, spec, { {foobar,Arity}, [{type,L,'fun', [{type,L,...
%
-type function_spec() :: form().


% Located type specification of a function:
%
-type located_function_spec() :: { location(), function_spec() }.


-type function_info() :: #function_info{}.



% Describes the name of a type (without the names of the types it depends on,
% for polymorphic ones).
%
-type type_name() :: atom().


% Number of types a (possibly polymorphic) type depends on (possibly zero for
% plain types).
%
-type type_arity() :: basic_utils:count().


% The "most precise" description of a primitive, simple type (ex: 'boolean' and
% 'atom') coexist, 'number' are not used, etc.
%
% A note about Erlang floats: they are actually IEEE 754 double-precision
% floating-point numbers, a format that occupies 8 bytes (64 bits) per float in
% memory.
%
% More precisely, as one can see in erts/emulator/beam/erl_term.h, a float_def
% is an union able to contain a ieee754_8 datatype, aliased to the 'double' C
% datatype.
%
% Polymorphic types (ex: lists) are described with no mention of the types they
% may depend on (ex: 'list' can be specified, not 'list(float())' or anything
% like that).
%
-type primitive_type_description() :: 'atom'
									| 'binary'
									| 'boolean'
									| 'float'
									| 'function'
									| 'integer'
									| 'list'
									| 'pid'
									| 'port'
									| 'record'
									| 'reference'
									| 'tuple'.


% The description of any given type is based on primitive_type_description/0)
% and can be done in two complementary forms: the textual one, and the internal
% one, which are relatively different.



% Textual type description: type-as-a-string, inspired from the syntax used for
% type specifications (http://erlang.org/doc/reference_manual/typespec.html),
% yet different. Notably, monomorphic types do not end with empty parentheses
% (ex: "integer", not "integer()") and atoms are always surrounded by simple
% quotes (ex: "'an_atom'|'another_one'").
%
% For example: "[{float,boolean}]".
%
-type type_description() :: string().



% Description of a nesting depth reached when parsing a type description.
%
% It is in pratice a {P,B} pair, where P is the parenthesis depth (i.e. the
% number of the parentheses that have been opened and not closed yet) and B is
% the bracket depth (i.e. the same principle, for "[]" instead of for "()"):
%
-type nesting_depth() :: { basic_utils:count(), basic_utils:count() }.


% Internal, "formal", actual programmatic description of a type according to our
% conventions: type-as-a-term (either contextual or explicit, F2 or F3), relying
% on a translated version of the textual type (which is for example:
% "[{float,boolean}]").
%
% This "internal type language of the Common layer" is largely inspired from the
% forms that can be found in actual ASTs.
%
% Requirements for this term-based description were:
%
% - be able to represent at least any actual (that can be readily instantiated,
% hence non-polymorphic) type (like "-type a() :: ...", not "-type a(T) ::
% ..."); should, in the future, polymorphic types have to be *defined* (not
% merely used), then (non-empty) parentheses could be introduced
%
% - be able to nevertheless *use* polymorphic types, as they are certainly
% useful (ex: associative tables, lists, etc.); a problem is that, in terms (as
% opposed to in the textual counterpart), parentheses cannot be used to express
% these polymorphic types (not only they denote function calls, but also are
% not legit components of a term); therefore the convention chosen here is to
% specify types as pairs, the first element being the name of the type, the
% second one being the (ordered) list of the types it depends on; then the
% textual type "a( T1, T2 )" is translated to the {a,[T1,T2]} type term; most
% types being "monomorphic", they are represented as {my_simple_type,[]} (which
% cannot be abbreviated by only the 'my_simple_type' atom, as it would lead to
% ambiguous forms)
%
% So, as an example, the type-as-a-term corresponding to "[{float,boolean}]"
% is: { list, [ { tuple, [ {float,[]}, {boolean,[]} ] } ] }
%
% Note that an alternate type language (sticking more closely to its textual
% counterpart) could have been a more direct [{float,boolean}] term (hence
% getting rid of the parentheses and the pair with an empty list in second
% position); reason for not doing so: then no possible support of the
% polymorphic types that happen to be often needed.
%
% The origin of this term-as-a-type notation is clearly the standard (Erlang)
% type specifications; for example 'meta_utils:string_to_form( "-type a() ::
% [{float(),boolean()}]." ).' returns following AST form:
%
%  '{attribute,1,type, {a,{type,1,list, [{type,1,tuple,[{type,1,float,[]},
%  {type,1,boolean,[]}]}]}'
%
% As a result the counterpart to the aforementioned "[{float(),boolean()}]" type
% string is translated in ASTs as:
%
% { type, 1, list, [{type,1,tuple,[{type,1,float,[]}, {type,1,boolean,[]}]} ] }
%
% Then one can remove:
%
% - the 'type' (and 'user_type') atoms (not making then a specific distinction
% between the origin of a type); a list of built-in types - names and arities -
% is maintained, other types being then user ones)
%
% - the line numbers (the '1's here), not useful in that context, hence stripped
%
% Then we obtain our aforementioned term-as-a-type:
%    { list, [ { tuple, [ {float,[]}, {boolean,[]} ] } ] }
%
% We can therefore describe this way arbitrary types as valid terms.
%
% Next steps:
%
% - define and document the full type language (elementary datatypes - like
% boolean, integer, float, symbols - and constructs - like list, tuple, union,
% atom)
%
% - support it, notably define functions to tell whether a given term is an
% instance of a specified type
%
% Experiment with meta_utils:string_to_form/1 and have fun!
%
% Ex: "-type a() :: [foobar()]." yields: '{attribute,1,type, {a,{type,1,
%    list,[{user_type,1,foobar,[]}]},[]}}'.
%
% See also: http://erlang.org/doc/apps/erts/absform.html
%
% Finally, a direct string representation can be converted into a type(); maybe
% writing a parser may not mandatory, as "{ float(), atom() }" may be a string
% expression evaluated with functions that we can bind to obtain a closer term,
% such as: float() -> { float, [] }.
%
% Of course, on a related note, if TextualType = "{ list, [
% {tuple,[float,boolean]} ] }", then meta_utils:string_to_value( TextualType )
% will return the expected: {list,[{tuple,[{float,[]},{boolean,[]}]}]}
%
% Note that such a type may not be fully explicit, as it may contain unresolved
% references to other types; for example: { list, [ {count,[] } ] } does not
% specify what the count() type is.
%
-type type() :: term().


% An explicit type is a type that has been fully resolved in terms of built-in
% constructs; it is thus self-standing.
%
-type explicit_type() :: type().


% Type of functions to transform terms during a recursive traversal (see
% traverse_term/4).
%
% Note: apparently we cannot use the 'when' notation here (InputTerm ... when
% InputTerm :: term()).
%
-type term_transformer() :: fun( ( term(), basic_utils:user_data() ) ->
									   { term(), basic_utils:user_data() } ).



-type module_info() :: #module_info{}.



% Directly inspired from erl_lint:


% Description of a compilation-related issue (error or warning).
%
-type issue_description() :: term().


% Full information about a compilation-related issue.
%
% The module is the one emitting that issue (ex: erl_lint)
%
-type issue_info() :: { line(), module(), issue_description() }.


% A warning regarding a source file, corresponding to a list of error
% informations.
%
-type issue_report() :: { file_utils:file_name(), [ issue_info() ] }.



-export_type([ parse_transform_options/0, line/0, file_loc/0, form/0, ast/0,
			   location/0, located_form/0, located_ast/0,
			   attribute_name/0, attribute_value/0, attribute/0,
			   function_name/0, function_id/0,
			   clause_def/0, function_spec/0, located_function_spec/0,
			   function_info/0,
			   type_name/0, type_arity/0, primitive_type_description/0,
			   type_description/0, nesting_depth/0, type/0, explicit_type/0,
			   term_transformer/0, module_info/0,
			   issue_description/0, issue_info/0, issue_report/0 ]).



% Parse-transform related functions:
%
-export([ init_module_info/0,
		  function_info_to_string/1,
		  traverse_term/4,
		  term_to_form/1, variable_names_to_ast/2,
		  string_to_form/1, string_to_form/2,
		  string_to_expressions/1, string_to_expressions/2,
		  string_to_value/1,
		  beam_to_ast/1,
		  extract_module_info_from_ast/1, recompose_ast_from_module_info/1,
		  erl_to_ast/1,
		  check_module_info/1, module_info_to_string/1,
		  write_ast_to_file/2,
		  raise_error/1, get_error_form/3, format_error/1 ]).


% General functions:
%
-export([ list_exported_functions/1, get_arities_for/2,
		  is_function_exported/3, check_potential_call/3 ]).


% Type-related functions:
%
-export([ description_to_type/1, type_to_description/1, type_to_string/1,
		  get_type_of/1, get_elementary_types/0, is_type/1, is_of_type/2,
		  is_of_described_type/2, is_homogeneous/1, is_homogeneous/2,
		  are_types_identical/2 ]).



% For debugging:
-export([ interpret_issue_reports/1, interpret_issue_report/1,
		  interpret_issue_info/2, interpret_issue_description/2 ] ).


% Work in progress:
-export([ tokenise_per_union/1 ]).

% Returns a new, blank instance of the module_info record.
%
-spec init_module_info() -> module_info().
init_module_info() ->
	#module_info{ functions=?table:new() }.



% Returns a textual description of the specified function information.
%
-spec function_info_to_string( function_info() ) -> text_utils:ustring().
function_info_to_string( #function_info{
							name=Name,
							arity=Arity,
							location=_Location,
							definition=Clauses,
							spec=LocatedSpec,
							exported=Exported } ) ->

	ExportString = case Exported of

		true ->
			"exported";

		false ->
			"local"

	end,

	DefString = io_lib:format( "~B clause(s) defined", [ length( Clauses ) ] ),

	SpecString = case LocatedSpec of

		undefined ->
			"no type specification";

		_ ->
			"a type specification"

	end,

	io_lib:format( "~s/~B, ~s, with ~s and ~s",
				   [ Name, Arity, ExportString, DefString, SpecString ] ).



% Traverses specified term (possibly with nested subterms - the function will
% recurse in lists and tuples), calling specified transformer function on each
% instance of specified type, in order to replace that instance by the result of
% that function.
%
% Returns an updated term, with these replacements made.
%
% Ex: the input term could be T={ a, [ "foo", { c, [ 2.0, 45 ] } ] } and the
% function might replace, for example, floats by <<bar>>; then T'={ a, [ "foo",
% { c, [ <<bar>>, 45 ] } ] } would be returned.
%
% Note: the transformed terms are themselves recursively transformed, to ensure
% nesting is managed. Of course this implies that the term transform should not
% result in iterating the transformation infinitely.
%
% As a result it may appear that a term of the targeted type is transformed
% almost systematically twice: it is first transformed as such, and the result
% is transformed in turn. If the transformed term is the same as the original
% one, then that content will be shown as analysed twice.
%
-spec traverse_term( term(), primitive_type_description(), term_transformer(),
			 basic_utils:user_data() ) -> { term(), basic_utils:user_data() }.

% Here the term is a list and this is the type we want to intercept:
traverse_term( TargetTerm, _TypeDescription=list, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	traverse_transformed_term( TransformedTerm, _TypeDescription=list,
							   TermTransformer, NewUserData );


% Here the term is a list and we are not interested in them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	traverse_list( TargetTerm, TypeDescription, TermTransformer, UserData );


% Here the term is a tuple (or a record...), and we want to intercept them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm )
	andalso ( TypeDescription =:= tuple orelse TypeDescription =:= record ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	traverse_transformed_term( TransformedTerm, TypeDescription,
							   TermTransformer, NewUserData );


% Here the term is a tuple (or a record...), and we are not interested in them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm ) ->

	traverse_tuple( TargetTerm, TypeDescription, TermTransformer, UserData );


% Base case (current term is not a binding structure, it is a leaf of the
% underlying syntax tree):
%
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData ) ->

	case get_type_of( TargetTerm ) of

		TypeDescription ->
			TermTransformer( TargetTerm, UserData );

		_ ->
			% Unchanged:
			{ TargetTerm, UserData }

	end.



% Helper to traverse a list.
%
traverse_list( TargetList, TypeDescription, TermTransformer, UserData ) ->

	{ NewList, NewUserData } = lists:foldl(
								 fun( Elem, { AccList, AccData } ) ->

			{ TransformedElem, UpdatedData } = traverse_term( Elem,
							TypeDescription, TermTransformer, AccData ),

			% New accumulator, produces a reversed element list:
			{ [ TransformedElem | AccList ], UpdatedData }

								 end,

								 _Acc0={ _Elems=[], UserData },

								 TargetList ),

	{ lists:reverse( NewList ), NewUserData }.



% Helper to traverse a tuple.
%
traverse_tuple( TargetTuple, TypeDescription, TermTransformer, UserData ) ->

	% We do exactly as with lists:
	TermAsList = tuple_to_list( TargetTuple ),

	{ NewList, NewUserData } = traverse_list( TermAsList, TypeDescription,
											  TermTransformer, UserData ),

	{ list_to_tuple( NewList ), NewUserData }.



% Helper to traverse a transformed term (ex: if looking for a { user_id, String
% } pair, we must recurse in nested tuples like: { 3, { user_id, "Hello" }, 1 }.
%
traverse_transformed_term( TargetTerm, TypeDescription, TermTransformer,
						   UserData ) ->

	case TermTransformer( TargetTerm, UserData ) of

		{ TransformedTerm, NewUserData } when is_list( TransformedTerm ) ->
			traverse_list( TransformedTerm, TypeDescription, TermTransformer,
						   NewUserData );

		{ TransformedTerm, NewUserData } when is_tuple( TransformedTerm ) ->
			traverse_tuple( TransformedTerm, TypeDescription, TermTransformer,
						   NewUserData );

		% { ImmediateTerm, NewUserData } ->
		Other ->
			Other

	end.




% Section to manage ASTs and forms.


% Converts the specified Erlang term (ex: the float '42.0') into a corresponding
% form (ex: '{ float, _Line=0, 42.0 }').
%
-spec term_to_form( term() ) -> form().
term_to_form( Term ) ->

	case erl_syntax:abstract( Term ) of

		% Either the doc or the type information for erl_syntax:abstract/1 is
		% incorrect:

		%badarg ->
		%	throw( { term_abstraction_failed, Term } );

		SyntaxTree ->

			% Could be used with erl_syntax:is_tree/1:
			% case erl_syntax:revert( SyntaxTree ) of...
			erl_syntax:revert( SyntaxTree )

	end.



% Converts a list of names of variables into the corresponding AST.
%
% Ex: variable_names_to_ast( [ "V1", "Alpha", "A" ], _Line=0 ) = [ {cons,0,
% {var,0,'V1'}, {cons,0,{var,0,'Alpha'}, {cons,0,{var,0,'A'}, {nil,0} } } } ]
%
-spec variable_names_to_ast( [ string() ], line() ) -> ast().
variable_names_to_ast( VariableNames, Line ) ->

	% Could be done directly recursively by incrementally 'consing' reversed
	% list.

	NameListString = "[ " ++ text_utils:join( ", ",  VariableNames ) ++ " ].",

	string_to_expressions( NameListString, Line ).



% Converts the specified source code of a form (as a string) into its
% corresponding abstract form (assuming being in line #1).
%
% Ex: string_to_form( "f() -> hello_world." ) returns
%   { function, 1, f, 0, [ { clause, 1, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( string() ) -> form().
string_to_form( FormString ) ->
	string_to_form( FormString, _Loc=1 ).



% Converts the specified source code of a form (i.e., a string) into its
% corresponding abstract form.
%
% Ex: string_to_form( "f() -> hello_world.", 42 ) returns
%   { function, 1, f, 0, [ { clause, 42, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( string(), file_loc() ) -> form().
string_to_form( FormString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( FormString, Location ) of

		% Ex: [{atom,1,f},{'(',1},{')',1},{'->',1},{atom,1,hello_world},{dot,1}]
		{ ok, Toks, _EndLocation } ->
			%io:format( "Tokens: ~p~n", [ Toks ] ),
			Toks;

		ErrorTok ->
			throw( { form_tokenizing_error, FormString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_form( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { form_parsing_error, FormString, ErrorPar } )

	end.



% Converts the specified source code of a list of expressions (i.e., a string)
% into its corresponding AST (assuming being in line #1).
%
% Ex: string_to_expressions( "[ { a, 1 }, foobar ]" ) returns
%   [ { cons, 1, { tuple, 1, [ {atom,1,a}, {integer,1,1} ] },
%     { cons, 1, {atom,1,foobar}, {nil,1} } } ]
%
-spec string_to_expressions( string() ) -> ast().
string_to_expressions( ExpressionString ) ->
	string_to_expressions( ExpressionString, _Loc=1 ).



% Converts the specified source code of a term (i.e., a string) and a location
% into the corresponding abstract form.
%
% Ex: string_to_expressions( "[ { a, 1 }, foobar ]", _Loc=42 ) returns
%   [ { cons, 42, { tuple, 42, [ {atom,42,a}, {integer,42,1} ] },
%     { cons, 42, {atom,42,foobar}, {nil,42} } } ]
%
-spec string_to_expressions( string(), file_loc() ) -> ast().
string_to_expressions( ExpressionString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( ExpressionString, Location ) of

		% Ex: [ {'[',42}, {'{',42}, {atom,42,a}, {',',42}, {integer,42,1},
		% {'}',42}, {',',42}, {atom,42,foobar}, {']',42} ]
		{ ok, Toks, _EndLocation } ->
			%io:format( "Tokens: ~p~n", [ Toks ] ),
			Toks;

		ErrorTok ->
			throw( { expression_tokenizing_error, ExpressionString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_exprs( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { expression_parsing_error, ExpressionString, ErrorPar } )

	end.



% Converts the specified source code of a term (i.e., a string) into its
% corresponding value.
%
% Ex: string_to_value( "[ {tiger,[lion,leopard]} ]" ) returns the
% [{tiger,[lion,leopard]}] term.
%
-spec string_to_value( string() ) -> term().
string_to_value( ExpressionString ) ->

	% We automatically add the necessary final dot:
	[ Expr ] = string_to_expressions( ExpressionString ++ "." ),

	{ value, Result, _NewBindings } = erl_eval:expr( Expr, _Bindings=[] ),

	Result.



% Reads the specified BEAM file (expected to be compiled with debug information)
% and returns the corresponding AST.
%
% Note that the filename must be a relative or absolute path pointing directly
% to the BEAM file (it is not searched through the code path).
%
-spec beam_to_ast( file:filename() ) -> ast().
beam_to_ast( BeamFilename ) ->

	% We do not use functions from other Common modules here (ex: file_utils) as
	% they are not expected to be built yet (they will be built with the common
	% parse transform afterwards).
	%
	case file:read_link_info( BeamFilename ) of

		{ ok, FileInfo } ->
			#file_info{ type=regular } = FileInfo,
			ok;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, BeamFilename } );

		{ error, enoent } ->
			throw( { non_existing_beam_file, BeamFilename } )

	end,

	% We could basically list all chunks, but we are only interested here in the
	% abstract code:
	%

	% Everything:
	%Chunks = [ abstract_code, attributes, compile_info, exports,
	%		   labeled_exports, imports, indexed_imports, locals,
	%		   labeled_locals, atoms ],

	% Just the code AST:
	Chunks = [ abstract_code ],

	% Everything but the code AST:
	% OtherChunks = [ attributes, compile_info, exports,
	%		   labeled_exports, imports, indexed_imports, locals,
	%		   labeled_locals, atoms ],

	%Options = [ allow_missing_chunks ],
	Options=[],

	case beam_lib:chunks( BeamFilename, Chunks, Options ) of

		{ ok, { _Module, [ { abstract_code, { _RawAbstractV1,
											  AbstractCode } } ] } } ->
			%io:format( "Module = ~p.~n", [ Module ] ),
			AbstractCode;

		{ error, beam_lib, Reason } ->
			throw( { beam_reading_failed, Reason } )

	end.



% Processes the specified AST relative to a whole module, and returns the
% corresponding information gathered.
%
-spec extract_module_info_from_ast( ast() ) -> module_info().
extract_module_info_from_ast( AST ) ->

	%io:format( "Processing following AST:~n~p~n", [ AST ] ),
	%io:format( "Processing AST:~n" ),

	%write_ast_to_file( AST, "original-extracted-ast.txt" ),

	% First we check whether the corresponding code compiles:

	% We could define specific compile options, yet they could be too
	% restrictive (more than the ones of the compiler) and moreover it would
	% force us to specify a filename.

	% We cannot simply count errors and warnings, as we have in each case a list
	% of per-file elements (a list of lists), in the order of the specified AST
	% (thus additionally a given file may happen multiple times); a count is not
	% useful here anyway.

	% Finally we have not real freedom in terms of output, as we prefer to
	% respect the native display format of the error messages so that tools (ex:
	% emacs, possible erlide and all) are still able to manage them.

	pre_check_ast( AST ),

	InitModuleInfo = init_module_info(),

	ModuleInfo = process_ast( AST, InitModuleInfo, _InitialFormCounter=1 ),

	% Uncomment with care, as must ultimately depend *only* on non-bootstrapped
	% modules (like {meta,text}_utils) - this should be the case here:
	%
	%io:format( "Resulting module information:~n~s~n",
	%		   [ module_info_to_string( ModuleInfo ) ] ),

	case ModuleInfo#module_info.unhandled_forms of

		[] ->
			ok;

		UnhandledForms ->

			UnHandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			io:format( "Warning, ~B forms have not be handled: ~s",
					   [ length( UnhandledForms ),
						 text_utils:strings_to_string( UnHandledStrings ) ] )

	end,

	% Additional linting, just after extraction:
	check_module_info( ModuleInfo ),

	ModuleInfo.


pre_check_ast( AST ) ->

	%io:format( "~p~n", [ AST ] ),

	% Finally interpret_issue_reports/1 directly outputs the issues:
	case erl_lint:module( AST ) of

		{ ok, _Warnings=[] } ->
			%io:format( "(no warning or error emitted)~n" ),
			ok;

		{ ok, Warnings } ->
			%io:format( "Warnings, reported as errors: ~s~n",
			%		   [ interpret_issue_reports( Warnings ) ] ),
			interpret_issue_reports( Warnings ),
			exit( warning_reported );

		{ error, Errors, _Warnings=[] } ->
			%io:format( "Errors reported: ~s~n",
			%		   [ interpret_issue_reports( Errors ) ] ),
			interpret_issue_reports( Errors ),
			exit( error_reported );

		{ error, Errors, Warnings } ->
			%io:format( "Errors reported: ~s~n",
			%		   [ interpret_issue_reports( Errors ) ] ),
			interpret_issue_reports( Errors ),

			%io:format( "Warnings, reported as errors: ~s~n",
			%		   [ interpret_issue_reports( Warnings ) ] ),
			interpret_issue_reports( Warnings ),

			exit( error_reported )

	end.



% Here all relevant parts of the specified AST (located forms) are matched in
% turn, and stored in the specified module_info once located using
% basic_utils:sortable_id/0 identifiers.
%
% Each of these initial sortable identifiers is here just a list of one integer,
% a counter incremented at each form (see basic_utils:sortable_id/0 for the
% description of these expandable identifiers).


% Module section:

% Any lacking, invalid or duplicated module declaration will be caught by the
% compiler anyway:
%
-spec process_ast( located_ast(), module_info(), basic_utils:count() ) ->
						 module_info().
process_ast( _AST=[ Form={ attribute, _Line, module, ModuleName } | T ],
			 W=#module_info{ module=undefined, module_def=undefined },
			 FormCounter ) ->

	%io:format( " - module declaration for ~s~n", [ ModuleName ] ),

	% When processing X.beam, we should not remove the lines like:
	% {attribute,37,file,{"X.erl",37} as they allow to report errors
	% appropriately.

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{ module=ModuleName, module_def=LocForm },
				 FormCounter+1 );



% Include section:
%
process_ast( _AST=[ Form={ attribute, _Line, file, { Filename, _N } } | T ],
			 W=#module_info{ includes=Inc, include_defs=IncDefs },
			 FormCounter ) ->

	%io:format( " - file declaration with ~s~n", [ Filename ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NormFilename = file_utils:normalise_path( Filename ),
	NormFilename = Filename,

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilenames = case lists:member( NormFilename, Inc ) of

		true ->
			Inc;

		false ->
			[ NormFilename | Inc ]

	end,

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{ includes=NewFilenames,
								   include_defs=[ LocForm | IncDefs ] },
				 FormCounter+1 );



% Type definition section:
%
process_ast( _AST=[ Form={ attribute, _Line, type,
						   { TypeName, TypeDef, _SubTypeList } } | T ],
			 W=#module_info{ type_definitions=TypeDefs,
							 type_definition_defs=TypeDefsDefs },
			 FormCounter ) ->

	%io:format( " - type declaration for ~p: ~p~n", [ TypeName, F ] ),

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{
				   type_definitions=[ { TypeName, TypeDef } | TypeDefs ],
				   type_definition_defs=[ LocForm | TypeDefsDefs ] },
				 FormCounter+1 );



% Type export section:
%
process_ast( _AST=[ Form={ attribute, _Line, export_type, DeclaredTypes } | T ],
			 W=#module_info{ type_exports=TypeExports,
							 type_export_defs=TypeExportDefs },
			 FormCounter ) when is_list( DeclaredTypes ) ->

	%io:format( " - export type declaration for ~p~n", [ DeclaredTypes ] ),

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{ type_exports= DeclaredTypes ++ TypeExports,
								   type_export_defs=
									   [ LocForm | TypeExportDefs ] },
				 FormCounter+1 );



% Function export section:
%
process_ast( _AST=[ Form={ attribute, _Line, export, FunctionIds } | T ],
			 W=#module_info{ function_exports=FunExports,
							 functions=FunctionTable }, FormCounter ) ->

	%io:format( " - export declaration for ~p~n", [ FunctionIds ] ),

	NewFunctionTable = lists:foldl(

		fun( FunId={ Name, Arity }, FunTableAcc ) ->

			 NewFunInfo = case ?table:lookupEntry( FunId, FunTableAcc ) of

				 key_not_found ->

					  % New entry then:
					  #function_info{
						 name=Name,
						 arity=Arity,
						 % Implicit:
						 %location=undefined
						 %line=undefined
						 %definition=[],
						 %spec=undefined
						 exported=true
						};

				 % A function *might* be exported more than once:
				 { value, FunInfo } -> % F=#function_info{ exported=false } } ->
					  % Just add the fact that the function is exported then:
					  FunInfo#function_info{ exported=true }

			end,

			?table:addEntry( FunId, NewFunInfo, FunTableAcc )

		end,

		_Acc0=FunctionTable,
		_List=FunctionIds ),

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{
					  function_exports=[ LocForm | FunExports ],
					  functions=NewFunctionTable }, FormCounter+1 );



% Function definition section:
%
process_ast( _AST=[ _Form={ function, Line, Name, Arity, Clauses } | T ],
			 W=#module_info{ functions=FunctionTable }, FormCounter ) ->

	%io:format( " - function definition for ~p:~p~n", [ Name, Arity ] ),

	% The non-first clauses could be checked as well:
	%
	% (when adding a function, we may not check if ever there was a pre-existing
	% one - multiple definitions will be rejected by the compiler anyway)

	FunId = { Name, Arity },

	BasicLocation = [ FormCounter ],

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{
			   name=Name,
			   arity=Arity,
			   location=BasicLocation,
			   line=Line,
			   definition=Clauses
			   % Implicit:
			   %spec=undefined
			  };

		{ value, F=#function_info{ definition=[] } } ->
				% Already here because of an export; just add the missing
				% information then:
				F#function_info{ location=BasicLocation,
								 line=Line,
								 definition=Clauses };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for, FunId } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%io:format( "function ~s/~B with ~B clauses registered.~n",
	%		   [ Name, Arity, length( Clauses ) ] ),

	process_ast( T, W#module_info{ functions=NewFunctionTable },
				 FormCounter+1 );



% Spec attributes:
process_ast( _AST=[ Form={ attribute, _Line, spec, {
											   FunId={ FunctionName, Arity },
											   _SpecList } } | T ],
			 W=#module_info{ functions=FunctionTable },
			 FormCounter ) ->

	%io:format( " - spec definition for ~p:~p~n", [ FunctionName, Arity ] ),

	BasicLocation = [ FormCounter ],

	LocatedSpec = { BasicLocation, Form },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{
			   name=FunctionName,
			   arity=Arity,
			   % Implicit:
			   %location=undefined,
			   %line=undefined,
			   %definition=[]
			   spec=LocatedSpec
			  };

		{ value, F=#function_info{ spec=undefined } } ->
			% Just add the form then:
			F#function_info{ spec=LocatedSpec };

		% Here a spec was already set:
		_ ->
			raise_error( { multiple_spec_for, FunId } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%io:format( "spec for function ~s/~B registered.~n",
	%		   [ FunctionName, Arity ] ),

	process_ast( T, W#module_info{ functions=NewFunctionTable },
				 FormCounter+1 );



% Other attribute section:
%
process_ast( _AST=[ Form={ attribute, _Line, AttributeName, AttributeValue }
					| T ],
			 W=#module_info{ parse_attributes=Attributes,
							 parse_attribute_defs=AttributeDefs },
			 FormCounter ) ->

	%io:format( " - attribute definition for ~p~n", [ AttributeName ] ),

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	process_ast( T, W#module_info{
				   parse_attributes=[ { AttributeName, AttributeValue }
									  | Attributes ],
				   parse_attribute_defs=[ LocForm | AttributeDefs ] },
				 FormCounter+1 );



% We expect the module name to be known when ending the processing:
%
process_ast( _AST=[ _Form={ eof, _Line } ],
			 Infos=#module_info{ module=undefined }, _FormCounter ) ->
	raise_error( { eof_while_no_module, Infos } );



% Form expected to be defined once, and to be the last one:
%
process_ast( _AST=[ Form={ eof, _Line } ], W=#module_info{ last_line=undefined,
							   module=Module, includes=Inc }, FormCounter ) ->

	%io:format( " - eof declaration at ~p~n", [ Line ] ),

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	% End of file found, doing some housekeeping.

	% We do not want to have the filename of the currently processed module in
	% the includes:

	% Reconstructs the supposedly deduced module filename:
	ModFilename = atom_to_list( Module ) ++ ".erl",

	% Due to the removal of include duplicates, can be listed only up to once:
	NoModInc = lists:delete( ModFilename, Inc ),

	% Only "normal", non-recursing exit of that function:
	W#module_info{ includes=NoModInc, last_line=LocForm };


% Handling errors:
%
% Apparently parse transforms *have* to manage errors by themselves. Indeed we
% stored them in the unhandled_forms field of the module_info record and
% reinjected them in the returned AST, hoping that the compiler chain would
% complain appropriately (at least as it does without parse transforms).
%
% However it does not seem to be the case, as reinserting the error forms (where
% they were, thanks to their location information) results in (when adding an
% intentional error in the source file):
%
% foo.erl: internal error in lint_module;
% crash reason: badarg
%
% in function  erl_anno:set/3
%    called as erl_anno:set(file,"class_Mesh.erl",undefined)
% in call from erl_lint:set_form_file/2 (erl_lint.erl, line 690)
% in call from erl_lint:eval_file_attr/2 (erl_lint.erl, line 679)
% in call from erl_lint:forms/2 (erl_lint.erl, line 641)
% in call from erl_lint:module/3 (erl_lint.erl, line 498)
% in call from compile:lint_module/1 (compile.erl, line 999)
% in call from compile:'-internal_comp/4-anonymous-1-'/2 (compile.erl, line 295)
% in call from compile:fold_comp/3 (compile.erl, line 321)
%
% So we manage the errors by ourselves (losing the emacs error mode support).


% (for some reason, the reported lines are incremented, we have to decrement
% them)


% Preprocessor (eep) errors:

process_ast( _AST=[ _Form={ error,
	   { Line, epp, { include, file, FileName } } } | _T ], _Infos,
			 _FormCounter ) ->

	raise_error( { include_file_not_found, FileName, { line, Line-1 } } );


process_ast( _AST=[ _Form={ error,
	   { Line, epp, { undefined, VariableName, none } } } | _T ], _Infos,
			 _FormCounter ) ->

	raise_error( { undefined_macro_variable, VariableName, { line, Line-1 } } );


process_ast( _AST=[ _Form={ error, { Line, epp, Reason } } | _T ], _Infos,
			 _FormCounter ) ->

	raise_error( { preprocessing_failed, Reason, { line, Line-1 } } );



% Parser (erl_parse) errors:

process_ast( _AST=[ _Form={ error, { Line, erl_parse, Reason } } | _T ],
			 _Infos, _FormCounter ) ->

	raise_error( { parsing_failed, text_utils:format( Reason, [] ),
			   { line, Line-1 } } );


% Catch-all, to ensure that we captured all possible forms:
%
process_ast( _AST=[ Form | T ], W=#module_info{
									 unhandled_forms=UnhandledForms },
			 FormCounter ) ->

	% io:format( "WARNING: unhandled form '~p' not managed.~n", [ Form ] ),
	% raise_error( { unhandled_form, Form } );

	LocForm = { _BasicLocation=[ FormCounter ], Form },

	NewUnhandledForms = [ LocForm | UnhandledForms ],

	process_ast( T, W#module_info{ unhandled_forms=NewUnhandledForms },
				 FormCounter+1 );


process_ast( _AST=[], Infos, _FormCounter ) ->
	raise_error( { no_eof_found, Infos } ).





% Recomposes an AST from specified module information.
%
-spec recompose_ast_from_module_info( module_info() ) -> ast().
recompose_ast_from_module_info( #module_info{

			% Between parentheses: fields unused here

			% (module)
			module_def=ModuleDef,
			compilation_option_defs=CompileOptDefs,
			% (parse_attributes)
			parse_attribute_defs=ParseAttributeDefs,
			% (includes)
			include_defs=IncludeDefs,
			% (type_definitions)
			type_definition_defs=TypeDefsDefs,
			% (type_exports)
			type_export_defs=TypeExportsDefs,
			function_exports=FunctionExports,
			% The main part of the AST:
			functions=Functions,
			last_line=LastLineDef,
			unhandled_forms=UnhandledForms

								  } ) ->

	FunctionDefs = get_located_forms_of_functions( Functions ),

	% All these definitions are located:
	UnorderedLocatedAST = [ ModuleDef, LastLineDef | TypeExportsDefs
							++ TypeDefsDefs
							++ FunctionExports
							++ ParseAttributeDefs
							++ IncludeDefs
							++ CompileOptDefs
							++ FunctionDefs
							++ UnhandledForms ],

	% As the order of forms matters, we sort them according to their location:
	OrderedLocatedAST = lists:keysort( _LocIndex=1, UnorderedLocatedAST ),

	OrderedAST = [ Form || { _Location, Form } <- OrderedLocatedAST ],

	%io:format( "Recomposed AST:~n~p~n", [ OrderedAST ] ),

	OrderedAST.




% Returns a list of the located forms corresponding to all the functions
% (definition and spec) described in the specified table.
%
-spec get_located_forms_of_functions ( ?table:?table() ) -> ast().
get_located_forms_of_functions( FunctionTable ) ->

	% Dropping the keys (function_id(), i.e. function identifiers), focusing on
	% function_info():
	%
	FunInfos = ?table:values( FunctionTable ),

	lists:foldl( fun( FunInfo, ASTAcc ) ->
						 get_forms_for_fun( FunInfo ) ++ ASTAcc
				 end,
				 _Acc0=[],
				 _List=FunInfos ).



% Returns an AST corresponding to specified function information.
%
-spec get_forms_for_fun( function_info() ) -> ast().
get_forms_for_fun( #function_info{
					name=Name,
					arity=Arity,
					location=Location,
					line=Line,
					definition=Clauses,
					spec=undefined } ) ->
	[ { Location, { function, Line, Name, Arity, Clauses } } ];

get_forms_for_fun( #function_info{
					name=Name,
					arity=Arity,
					location=Location,
					line=Line,
					definition=Clauses,
					spec=LocSpec } ) ->
	[ LocSpec, { Location, { function, Line, Name, Arity, Clauses } } ].



% Reads specified Erlang source file (*.erl) and returns the corresponding AST.
%
% For example useful to debug a parse transform first separately from the
% compile pipe-line, relying here on the usual, convenient error management
% instead of having little informative messages like: 'undefined parse transform
% 'foobar'' as soon as a call to a non-existing module:function/arity is made.
%
-spec erl_to_ast( file_utils:file_name() ) -> ast().
erl_to_ast( ErlSourceFilename ) ->

	case epp:parse_file( ErlSourceFilename, _Opts=[] ) of

		{ error, Error } ->
			throw( { parse_file_failed, ErlSourceFilename, Error } );

		{ ok, AST } ->
			AST

	end.



% Checks the correctness of specified module information.
%
-spec check_module_info( module_info() ) -> basic_utils:void().
check_module_info( #module_info{ module=undefined } ) ->
	raise_error( no_module_known );

check_module_info( #module_info{ module_def=undefined } ) ->
	raise_error( no_module_defined );

check_module_info( #module_info{ last_line=undefined } ) ->
	raise_error( no_last_line_found );


check_module_info( ModuleInfo=#module_info{ unhandled_forms=[] } ) ->
	%io:format( "Checking AST.~n" ),
	check_module_parse( ModuleInfo ),
	check_module_include( ModuleInfo ),
	check_module_type_definition( ModuleInfo ),
	check_module_export( ModuleInfo ),
	check_module_functions( ModuleInfo );

check_module_info( #module_info{ unhandled_forms=UnhandledForms } ) ->

	Forms = [ F || { _Loc, F } <- UnhandledForms ],

	raise_error( { unhandled_forms, Forms } ).



% Helper to check module parsed attributes.
%
check_module_parse( #module_info{
						 parse_attributes=ParseAttributes,
						 parse_attribute_defs=ParseAttributeDefs } ) ->

	Len = length( ParseAttributes ),

	case length( ParseAttributeDefs ) of

		Len ->
			ok;

		_ ->
			raise_error( { parse_attribute_mismatch, ParseAttributes,
						   ParseAttributeDefs } )

	end.


% Helper to check module includes.
%
check_module_include( #module_info{
						 includes=Includes,
						 include_defs=IncludeDefs } ) ->

	Len = length( Includes ),

	case length( IncludeDefs ) of

		% Includes are filtered (ex: for duplicates):
		L when L < Len ->
			raise_error( { include_mismatch, Includes,
						   IncludeDefs } );

		_ ->
			ok

	end.


% Helper to check module type definitions.
%
check_module_type_definition( #module_info{
								 type_definitions=TypeDefs,
								 type_definition_defs=TypeDefsDefs } ) ->

	Len = length( TypeDefs ),

	case length( TypeDefsDefs ) of

		Len ->
			ok;

		_ ->
			raise_error( { type_definition_mismatch, TypeDefs,
						   TypeDefsDefs } )

	end.


% Helper to check module type exports.
%
check_module_export( #module_info{
						 type_exports=TypeExports,
						 type_export_defs=TypeExportDefs } ) ->

	Len = length( TypeExports ),

	case length( TypeExportDefs ) of

		% A single export attribute can export monre than one type:
		%
		L when L > Len ->
			raise_error( { type_export_mismatch, TypeExports,
						   TypeExportDefs } );

		_ ->
			ok

	end.



% Helper to check module functions.
%
check_module_functions( #module_info{ functions=Functions } ) ->

	FunInfos = ?table:enumerate( Functions ),

	[ check_function( FunId, FunInfo ) || { FunId, FunInfo } <- FunInfos ].



% Nothing to check for 'spec' or 'exported':
%
check_function( FunId, _FunInfo=#function_info{ definition=[] } ) ->
	raise_error( { no_definition_found_for, FunId } );

check_function( _FunId={ Name, Arity }, _FunInfo=#function_info{
													name=Name,
													arity=Arity } ) ->
	% Match:
	ok;

check_function( FunId, _FunInfo=#function_info{
								   name=SecondName,
								   arity=SecondArity } ) ->
	raise_error( { definition_mismatch, FunId,
				   { SecondName, SecondArity } } ).



% Returns a textual description of specified module information.
%
% Note: the location information is dropped for all located definitions.
%
-spec module_info_to_string( module_info() ) -> text_utils:ustring().
module_info_to_string( #module_info{
						 module=Module,
						 module_def={ _, ModuleDef },
						 compilation_option_defs=CompileOptDefs,
						 parse_attributes=ParseAttributes,
						 parse_attribute_defs=ParseAttributeDefs,
						 includes=Includes,
						 include_defs=IncludeDefs,
						 type_definitions=TypeDefs,
						 type_definition_defs=TypeDefsDefs,
						 type_exports=TypeExports,
						 type_export_defs=TypeExportDefs,
						 function_exports=FunctionExports,
						 functions=Functions,
						 last_line=LastLine,
						 unhandled_forms=UnhandledForms
						} ) ->

	FunctionStrings = [ io_lib:format( "~s",
									   [ function_info_to_string( Info ) ] )
						|| { _FunId, Info } <- ?table:enumerate( Functions ) ],

	LastLineString = case LastLine of

		undefined ->
			"unknown";

		{ _Loc, { eof, Count } } ->
			text_utils:format( "~B", [ Count ] )

	end,

	% To mark an additional offset for the sublists:
	Bullet = "   * ",

	UnhandledString = case UnhandledForms of

		[] ->
			"(no unhandled form)";

		_ ->
			UnhandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			text_utils:format( "~B unhandled forms: ~s",
							   [ length( UnhandledForms ),
								 text_utils:strings_to_string( UnhandledStrings,
															   Bullet ) ] )
	end,

	Infos = [

			  text_utils:format( "module: ~p~n", [ Module ] ),
			  text_utils:format( "module definition: ~p~n", [ ModuleDef ] ),

			  text_utils:format( "~B compile option definitions: ~p~n",
								 [ length( CompileOptDefs ),
								   [ C || { _, C } <- CompileOptDefs ] ] ),

			  text_utils:format( "~B parse attributes: ~p~n",
								 [ length( ParseAttributes ),
								   ParseAttributes ] ),

			  text_utils:format( "parse attribute definitions: ~p~n",
								 [ [ P || { _, P } <- ParseAttributeDefs ] ] ),

			  text_utils:format( "~B actual includes: ~p~n",
								 [ length( Includes ), Includes ] ),

			  text_utils:format( "include definitions: ~p~n",
								 [ [ I || { _, I } <- IncludeDefs ] ] ),

			  text_utils:format( "~B type definitions: ~p~n",
								 [ length( TypeDefs ), TypeDefs ] ),

			  text_utils:format( "type definitions: ~p~n",
								 [ [ T || { _, T } <- TypeDefsDefs ] ] ),

			  text_utils:format( "~B type exports: ~p~n",
								 [ length( TypeExports ), TypeExports ] ),

			  text_utils:format( "type export definitions: ~p~n",
								 [ [ E || { _, E } <- TypeExportDefs ] ] ),

			  text_utils:format( "~B function export definitions: ~p~n",
								 [ length( FunctionExports ),
								   [ F || { _, F } <- FunctionExports ] ] ),

			  text_utils:format( "~B functions: ~s~n",
								 [ length( FunctionStrings ),
								   text_utils:strings_to_string(
									 FunctionStrings, Bullet ) ] ),

			  text_utils:format( "line count: ~s~n", [ LastLineString ] ),

			  UnhandledString

			  ],

	text_utils:format( "Information about module '~s':~n~s",
					   [ Module, text_utils:strings_to_string( Infos ) ] ).




% Writes specified AST into specified (text) file.
%
% Useful for example to determine differences between ASTs.
%
-spec write_ast_to_file( ast(), file_utils:file_name() ) -> basic_utils:void().
write_ast_to_file( AST, Filename ) ->

	% Note: we cannot actually use file_utils, which is not a prerequisite of
	% the 'Common' parse transform:

	% We overwrite any pre-existing file:
	{ ok, File } = file:open( Filename, [ write, raw ] ),

	[ ok = file:write( File, io_lib:format( "~p~n", [ F ] )  ) || F <- AST ],

	ok = file:close( File ).



% Raises a (compile-time, rather ad hoc) error when applying a parse transform,
% to stop the build on failure and report the actual error.
%
% Used to be a simple throw, but then for parse transforms the error message was
% garbled in messages like:
%
% """
% internal error in lint_module;
% crash reason: function_clause
%
%  in function  erl_lint:'-compiler_options/1-lc$^0/1-0-'/1
%     called as erl_lint:'-compiler_options/1-lc$^0/1-0-'({
% table_type_defined_more_than_once,{line,12},foo_hashtable,bar_hashtable})
%
% See also: raise_parse_error/ for a better, more standard system for error
% management.
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->

	%throw( ErrorTerm )
	%io:format( "~n~n*** Error:~n~p.~n", [ ErrorTerm ] ),

	% Does not add any information (just non-relevant erl_parse, epp
	% etc. state):
	%
	%erlang:exit( { ErrorTerm, erlang:get_stacktrace() } ).

	erlang:exit( ErrorTerm ).



% Returns an AST form in order to raise a (compile-time, standard) error when
% applying a parse transform, to stop the build on failure and report the actual
% error.
%
% The specified error term will be transformed by the specified module into a
% (textual) error message (see format_error/1), and then will be reported as
% originating from the specified line in the source file of the module being
% compiled.
%
-spec get_error_form( basic_utils:error_reason(), basic_utils:module_name(),
					  line() ) -> form().
get_error_form( ErrorTerm, FormatErrorModule, Line ) ->

	% Actually the most standard way of reporting an error seems to insert a
	% dedicated form in the AST.

	% May ultimately report (thanks to ?MODULE:format_error/1), when compiling a
	% foobar module and if:
	%
	% - Line is 15
	%
	% - 'apply( FormatErrorModule, format_error, [ ErrorTerm ] )' is "my error
	% message":
	%
	% the following error message: "foobar:15: my error message".
	%
	{ error, { Line, FormatErrorModule, ErrorTerm } }.



% This function (whose name is standard, conventional) is to be defined on a
% per-module basis (typically in the module defining the parse transform being
% applied) and allows to convert error terms (that are, here, related to
% parse-transforms) into textual messages that can output by the build chain.
%
-spec format_error( basic_utils:error_reason() ) -> string().
format_error( ErrorTerm ) ->

	% Of course this is just an example:
	%
	text_utils:format( "my meta_utils error reported: ~s", [ ErrorTerm ] ).



% Interprets specified list of issue reports.
%
-spec interpret_issue_reports( [ issue_report() ] ) -> basic_utils:void().
interpret_issue_reports( _IssueReports=[] ) ->
	% Should never happen:
	io:format( "(no remark emitted)" );

% No need to further special-case the number of issue reports, as it is not
% meaningful (one may include an arbitrary long list):

%interpret_issue_reports( _IssueReports=[ OneIssueReport ] ) ->
%	interpret_issue_report( OneIssueReport );

interpret_issue_reports( IssueReports ) ->

	[ interpret_issue_report( R ) || R <- IssueReports ].

	%text_utils:format( "~B remarks: ~s", [ length( IssueReports ),
	%					text_utils:strings_to_string( ReportStrings ) ] ).


% Interprets specific issue report.
%
-spec interpret_issue_report( issue_report() ) -> basic_utils:void().
interpret_issue_report( _IssueReport={ Filename, IssueInfos } ) ->

	% We could normalise it instead, yet file_utils would become a dependency:
	CanonicFilename = filename:basename( Filename ),

	[ interpret_issue_info( CanonicFilename, E ) || E <- IssueInfos ].

	%text_utils:format( "in file '~s': ~s", [ CanonicFilename,
	%		   text_utils:strings_to_string( IssueStrings ) ] ).



% Interprets specific error description.
%
-spec interpret_issue_info( file_utils:file_name(), issue_info() ) ->
								  basic_utils:void().
interpret_issue_info( Filename,
					  _IssueInfo={ Line, DetectorModule, IssueDesc } ) ->

	% Module is the detecting one, typically erl_lint:
	%text_utils:format( "line #~B, module '~p', ~s", [ Line, Module,
	%						interpret_issue_description( IssueDesc ) ] ).

	%text_utils:format( "line #~B: ~s", [ Line,
	%		interpret_issue_description( IssueDesc, DetectorModule ) ] ).

	io:format( "~s:~B: ~s~n", [ Filename, Line,
			interpret_issue_description( IssueDesc, DetectorModule ) ] ).



% Interprets specific issue description, detected by specified module.
%
% Note: full control is offered here to enrich this function at will, if wanted.
%
-spec interpret_issue_description( issue_description(),
								   basic_utils:module_name() ) -> string().
interpret_issue_description( IssueDescription, DectectorModule ) ->
	%For example, the detector module may be erl_lint:
	DectectorModule:format_error( IssueDescription ).



% Lists (in the order of their definition) all the functions ({Name,Arity}) that
% are exported by the specified module, expected to be found in the code path.
%
-spec list_exported_functions( basic_utils:module_name() ) ->
									 [ function_id() ].
list_exported_functions( ModuleName ) ->

	% To avoid a unclear message like 'undefined function XXX:module_info/1':
	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			throw( { module_not_found_in_path, ModuleName } );

		_ ->
			ok

	end,

	ModuleName:module_info( exports ).



% Returns a list of the arities for which the specified function of the
% specified module is exported.
%
-spec get_arities_for( basic_utils:module_name(), function_name() ) ->
							 [ arity() ].
get_arities_for( ModuleName, FunctionName ) ->

	ExportedFuns = list_exported_functions( ModuleName ),

	% Match on FunctionName:
	[ Arity || { Name, Arity } <- ExportedFuns, Name =:= FunctionName ].



% Tells whether the specified function (name with arity) is exported by the
% specified module.
%
-spec is_function_exported( basic_utils:module_name(), function_name(),
							arity() ) -> boolean().
is_function_exported( ModuleName, FunctionName, Arity ) ->
	lists:member( { FunctionName, Arity },
				  list_exported_functions( ModuleName ) ).



% Checks whether a potential upcoming call to the specified MFA
% (Module,Function,Arguments) has a chance of succeeding.
%
-spec check_potential_call( basic_utils:module_name(), function_name(),
		[ basic_utils:argument() ] ) ->
				'ok' | 'module_not_found' | 'function_not_exported'.
check_potential_call( ModuleName, FunctionName, Arguments )
  when is_atom( ModuleName ) andalso is_atom( FunctionName )
	   andalso is_list( Arguments ) ->

	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			module_not_found;

		_ ->
			Arity = length( Arguments ),
			case is_function_exported( ModuleName, FunctionName, Arity ) of

				true ->
					ok;

				false ->
					function_not_exported

			end

	end;

check_potential_call( ModuleName, FunctionName, Arguments ) ->

	case is_atom( ModuleName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_module_name, ModuleName } )

	end,

	case is_atom( FunctionName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_function_name, FunctionName } )

	end,

	% Only remaining possibility:
	throw( { non_list_arguments, Arguments } ).



% Type-related section.
%
% Note: currently, only a very basic, ad hoc support ("hand-made look-up
% tables") is provided.
%
% Later we would like to really parse any type description (ex: "[ { float, [
% boolean ] } ]") and be able to manage it as type() (including the checking of
% terms against types).



% Returns the actual type corresponding to specified type description: parses
% the specified string to determine the type described therein.
%
% Note: returns a correct type, but currently rarely the expected, most precise
% one.
%
-spec description_to_type( type_description() ) -> type().
description_to_type( TypeDescription ) ->

	CanonicalDesc = text_utils:remove_whitespaces( TypeDescription ),

	%io:format( "CanonicalDesc = '~s'~n", [ CanonicalDesc ] ),

	scan_type( CanonicalDesc ).



% To perfom its parsing, we must split the full description recursively.
%
% The worst (and thus first) top-level construct to detect is the union. We
% consider that we are always in an union (possibly including only one term, in
% which case it can be simplified out.
%
% We do that by scanning for terms from left-to-right, keeping track of the
% nesting.
%
%-spec scan_type( type_description() ) -> type().
%scan_type( TypeDescription ) ->
	%case tokenise_per_union( TypeDescription ) of

	%	[ T ] ->
	%		T;

	%	UnionisedTypes ->
	%		{ union, [ scan_type( T ) || T <- UnionisedTypes ] }

	%end.

% Last: all other types.
%
scan_type( _TypeDescription ) ->
	% Most imprecise (yet correct) type (commented-out as may hide issues):
	any.

	% Either not yet implemented or plain wrong:
	%throw( { type_interpretation_failed, TypeDescription } ).


% Splits the specified type description according to union delimiters
%
-spec tokenise_per_union( type_description() ) -> [ type_description() ].
tokenise_per_union( TypeDescription ) ->

	% We track the nesting depth and only fetch the top-level union members;
	%
	InitialNestingDepth = { _P=0, _B=0 },
	parse_nesting( TypeDescription, InitialNestingDepth ).



% Parses the specified type description in order to split it according in nested
% sub-expressions that may be recursively parsed.
%
-spec parse_nesting( type_description(), nesting_depth() ) ->
						   [ type_description() ].
parse_nesting( _TypeDescription, _NestingDepth ) ->

	% A goal is to detect atoms delimited with single quotes (which are
	% immediate atom values) from the unquoted ones (which designate types)
	%
	throw( not_implemented_yet ).





% Returns the type description (in canonical form, notably without whitespaces)
% corresponding to specified type.
%
% Note: currently does not return a really relevant type description; basically
% meant to be the function reciprocal to scan_type/1.
%
-spec type_to_description( type() ) -> type_description().
% First, simple types, in alphabetical order:
type_to_description( _Type=atom ) ->
	"atom";

type_to_description( _Type=integer ) ->
	"integer";

type_to_description( _Type=float ) ->
	"float";

type_to_description( _Type=boolean ) ->
	"boolean";

type_to_description( _Type=string ) ->
	"string";

type_to_description( _Type=any ) ->
	"any";

type_to_description( _Type=none ) ->
	"none";


% Then polymorphic constructs:


% No "list()"-like (with no specific type) supported.

type_to_description( _Type={ list, T } ) ->
	"[" ++ type_to_description( T ) ++ "]";

type_to_description( _Type={ union, TypeList } ) when is_list( TypeList ) ->
	text_utils:join( _Separator="|",
					 [ type_to_description( T ) || T <- TypeList ] );

type_to_description( _Type={ tuple, TypeList } ) when is_list( TypeList ) ->
	TypeString = text_utils:join( _Separator=",",
					  [ type_to_description( T ) || T <- TypeList ] ),
	"{" ++ TypeString ++ "}";

type_to_description( _Type={ table, [ Tk, Tv ] } ) ->
	"table(" ++ type_to_description( Tk ) ++ "," ++ type_to_description( Tv )
		++ ")";


type_to_description( Type ) ->

	% Could be misleading (ex: any() not matching any()):
	%"any".

	%text_utils:format( "~p", [ Type ] ).

	throw( { type_description_failed, Type } ).



% Returns a textual representation of the specified type.
%
-spec type_to_string( type() ) -> string().
type_to_string( Type ) ->
	type_to_description( Type ).



% Returns an atom describing, as precisely as possible, the overall type of the
% specified primitive term.
%
% Note: limited to primitive types, not compounded ones (like [float()]).
%
% is_number/1, is_record/1, etc. not usable here.
%
% Note: often we do not want to retrieve the actual type of a term but need
% instead to determine whether the term can be considered as an instance of a
% specific type (this is not strictly the same need, as a given term in general
% may be seen of being of multiple types).
%
-spec get_type_of( term() ) -> primitive_type_description().
get_type_of( Term ) when is_boolean( Term ) ->
	'boolean';

get_type_of( Term ) when is_atom( Term ) ->
	'atom';

get_type_of( Term ) when is_binary( Term ) ->
	'binary';

get_type_of( Term ) when is_float( Term ) ->
	'float';

get_type_of( Term ) when is_function( Term ) ->
	'function';

get_type_of( Term ) when is_integer( Term ) ->
	'integer';

get_type_of( Term ) when is_pid( Term ) ->
	'pid';

get_type_of( Term ) when is_list( Term ) ->
	case text_utils:is_string( Term ) of

		true ->
			'string';

		false ->
			case text_utils:is_list_of_strings( Term ) of

				true ->
					'[string]';

				false ->
					'list'

			end

	end;

get_type_of( Term ) when is_port( Term ) ->
	'port';

%get_type_of( Term ) when is_record( Term ) ->
%	'record';

get_type_of( Term ) when is_tuple( Term ) ->
	'tuple';

get_type_of( Term ) when is_reference( Term ) ->
	'reference'.



% Returns a list of the elementary, "atomic" types.
%
-spec get_elementary_types() -> [ type_name() ].
get_elementary_types() ->
	[ 'atom', 'binary', 'boolean', 'float', 'function', 'integer', 'list',
	  'pid', 'port', 'record', 'reference', 'tuple' ].



% Tells whether specified term designates a type (i.e. a type() instance).
%
% (only the elementary types are currently recognised)
%
-spec is_type( term() ) -> boolean().
%is_type( { Tag, SubTypes } ) when is_list( SubTypes ) ->
%	lists:member( Tag, get_elementary_types() );
%
%is_type( _T ) ->
%	false.

% To be implemented:
is_type( _T ) ->
	true.



% Tells whether specified term is of specified type (predicate).
%
% Note: currently only a very partial checking is made, based on top-level
% primitive types; later the type will be recursed into, in order to check
% whether the term complies with this expected structure.
%
-spec is_of_type( term(), type() ) -> boolean().
is_of_type( _Term, _Type='any' ) ->
	true;

is_of_type( Term, _Type='string' ) when is_list( Term ) ->
	text_utils:is_string( Term );

is_of_type( Term, Type ) ->

	case get_type_of( Term ) of

		Type ->
			true;

		_ ->
			false

	end.



% Tells whether the specified term is of specified textually-described type.
%
% Note: currently no checking is made and the test always succeeds.
%
-spec is_of_described_type( term(), type_description() ) -> boolean().
is_of_described_type( _Term, _TypeDescription ) ->

	%throw( { not_implemented_yet, {is_of_described_type,2} } ).

	% ActualType = description_to_type( TypeDescription ),
	% is_of_type( ActualType ).

	true.



% Tells whether specified non-empty container (list or tuple) is homogeneous in
% terms of type, i.e. whether all its elements are of the same type.
%
% If true, returns the common type.
% If false, returns two of the different types found in the container.
%
-spec is_homogeneous( list() | tuple() ) ->
		{ 'true', primitive_type_description() } | { 'false',
			 { primitive_type_description(), primitive_type_description() } }.
is_homogeneous( _List=[] ) ->
	% We want to return types:
	throw( empty_container );

is_homogeneous( _List=[ H | T ] ) ->

	Type = get_type_of( H ),

	is_homogeneous_full_helper( T, Type );

is_homogeneous( Tuple ) when is_tuple( Tuple ) ->

	ElemList = tuple_to_list( Tuple ),

	is_homogeneous( ElemList ).



% Tells whether specified non-empty container (list or tuple) is homogeneous in
% terms of type, i.e. whether all its elements are of the same, specified,
% primitive type.
%
-spec is_homogeneous( list() | tuple(), primitive_type_description() ) ->
							boolean().
is_homogeneous( _List=[], _Type ) ->
	% Considered homogeneous:
	true;

is_homogeneous( List, Type ) when is_list( List ) ->
	is_homogeneous_helper( List, Type );

is_homogeneous( Tuple, Type ) when is_tuple( Tuple ) ->

	ElemList = tuple_to_list( Tuple ),

	is_homogeneous_helper( ElemList, Type ).


% Helper:
is_homogeneous_full_helper( _Elems=[], Type ) ->
	{ true, Type };

is_homogeneous_full_helper( _Elems=[ H | T ], Type ) ->

	case get_type_of( H ) of

		Type ->
			is_homogeneous_full_helper( T, Type );

		OtherType ->
			{ false, { Type, OtherType } }

	end.


% Other helper:
is_homogeneous_helper( Elems, Type ) ->
	{ Bool, _TypeInfo } = is_homogeneous_full_helper( Elems, Type ),
	Bool.



% Tells whether the two specified types are the same (i.e. designate the same
% actual type, are aliases).
%
-spec are_types_identical( type(), type() ) -> boolean().
are_types_identical( Type, Type ) ->
	true;

are_types_identical( _FirstType, _SecondType ) ->
	false.
