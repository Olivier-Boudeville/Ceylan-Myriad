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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Tuesday, December 25, 2018



% Management of conditional compilation, like generalized, limitation-less
% macros.
%
-module(cond_utils).


-export([ get_token_table_from/1, if_defined/1, if_defined/2 ]).


% For the table macro:
-include("meta_utils.hrl").


% A token (defined either through a -D command-line option or through an
% in-source compile attribute), whose definition enables the conditional
% execution of associated code.
%
% Ex: a 'debug_gui' token would enable, if defined, associated code, like in:
% cond_utils:if_defined( debug_gui, [ f(), A = B, g( C ) ] )
%
-type token() :: atom().


% An expression that is conditionally enabled:
-type expression() :: any().

-type expressions() :: expression() | [ expression() ].


% Table to establish easily whether a token has been defined and, if yes, a
% value (if any; otherwise it is set to 'undefined') that has been associated to
% it.
%
-type token_table() :: ?table:?table( token(), basic_utils:maybe( term() ) ).

-export_type([ token/0, expression/0, expressions/0, token_table/0 ]).


% Shorthand:
-type void() :: basic_utils:void().



% Implementation notes:
%
% About tokens
%
% There are to be specified as command-line build options, typically thanks to
% the ERLANG_COMPILER_TOKEN_OPT make variable, defined in GNUmakevars.inc.
%
% For example, if defining that variable as:
% ERLANG_COMPILER_TOKEN_OPT := -Dmy_test_token -Dmy_other_test_token=51
%
% then a parse-transform (ex: the Myriad one) would be automatically called as:
%     parse_transform( InputAST, Options ) -> ...
%
% and this option list would contain, among other elements:
%         {d,my_test_token},
%         {d,my_other_test_token,51}

% (the atom 'd' must mean 'define' in this compilation context)
%
% Then, in addition to the standard macros such as:
%
% -ifdef(my_test_token).
%  [...]
% -endif
%
% -if(?my_other_test_token >= 22).
%  [...]
% -endif
%
% the parse transform is also able to take into account these information.



% Returns the tokens declared among the compile options.
%
-spec get_token_table_from( ast_info:compile_option_table() ) ->
								  token_table().
get_token_table_from( OptionTable ) ->

	% The 'd' compile option must correspond to the compilation defines:
	case ?table:lookupEntry( _K='d', OptionTable ) of

		% Ex: L=[my_test_token,{my_other_test_token,51}]
		{ value, L } ->
			% Returns a filled table:
			register_tokens( L, ?table:new() );

		key_not_found ->
			%  Empty table then, no token available:
			?table:new()

	end.


% (helper)
register_tokens( _L=[], TokenTable ) ->
	TokenTable;

register_tokens( _L=[ { Token, Value } | T ], TokenTable ) when is_atom( Token ) ->
	% Crashes if a token is defined more than once (must be abnormal):
	NewTokenTable = ?table:addNewEntry( Token, Value, TokenTable ),
	register_tokens( T, NewTokenTable );

register_tokens( _L=[ Token | T ], TokenTable ) when is_atom( Token ) ->
	% A token without a value is associated to 'undefined':
	NewTokenTable = ?table:addNewEntry( Token, _V=undefined, TokenTable ),
	register_tokens( T, NewTokenTable ).



% Example of transformation:
%
% cond_utils:if_defined( my_token, [ A = 1,
%									 io:format( "Conditional code executed!" ),
%									 B = A + 1 ] ),
%
% would be by default literally translated into:
%
% (line numbers replaced by anonymous mute variables)
%
%  {call,_,
%	  {remote,_,{atom,_,cond_utils},{atom,_,if_defined}},
%	  [{var,_,'Token'},
%	   {cons,_,
%		   {match,_,{var,_,'A'},{integer,_,1}},
%		   {cons,_,
%			   {call,_,
%				   {remote,_,{atom,_,io},{atom,_,format}},
%				   [{string,_,"Conditional code executed!"}]},
%			   {cons,_,
%				   {match,_,
%					   {var,_,'B'},
%					   {op,_,'+',{var,_,'A'},{integer,_,1}}},
%				   {nil,_}}}}]},
%
%
% whereas we want it to become either (should my_token be defined):
%
%  {match,_,{var,_,'A'},{integer,_,1}},
%               {call,_,
%                     {remote,_,{atom,_,io},{atom,_,format}},
%                     [{string,_,"Conditional code executed!~n"}]},
%               {match,_,
%                      {var,_,'B'},
%                      {op,_,'+',{var,_,'A'},{integer,_,1}}},
%
%    (i.e. we "uncons" said expression list)
%
% or, should my_token not be defined: exactly nothing.





% Conditional execution, enabled iff the debug mode has been enabled (i.e. iff
% its token has been specified, either through the command-line or as an
% in-source compile option).
%
-spec if_defined( expressions() ) -> void().
if_defined( Expressions ) ->
	if_defined( _Token=debug_mode, Expressions ).



% Conditional execution, enabled iff the specified token has been specified
% (i.e. iff its token has been specified, either through the command-line or as
% an in-source compile option), in which case the specified expressions are
% injected (otherwise they are simply dismissed as a whole).
%
% Note: the first parameter, Token, must be an immediate value, an atom (not
% even a variable whose value happens to be an atom).
%
% So 'cond_utils:if_defined( hello, [...] )' will be accepted, while even 'A=hello,
% cond_utils:if_defined( A, [...] )' will be rejected.
%
% As for the second parameter, it shall be directly a list of expressions; for
% example 'cond_utils:if_defined( debug_mode, _Exprs=[...])' would be rejected.
%
% Finally, should the relevant token not be defined, the corresponding
% expressions are dismissed, which may lead variables only mentioned in said
% expressions to be reported as unused; for example: 'A = 1,
% cond_utils:if_defined( non_existing_token, [ A = 1, ... ] )' will report that
% variable 'A' is unused.
% 
-spec if_defined( token(), expressions() ) -> void().
if_defined( Token, Expressions ) ->

	% Never expected to be called, as replaced by the Myriad parse transform
	% either by the actual expressions, or by nothing at all:
	%
	throw( { untransformed_conditional, Token, Expressions } ).
