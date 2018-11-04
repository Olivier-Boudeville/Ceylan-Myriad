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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, February 4, 2018



% Module in charge of generating parts of an AST (ex: elements of forms).
%
-module(ast_generation).


-export([ get_iterated_param_name/1, get_header_params/1, list_variables/1 ]).



% Shorthands:
-type form_element() :: ast_base:form_element().


% Returns, in AST form, a reference to an iterated variable.
%
% Ex: 'Myriad_Param_4' = get_iterated_param_name( 4 ).
%
-spec get_iterated_param_name( basic_utils:count() ) -> atom().
get_iterated_param_name( Count ) ->
	String = text_utils:format( "Myriad_Param_~B", [ Count ] ),
	text_utils:string_to_atom( String ).




% Returns, as form elements, conventional call parameter names, as form
% elements, corresponding to a function of specified arity.
%
% This is typically useful when generating a function form, to define its
% header, like in 'f(A,B)->...'.
%
% Ex: [ {var,Line,'A'}, {var,Line,'B'} ] = get_header_params( 2 ).
%
-spec get_header_params( arity() ) -> [ form_element() ].
get_header_params( Arity ) ->
	get_header_params( Arity, _Acc=[] ).


get_header_params( _Arity=0, Acc ) ->
	Acc;

get_header_params( Arity, Acc ) ->
	NewAcc = [ { var, _Line=0, get_iterated_param_name( Arity ) } | Acc ],
	get_header_params( Arity-1, NewAcc ).



% Returns the form element corresponding a list of variables.
%
% Ex: [ { cons,Line,{var,Line,'A'}, { cons,Line,{var,Line,'B'}, {nil,Line}} } ]
% = list_variables( 2 ).
%
-spec list_variables( basic_utils:count() ) -> form_element().
list_variables( Count ) ->
	list_variables( Count, _Index=1 ).


list_variables( _Count=0, _Index ) ->
	{ nil, _Line=0 };

list_variables( Count, Index ) ->
	Line = 0,
	{ cons, Line, { var, Line, get_iterated_param_name( Index ) },
	  list_variables( Count-1, Index+1 ) }.
