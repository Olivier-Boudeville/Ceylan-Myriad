% Copyright (C) 2015-2018 Olivier Boudeville
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
% Creation date: Friday, December 19, 2014.



% This module allows to test with as few dependencies as possible the
% 'common_parse_transform' parse transform as a standalone unit, hence with
% proper error and warning messages.
%
% See the common_parse_transform.erl tested module.
%
-module(minimal_parse_transform_test).


-export([ run/0, perform_direct_ast_operations/1 ]).


perform_direct_ast_operations( TargetSourceFile ) ->

	io:format( "~nNow performing directly AST-level operations:~n~n" ),

	BaseAST = meta_utils:erl_to_ast( TargetSourceFile ),

	io:format( "Base AST:~n~p~n", [ BaseAST ] ),

	BaseModuleInfo = meta_utils:extract_module_info_from_ast( BaseAST ),

	io:format( "Base module info: ~s~n~n",
			   [ meta_utils:module_info_to_string( BaseModuleInfo ) ] ),

	FinalModuleInfo = BaseModuleInfo,

	io:format( "Final module info: ~s~n~n",
			   [ meta_utils:module_info_to_string( FinalModuleInfo ) ] ),

	FinalAST = meta_utils:recompose_ast_from_module_info( FinalModuleInfo ),

	io:format( "Final AST:~n~p~n", [ FinalAST ] ).



-spec run() -> no_return().
run() ->

	%TargetSourceFile = "code_utils.erl",
	TargetSourceFile = "simple_parse_transform_target.erl",
	%TargetSourceFile = "../data-management/preferences.erl",

	io:format( "Applying the common parse transform to the "
			   "'~s' source file.~n~n", [ TargetSourceFile ] ),

	TransformedAST = common_parse_transform:run_standalone( TargetSourceFile ),

	io:format( "Transformed AST:~n~p~n~n", [ TransformedAST ] ),

	%ast_utils:write_ast_to_file( TransformedAST, TargetSourceFile ++ ".ast" ),

	%perform_direct_ast_operations( TargetSourceFile ),

	init:stop( _StatusCode=0 ).
