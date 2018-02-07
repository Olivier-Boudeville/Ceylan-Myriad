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



% Module in charge of handling records defined or used within an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_record).



% Field name, possibly '_':
-type field_id() :: atom().

-type ast_field_id() :: ast_base:ast_atom().


% AST definition for a field of a record, when creating it.
%
-type ast_creation_record_field() ::
		ast_creation_record_field( ast_base:ast_element() ).



% AST definition for a field of a record, when creating it.
%
% {record_field,LINE,Rep(Field_1),Rep(ValueType)}
%
-type ast_creation_record_field( ValueType ) ::
		{ 'record_field', line(), ast_field_id(), ValueType }.


-export_type([ field_id/0, ast_field_id/0,
			   ast_creation_record_field/0, ast_creation_record_field/1 ]).

-export([ transform_creation_record_fields/2, 
		  transform_creation_record_fields/3, 
		  transform_creation_record_field/3 ]).



% Shorthands:

-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().



% Transforms specified record fields, at creation.
%
% Note: context-insensitive function, considering that any kind of expression
% can be found for the field value.
%
-spec transform_creation_record_fields( [ ast_creation_record_field() ],
						ast_transforms() ) -> [ ast_creation_record_field() ].
transform_creation_record_fields( RecordFields, Transforms ) ->
	transform_creation_record_fields( RecordFields, Transforms,
							 fun ast_expression:transform_expression/2 ).


% Transforms specified record fields, at creation, applying to each record field
% the specified function to perform the relevant transformations (that depends
% on the context; ex: if being in a guard, in an expression, etc.).
%
-spec transform_creation_record_fields( [ ast_creation_record_field() ],
			ast_transforms(), ast_transform:transform_fun() ) ->
											  [ ast_creation_record_field() ].
transform_creation_record_fields( RecordField, Transforms, TransformFun ) ->
	transform_creation_record_fields( RecordField, Transforms, TransformFun ).


% Transforms specified record field, at creation.
%
% Ex: {record_field,LINE,Rep(Field_k),Rep(Gt_k)}.
%
-spec transform_creation_record_field( ast_creation_record_field(),
			   ast_transforms(), ast_transform:transform_fun() ) ->
											 ast_creation_record_field().
transform_creation_record_field(
  _RF={ record_field, Line, ASTFieldName, ASTValue },
  Transforms, TransformFun ) ->

	% Or an expression?
	ast_type:check_ast_atom( ASTFieldName, Line ),

	NewASTValue = TransformFun( ASTValue, Transforms ),

	{ record_field, Line, ASTFieldName, NewASTValue }.
