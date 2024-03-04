% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Saturday, March 2, 2024.


% @doc Unit tests for the <b>management of reference frames and trees</b>.
%
% See the reference_{frame,frame3,tree} tested modules.
%
-module(reference_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing reference frames and trees; we recreate "
		"here a version of "
		"https://howtos.esperide.org/reference-frame-tree.png." ),

	% Rs is the implicit root frame of reference.

	% Ra here is just a frame translated, relatively to the absolute frame, of:
	Va = [ 5, 0, 0 ],
	Transfa = transform4:translation( Va ),

	% No parent, thus absolutely defined:
	Ra = reference_frame3:new( Transfa ),

	test_facilities:display( "Created Ra: ~ts",
							 [ reference_frame3:to_string( Ra ) ] ),


	% Rb here is just a frame rotated, relatively to the absolute frame, based
	% on:
	%
	UnitAxisRotb = vector3:normalise( [ 1, 2, 3 ] ),
	AngleRotb = math_utils:degrees_to_radians( 41 ),
	Transfb = transform4:rotation( UnitAxisRotb, AngleRotb ),

	% No parent:
	Rb = reference_frame3:new( Transfb ),

	test_facilities:display( "Created Rb: ~ts",
							 [ reference_frame3:to_string( Rb ) ] ),


	% Rc here is just a frame scaled, relatively to the absolute frame, of:
	Factorsc = { 1.0, 1.1, 0.8 },
	Transfc = transform4:scaling( Factorsc ),

	% No parent, thus absolutely defined:
	Rc = reference_frame3:new( Transfc ),

	test_facilities:display( "Created Rc: ~ts",
							 [ reference_frame3:to_string( Rc ) ] ),


	BlankRefTree = reference_tree:new(),

	test_facilities:display( "Created ~ts",
							 [ reference_tree:to_string( BlankRefTree ) ] ),

	{ _RefIds=[ _RaId, RbId, _RcId ], FirstRefTree } =
		reference_tree:register( _Refs=[ Ra, Rb, Rc ], BlankRefTree ),

	% Re is more complex here:
	%
	% - its transformation is scaling (allowed at it is a leaf of the tree,
	% hence such scaling will not be inherited), then rotation, then translation
	% (order matters)
	%
	% - it is defined relatively to a non-root parent reference frame, Rb

	Transfe = transform4:sc_rot_tr( _ScaleFactors={ 1.0, 1.2, 1.1 },
		_RotAxis=[ 1, 0, 0], _RotAngle=math_utils:pi()/4,
		_TrV=[ 15, -3, 100 ] ),

	Re = reference_frame3:new( Transfe, RbId ),

	{ _ReId, WithReTree } = reference_tree:register( Re, FirstRefTree ),

	test_facilities:display( "With Re: ~ts",
							 [ reference_tree:to_string( WithReTree ) ] ),

	test_facilities:stop().
