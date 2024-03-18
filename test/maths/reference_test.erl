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

	% Target tree (identifiers added with '#'):
	%
	% frame 'Root' (#0)
	%   - frame 'Rs' (#1)
	%     - frame 'Rc' (#4)
	%     - frame 'Rb' (#3)
	%       - frame 'Re' (#5)
	%     - frame 'Ra' (#2)
	%       - frame 'Rf' (#6)
	%         - frame #8
	%         - frame 'Rg' (#7)


	BlankRefTree = reference_tree:new(),

	test_facilities:display( "Created a first ~ts.",
							 [ reference_tree:to_string( BlankRefTree ) ] ),


	% Rs corresponds to the root frame of reference (but has a name):
	Rs = reference_frame3:new_absolute( _SName="Rs" ),

	{ RsId, WithRsTree } = reference_tree:register( Rs, BlankRefTree ),



	% Ra here is just a frame translated, relatively to the absolute frame, of:
	Va = [ 5, 0, 0 ],
	Transfa = transform4:translation( Va ),

	% Absolutely defined (no parent):
	Ra = reference_frame3:new( _AName="Ra", Transfa, RsId ),

	test_facilities:display( "Created Ra: ~ts",
							 [ reference_frame3:to_string( Ra ) ] ),


	% Rb here is just a frame rotated, relatively to the absolute frame, based
	% on:
	%
	UnitAxisRotb = vector3:normalise( [ 1, 2, 3 ] ),
	AngleRotb = math_utils:degrees_to_radians( 41 ),
	Transfb = transform4:rotation( UnitAxisRotb, AngleRotb ),

	% No parent:
	Rb = reference_frame3:new( _BName="Rb", Transfb, RsId ),

	test_facilities:display( "Created Rb: ~ts",
							 [ reference_frame3:to_string( Rb ) ] ),


	% Rc here is just a frame scaled, relatively to the absolute frame, of:
	Factorsc = { 1.0, 1.1, 0.8 },
	Transfc = transform4:scaling( Factorsc ),

	% No parent, thus absolutely defined:
	Rc = reference_frame3:new( _CName="Rc", Transfc, RsId ),

	test_facilities:display( "Created Rc: ~ts",
							 [ reference_frame3:to_string( Rc ) ] ),


	{ _WithRabcRefIds=[ RaId, RbId, _RcId ], WithRabcTree } =
		reference_tree:register( _WithRabcRefs=[ Ra, Rb, Rc ], WithRsTree ),

	% Re is more complex here:
	%
	% - its transformation is scaling (could be allowed here at it is a leaf of
	% the tree, hence such scaling will not be inherited), then rotation, then
	% translation (order matters)
	%
	% - it is defined relatively to a non-root parent reference frame, Rb

	Transfe = transform4:sc_rot_tr( _ScaleFactors={ 1.0, 1.2, 1.1 },
		_RotAxis=[ 1, 0, 0], _RotAngle=math_utils:pi()/4,
		_TrV=[ 15, -3, 100 ] ),

	% Anonymous:
	Re = reference_frame3:new( _EName="Re", Transfe, RbId ),

	{ _ReId, WithReTree } = reference_tree:register( Re, WithRabcTree ),

	test_facilities:display( "With Re: ~ts.",
							 [ reference_tree:to_string( WithReTree ) ] ),

	Transff = transform4:identity(),
	Rf = reference_frame3:new( "Rf", Transff, RaId ),

	{ RfId, WithRfTree } = reference_tree:register( Rf, WithReTree ),

	Transfg = transform4:transition( _Origin={5,5,5},
									 _X=[0,1,0], _Y=[-1,0,0], _Z=[0,0,-1] ),

	Rg = reference_frame3:new( "Rg", Transfg, RfId ),

	Transfh = transform4:translation( _Vh=[ 10, 20,-5.2 ] ),

	% Anonymous:
	Rh = reference_frame3:new( Transfh, RfId ),

	{ _WithRghRefIds=[ _RgId, _RhId ], WithRghTree } =
		reference_tree:register( _WithRghRefs=[ Rg, Rh ], WithRfTree ),

	test_facilities:display( "Reference tree with Rg and Rh: ~ts~n",
		[ reference_tree:to_string( WithRghTree, _VerbLevel=high ) ] ),


	PathedRefId = RfId,

	WithRghRefTable = reference_tree:get_reference_table( WithRghTree ),

	{ IdPath, PathedRefTable } =
		reference_tree:get_path_from_root( PathedRefId, WithRghRefTable ),

	test_facilities:display( "Identifier path from root node to frame #~B: ~w.",
							 [ PathedRefId, IdPath ] ),

	ExpectedIdPath = [ RsId, RaId ],
	ExpectedIdPath = IdPath,

	FinalTree = reference_tree:set_reference_table( PathedRefTable,
													WithRghTree ),

	test_facilities:display( "Full view of this ~ts",
							 [ reference_tree:to_full_string( FinalTree ) ] ),

	reference_tree:check( FinalTree ),

	test_facilities:stop().
