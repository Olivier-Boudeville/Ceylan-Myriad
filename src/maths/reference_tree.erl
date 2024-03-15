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


% @doc Module implementing the support for <b>trees of reference frames</b>
% (frames of references).
%
% Such a tree (abbreviated as a "ref tree"; and also known as a "scene graph")
% keeps track of the parent/child relationships between reference frames.
%
% The root of a reference tree corresponds to the identity transform.
%
% The tree holds notably an associative table of reference frames, so that each
% of them can be easily looked up based on its identifier.
%
% We expect mostly transition matrices to be held by a reference tree (e.g. no
% scaling expected), as child frames would inherit them, which may not be
% desirable.
%
% @see reference_frame3 for example
%
-module(reference_tree).


% Implementation notes:
%
% Each node of a reference tree of interest is a reference frame, with a
% "pointer" (an identifier-based reference) to the parent one (if any), and a
% transformation (whose reference matrix is the transition matrix from the node
% to its parent - the directed arrow is their representation, and whose inverse
% one is the transition matrix from the parent to this node). So by design, in
% terms of transition matrices (not reference designators) we can freely
% navigate the branches of the reference tree both ways.
%
% All examples given relate to the following reference tree (R for local
% reference frames, C for cameras):
%
% Root
% ├── R1
% │   ├── C2
% │   └── R2
% │       └── R3
% └── R4
%     └── C1
%
% Let's suppose that we want to use the transition matrix from a reference frame
% R3 (e.g. a local coordinate system of an object) to another C2 (e.g. the one
% of a camera). The general mode of operation is first to climb the reference
% tree from the source (R3), then to go down to reach the target (C2).
%
% The shortest path is to go from R3 to the nearest common parent to R3 and C2,
% namely R1, and then to go from R1 to C2. Pros: most direct computations, less
% numerical errors. Cons: no easy reuse of them.
%
% Another path is to go from R3 to the tree root, and from there to C2. Cons:
% includes possibly many multiplications by a matrix and later by its inverse,
% like here for the transformation between R1 and the root frame (no runtime
% overhead incurred, but possibly more rounding errors). Pros: then, for each
% node, an (optional) transformation to the root frame can be stored. We could
% then obtain the transform from R3 to C2 thanks to only one matrix
% multiplication. The root node would then have to record up to one
% transformation for each of the other nodes.
%
% In both cases we should precompute or cache the paths involved for each pair
% of endpoints, as a triplet made of a "up moves" (climbing upward to the root)
% list and then of "down moves" (going downward from the root), and then any
% already computed transform:
%
% ref_path() :: {UpMoves :: [designated_ref()], DownMoves :: [designated_ref()],
% maybe(transform())}.
%
% For example to reach C2 from R3, the shortest path would be:
% PAs={[R2,R1],[C2]} (meaning R3 -> R2 -> R1 -> C2), while the path through root
% would be: PAr={[R2,R1,Root],[R1,C2]}.
%
% Either way such a path_table({From :: designated_ref(), To ::
% designated_ref()}) -> ref_path() could store the known paths.
%
% When a reference frame would change, it would invalidate all paths involving
% its node or any of its child ones (direct or not). This would a bit complex to
% determine directly, thus the dependency of paths w.r.t to nodes would have to
% be tracked, typically by assigning identifiers to paths and maintaining a
% dedicated table(designated_ref(), [path_id()]) to, whenever a designated frame
% changes, invalidate the corresponding paths easily.
%
% Yet such caching is a bit complex to implement and, at least in some cases
% (e.g. a constantly rotating higher-level frame of reference), this
% optimisation may be counter-productive (invalidating all paths, each one being
% actually used once, caching being then counter-productive).
%
% So, at least for the moment, we rely on a simpler, reference, unoptimised
% dynamic algorithm, relying only on paths passing through the root node and not
% being cached.
%
% Anyway all algorithms may respect the same API, akin to get_transform(From ::
% designated_ref(), To :: designated_ref()) -> transform().

% Note also that the actual reference frames currently supported by ref trees
% are 3D ones (ref3), yet other dimensions could easily be supported.


% Local record:
-record( reference_tree, {

	% The table of this tree concentrating all known reference frames, based on
	% their identifier.
	%
	ref_table :: ref_table(),

	% The identifier to be allocated at the next registered frame:
	next_ref_id = 1 :: ref_id(),

	% A table keeping track of the (direct) children of each frame of reference:
	ref_children :: ref_child_table()

	% A table keeping track of the shortest paths in the tree between two frames
	% of reference.
	%
	%path_table :: reference_frame:path_table(),

} ).

-type reference_tree() :: #reference_tree{}.
% A reference tree, also known as a scene graph.
%
% Note that the root, absolute reference frame is to be designated by the null
% (zero) reference frame identifier.


-define( root_id, 0 ).
% The identifier of the root, absolute reference frame.



-type ref_child_table() :: table( ref_id(), [ ref_id() ] ).
% A table associating to each reference frame a list of its child ones.
%
% All identifiers are contextual to an (implicit) reference table.


-export_type([ reference_tree/0, ref_child_table/0 ]).


-export([ new/0,
		  register/2,
		  to_string/1, to_string/2 ] ).


-include("reference_frame3.hrl").

-compile({ no_auto_import,[register/2]}).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type verbosity_level() :: text_utils:verbosity_level().

-type ref_id() :: reference_frame:ref_id().
-type ref_table() :: reference_frame:ref_table().
-type reference_frame() :: reference_frame:reference_frame().



% @doc Creates an (empty) reference tree.
-spec new() -> reference_tree().
new() ->
	EmptyTable = table:new(),

	InitChildTable = table:add_entry( _K=?root_id, _Children=[], EmptyTable ),

	#reference_tree{ ref_table=EmptyTable,
					 ref_children=InitChildTable }.



% @doc Registers the specified reference frame(s) in the specified tree; returns
% the identifier allocated to that frame or these frames (then in the same order
% as specified) in the tree, and the corresponding updated version of that tree.
%
-spec register( reference_frame(), reference_tree() ) ->
								{ ref_id(), reference_tree() };
			  ( [ reference_frame() ], reference_tree() ) ->
								{ [ ref_id() ], reference_tree() }.
register( Ref3=#reference_frame3{ parent=MaybeParent },
		  RefTree=#reference_tree{ ref_table=RefTable,
								   next_ref_id=Id,
								   ref_children=ChildTable } ) ->

	AugmRefTable = table:add_new_entry( _K=Id, _V=Ref3, RefTable ),

	NewChildTable = case MaybeParent of

		undefined ->
			NewRootChildren =
				[ Id | table:get_value( _KId=?root_id, ChildTable ) ],

			table:add_entries( [ { Id, _Children=[] },
								 { ?root_id, NewRootChildren } ], ChildTable );

		ParentRefId ->
			case table:get_value_with_default( _Kp=ParentRefId, _Def=not_found,
											   ChildTable ) of

				not_found ->
					throw( { non_registered_parent, ParentRefId, Ref3 } );

				ParentChildren ->
					NewParentChildren = [ Id | ParentChildren ],
					table:add_entries( [
						{ Id, _Children=[] },
						{ ParentRefId, NewParentChildren } ], ChildTable )

			end

	end,

	NewRefTree = RefTree#reference_tree{ ref_table=AugmRefTable,
										 next_ref_id=Id+1,
										 ref_children=NewChildTable },

	{ Id, NewRefTree };

register( Ref3s, RefTree ) ->
	register( Ref3s, _AccIds=[], RefTree ).



% (helper)
register( _Ref3s=[], AccIds, AccRefTree ) ->
	{ lists:reverse( AccIds ), AccRefTree };

register( _Ref3s=[ Ref3 | T ], AccIds, AccRefTree ) ->
	{ ThisId, ThisRefTree } = register( Ref3, AccRefTree ),
	register( T, [ ThisId | AccIds ], ThisRefTree ).



% @doc Returns a (short) textual representation of the specified reference tree.
-spec to_string( reference_tree() ) -> ustring().
to_string( RefTree ) ->
	to_string( RefTree, _VerbLevel=low ).


% @doc Returns a (short) textual representation of the specified reference tree,
% for the specified verbosity level.
%
-spec to_string( reference_tree(), verbosity_level() ) -> ustring().
to_string( #reference_tree{ ref_table=RefTable,
							ref_children=ChildTable },
		   _VerbLevel=low ) ->
	text_utils:format( "reference tree tracking ~B reference frames: ~ts",
		[ table:size( RefTable ), table:to_string( ChildTable ) ] );

to_string( #reference_tree{ ref_table=RefTable,
							ref_children=ChildTable },
		   VerbLevel=high ) ->
	tree:to_string( RefTable, ChildTable, _ParentNodeId=0, VerbLevel ).
