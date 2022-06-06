% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Saturday, June 4, 2022.


% @doc Gathering of facilities to manage <b>octrees</b>, a tree datastructure in
% which each internal node has exactly 8 children (octants, i.e. "cells" - a
% naming that is probably clearer) that partition the parent space.
%
% A global right cuboid is recursively split in 8 smaller, same-sized octants,
% (which are its direct child right cuboids), dividing the space thanks to a
% tree until some criterion is met (ex: volume/number of elements in the
% resulting leaves below some hreshold, maximum tree depth reached).
%
% See https://en.wikipedia.org/wiki/Octree.
%
-module(octree).


% Design notes:
%
% We implement here a "Point Region" (PR) octree, as opposed to a "MatriX-based"
% (MX) one, as we want our octree to adapt to unbounded spaces.

% Other related datastructures:
% - kd-trees (see https://en.wikipedia.org/wiki/K-d_tree)
% - "Bounding Volume Hierarchy" (BVH):
% https://en.wikipedia.org/wiki/Bounding_volume_hierarchy
% - "Binary space partitioning" (BSP):
% https://en.wikipedia.org/wiki/Binary_space_partitioning

% Octrees vs kd-trees: octrees are tries, kd-trees are balanced binary trees.


% Advantages of octrees:
%
% - high branching factor (8 for octrees) means shallower trees that incur fewer
% indirections, so searching may be fast, especially for homogeneous
% distributions
%
% - insertions and deletions are costly in kd-trees (rebalancing having to be
% done, whereas octrees do not have to be rebalanced); yet balancing handles
% heterogeneity better because it is adaptive, while searches in imbalanced
% tries (octrees) may require many indirections
%
% - bisection (as in octrees) lends itself to trivial implementation in terms of
% bit-twiddling; octrees can benefit greatly from precomputed distances when
% doing range lookups
%
% - kd-trees can have high aspect ratio (dissimilardimensions), whereas octree
% cells are guaranteed to be cubical; it makes it impossible to use volume
% bounds to control the number of cells that you have to examine when solving
% approximate nearest neighbor queries


% Advantages of kd-trees:
%
% - constructing and querying k-nearest neighbours: kd-trees are typically
% superior in performance for most datasets
%
% - kd-trees are guaranteed to have at most logarithmic depth (yet in general
% are deeper than octrees due to theiur binary subdivision); worst time
% complexity is lower than octrees


% Source:
%
% - https://cstheory.stackexchange.com/questions/8470/why-would-one-ever-use-an-octree-over-a-kd-tree
% - https://observablehq.com/@2talltim/spatial-data-structures-octrees-bsp-and-k-d-trees
% - https://observablehq.com/@2talltim/spatial-data-structures-octrees-bsp-and-k-d-trees
% - https://en.wikipedia.org/wiki/Quadtree (to be generalised from 2D to 3D, in
% octrees), notably https://en.wikipedia.org/wiki/Quadtree#Region_quadtree

% Some "octrees" may be unevenly divided (like in
% https://en.wikipedia.org/wiki/Quadtree#Point_quadtree).


% Specific requirements addressed here:

% We want a datastructure to track a large number of *moving* objects in a
% larger space, so octrees may be more relevant as migrating in a kd-tree is
% costlier); yet the likely heterogeneous distribution of objects would plead
% for kd-trees.
%
% The rather high branching factor of an octree (8) is probably an advantage in
% languages like Erlang where no pointer arithmetics or even reference-to-term
% exists; more importantly, each octant may be a process, transforming the
% overall octree in a concurrent datastructure; this module offers such an
% octree.
%
% With larger global spaces, to each octant an absolute, fixed referential can
% be associated, whose origin is defined relatively to the center/origin of its
% parent. The corresponding transformation, possibly also together with another
% precomputed one from the local octant to the (unique, top-level) root one,
% would allow each object to be defined only relatively to the (fixed)
% referential corresponding to its (current) octant; by enabling smaller
% floating-point distances, numerical errors should be significantly lessened.
%
% Note that multiple, different datastructures may be used (created and updated)
% simultaneously, so that each type of operation (ex: query) can be performed on
% the most efficient one.


% Our octree:
%
% - is not balanced, in the sense that octants will be split according to local
% criteria - rather than uniformly
%
% - each internal node (i.e. non-leaf node) may contain its own elements; this
% may be convenient for:
%
%  (1) (non-punctual, typically rigid-body) elements that would not fit in
%  smaller cells (that is: larger objects)
%
%  (2) rapidly-moving objects that would keep on transitioning between neighbour
%  cells (ex: a planet orbiting across multiple elementary cells)
%
%  (3) to store aggregate information, variable resolution representation of a
%  data field (ex: average value - like color, temperature, mass/gravitational
%  field - of the elements recursively contained)
%
%  However queries then have to take into account additionally the full path
%  from the current cell to the root one - which might not be desirable.
%
% - each node may by default contain any number of elements (typically as a set
% of references to objects); this translates to "unlimited bucket capacity" (as
% opposed to, ay, exactly one object per cell)
%
% - can be either be a classical datastructure, represented as a term used by a
% process, or a concurrent one, where each octant is managed by a dedicated
% process and identified by its PID

% For an octree of height H (defined as the maximum number of edges (ex: not
% vertices) traversed from root to leaves, its leaves define (up to) 8^H
% elementary cells; for a global space that would be a cube of side length L
% (hence of volume L^3), each cell would be of length L/2^H (hence of volume
% L^3/8^H).



% So that we can defined our own size/1:
-compile( { no_auto_import, [ size/1 ] } ).


-type node_content() :: maybe( any() ).
% The content of a node of an octree ('undefined' meaning empty content).


% For the octants and concurrent_octants records:
-include("octree.hrl").

-type octants() :: #octants{}.
% The 8 possible sub-octrees.

-type octants( _T ) :: #octants{}.
% The 8 possible sub-octrees, of content T.


-opaque octree() :: { node_content(), maybe( octants() ) }.
% Any octree is made of its own content and of up to 8 children octrees.
%
% Our octree may have content of its own even if it has at least one non-empty
% child (sub-octree). This may be useful for example for planets known to
% revolve in the current octree without being confined in any of its child
% octrees; or objects large enough/positioned so that they pertain partly to one
% octree, partly to at least one another.
%
% This octree type is sequential (as opposed to concurrent), in the sense that
% it is defined as a single term (used by a given process), unlike
% concurrent_octree/0.


-opaque octree( T ) :: { T , octants( T ) }.
% A typed octree polymorphic regarding its node content.
%
% See octree/0 for further details.



-opaque octree_pid() :: pid().
% The PID of a concurrent octree, in charge of a given octant.

-type concurrent_octants() :: #concurrent_octants{}.
% The 8 possible sub-concurrent octrees.

-type concurrent_octants( _T ) :: #concurrent_octants{}.
% The 8 possible sub-concurrent octrees, of content T.


-type octree_state( T ) :: { T, concurrent_octants( T ) }.
% The state of a concurrent octree, that is a state kept by a process in charge
% of a concurrent octree.


-type content_fold_fun() ::
		fun( ( node_content(), accumulator() ) -> accumulator() ).
% Describes a function that can be folded onto the content of an octree.


-type height() :: count().
% Height of an octree, as a number of edges between the root cell and the
% deepest leaf one.


-export_type([ octree/0, octree/1, node_content/0, octants/0, octants/1,

			   octree_pid/0, concurrent_octants/0, concurrent_octants/1,
			   octree_state/1,

			   content_fold_fun/0, height/0 ]).


-export([ new/0, new/1 ]).
%% , new/2, set_content/2, append_child/2, append_children/2,
%%		  map/2, fold_breadth_first/3, fold_depth_first/3,
%%		  height/1, size/1, to_string/1 ]).


% Shorthands:

-type count() :: basic_utils:count().
-type accumulator() :: basic_utils:accumulator().

%-type ustring() :: text_utils:ustring().



% @doc Creates an empty (regarding content and octants), sequential, octree.
-spec new() -> octree().
new() ->
	{ _NodeContent=undefined, #octants{} }.



% @doc Creates a sequential, octree having specified content, and no octants.
-spec new( node_content() ) -> octree().
new( NodeContent ) ->
	{ NodeContent, _Octants=undefined }.
