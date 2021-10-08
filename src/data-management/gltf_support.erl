% Copyright (C) 2021-2021 Olivier Boudeville
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
% Creation date: Monday, October 4, 2021.
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Gathering of various facilities about the <b>glTF 2.0</b> file format (GL
% Transmission Format).
%
% See [https://en.wikipedia.org/wiki/GlTF].
%
-module(gltf_support).


% For the various gltf_* records:
-include("gltf_support.hrl").


-type content() :: #gltf_content{}.
% Gathers all information regarding a glTF 2.0 content.



-type scene_index() :: zero_index().
% Index of a scene in a glTf content.

-type scene() :: #gltf_scene{}.
% A scene defined in a glTf content.



-type node_index() :: zero_index().
% Index of a node in a glTf content.


% node/0 being a builtin:
-type scene_node() :: #gltf_scene_node{}.
% A node defined in a glTf content.



-type mesh_index() :: zero_index().
% Index of a mesh in a glTf content.


-type mesh() :: #gltf_mesh{}.
% A mesh defined in a glTf content.


-type material() :: #gltf_material{}.


-type pbr_metallic_roughness() :: #gltf_pbr_metallic_roughness{}.
% Describes the metallic roughtness of a material, based on the Physically-Based
% Rendering (PBR) methodology.



-type buffer_index() :: zero_index().
% Index of a buffer in a glTf content.


-type buffer() :: #gltf_buffer{}.
% A buffer of raw data.



-type buffer_view_index() :: zero_index().
% Index of a buffer view in a glTf content.


-type buffer_view() :: #gltf_buffer_view{}.
% A view onto a given buffer.



-export_type([ content/0,
			   scene_index/0, scene/0,
			   node_index/0, scene_node/0,
			   mesh_index/0, mesh/0,
			   material/0,
			   pbr_metallic_roughness/0,
			   buffer_index/0, buffer/0,
			   buffer_view_index/0, buffer_view/0 ]).


-export([  ]).



% Shorthands:

-type zero_index() :: basic_utils:zero_index().

%-type ustring() :: text_utils:ustring().

%-type vector3() :: vector3:vector3().
%-type quaternion() :: quaternion:quaternion().

%-type render_color() :: gui_color:render_color().

% Design notes:
%
% Materials are defined based on the Physically-Based Rendering (PBR)
% methodology.
