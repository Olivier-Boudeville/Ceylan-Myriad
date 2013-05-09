% Copyright (C) 2003-2013 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for Graphical User Interface.
% See gui_test.erl for the corresponding test.
-module(gui).


-include("polygon.hrl").


% Rendering of GUI elements, based on the gs module.


% Implementation notes:

% Sadly with GS, buttons can only have a text label or an XBM image, with
% supports only black and white. One wanting to use either a BMP or a GIF should
% use the canvasbutton module (defined in lib/erlang/lib/toolbar-x.y.z/src) and
% directly usable from the shell. It emulates a GS button, on a specified
% canvas.




% Color-related operations.
-export([ get_colors/0, get_color/1 ]).



% Line-related rendering.
-export([ draw_line/3, draw_line/4, draw_lines/2, draw_lines/3,
		draw_segment/4 ]).


% Rendering of other elements.
-export([ draw_cross/2, draw_cross/3, draw_cross/4, draw_labelled_cross/3,
		draw_labelled_cross/4, draw_labelled_cross/5,
		draw_circle/3, draw_circle/4,
		draw_numbered_points/2 ]).


% The default length of the edge of a cross:
-define( default_cross_edge_length, 5 ).


% Color section.

% Here colors are defined as a triplet of color components: {R,G,B}.
%
% For example {0,0,0} is black and {255,255,255} is white.
%
% GS-predefined names (red, green, blue, white, black, grey, or yellow) are not
% used.

% Returns the list of known {color_name,ColorDefinition} associations.
% Taken from http://www.december.com/html/spec/color16.html.
get_colors() ->
	[
	  {aqua,{0,255,255}},
	  {black,{0,0,0}},
	  {blue,{0,0,255}},
	  {fuchsia,{255,0,255}},
	  {gray,{128,128,128}},
	  {grey,{128,128,128}},
	  {green,{0,128,0}},
	  {lime,{0,255,0}},
	  {maroon,{128,0,0}},
	  {navy,{0,0,128}},
	  {olive,{128,128,0}},
	  {purple,{128,0,128}},
	  {red,{255,0,0}},
	  {silver,{192,192,192}},
	  {teal,{0,128,128}},
	  {white,{255,255,255}},
	  {yellow,{255,255,0}}
	].



% Returns the RGB definition of the color specified by name (atom) or directly
% as a triplet of color components.
get_color( none ) ->
	% none is a special case, for example to disable filling:
	none;

get_color( Color={_R,_G,_B} ) ->
	Color;

get_color(ColorName) ->

	case proplists:get_value( ColorName, get_colors() ) of

		undefined ->
			throw( {unknown_color,ColorName} );

		Color ->
			Color

	end.




% Line section.


% {R,G,B}, or a the predefined name red, green, blue, white, black, grey, or
% yellow.
%
% For example {0,0,0} is black, and {255,255,255} is white.

% Draws a line between specified two points in specified canvas.
draw_line( P1, P2, Canvas ) ->
	gs:create( line, Canvas, [ {coords, [P1,P2]} ] ).


% Draws a line between specified two points in specified canvas, with specified
% color.
draw_line( P1, P2, Color, Canvas ) ->
	gs:create( line, Canvas, [ {coords, [P1,P2]}, {fg,Color} ] ).



% Draws lines between specified list of points, in specified canvas.
draw_lines( Points, Canvas ) ->
	gs:create( line, Canvas, [ {coords, Points} ] ).


% Draws lines between specified list of points in specified canvas, with
% specified color.
draw_lines( Points, Color, Canvas ) ->
	gs:create( line, Canvas, [ {coords, Points}, {fg,Color} ] ).



% Draws a segment of line L between the two specified ordinates.
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
draw_segment( L, Y1, Y2, Canvas ) ->
	gui:draw_line( {linear_2D:get_abscissa_for_ordinate(L,Y1),Y1},
				   {linear_2D:get_abscissa_for_ordinate(L,Y2),Y2}, Canvas ).



% Section for other elements.


% Draws an upright cross at specified location (2D point), with default edge
% length.
draw_cross( Location, Canvas ) ->
	draw_cross( Location, _DefaultEdgeLength=4, Canvas ).


% Draws an upright cross at specified location, with specified edge length.
draw_cross( _Location = {X,Y}, EdgeLength, Canvas ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( {X-Offset,Y}, {X+Offset+1,Y}, Canvas ),
	draw_line( {X,Y-Offset}, {X,Y+Offset+1}, Canvas ).


% Draws an upright cross at specified location, with specified edge length and
% color.
draw_cross( _Location = {X,Y}, EdgeLength, Color, Canvas ) ->
	Offset = EdgeLength div 2,
	% The last pixel of a line is not drawn, hence the +1:
	draw_line( {X-Offset,Y}, {X+Offset+1,Y}, Color, Canvas ),
	draw_line( {X,Y-Offset}, {X,Y+Offset+1}, Color, Canvas ).




% Draws an upright cross at specified location, with specified companion label.
draw_labelled_cross( Location={X,Y}, LabelText, Canvas ) ->
	draw_cross( Location, ?default_cross_edge_length, Canvas ),
	% Text a little above and on the right:
	gs:create( text, Canvas,[ {coords,[{X+4,Y-12}]},
				  {text,LabelText}]).


% Draws an upright cross at specified location, with specified edge length and
% companion label.
draw_labelled_cross( Location={X,Y}, EdgeLength, LabelText, Canvas )
  when is_integer(EdgeLength) ->

	draw_cross( Location, EdgeLength, Canvas ),
	% Text a little above and on the right:
	gs:create( text, Canvas,[ {coords,[{X+4,Y-12}]},
				  {text,LabelText}]);

% Draws an upright cross at specified location, with specified color and
% companion label.
draw_labelled_cross( Location, Color, LabelText, Canvas ) ->
	draw_labelled_cross( Location, ?default_cross_edge_length, Color,
						 LabelText, Canvas ).



% Draws an upright cross at specified location, with specified edge length and
% companion label, and with specified color.
draw_labelled_cross( Location={X,Y}, EdgeLength, Color, LabelText, Canvas ) ->
	ActualColor = gui:get_color(Color),
	draw_cross( Location, EdgeLength, ActualColor, Canvas ),
	% Text a little above and on the right:
	gs:create( text, Canvas,[ {coords,[{X+4,Y-12}]},
				  {text,LabelText}, {fg,ActualColor} ]).



% Renders specified circle in specified canvas.
draw_circle( _Center={X,Y}, Radius, Canvas ) ->
	TopLeft     = {X-Radius,Y-Radius},
	BottomRight = {X+Radius,Y+Radius},
	gs:create( oval, Canvas, [ {coords,[TopLeft,BottomRight]},
	  {fill,none}, {bw,1} ] ).


% Renders specified circle, with specified color, in specified canvas.
draw_circle( _Center={X,Y}, Radius, Color, Canvas ) ->

	TopLeft     = {X-Radius,Y-Radius},
	BottomRight = {X+Radius,Y+Radius},

	gs:create( oval, Canvas, [ {coords,[TopLeft,BottomRight]},
	  {fill,none}, {bw,1}, {fg,gui:get_color(Color)} ] ).



% Draws specified list of points, each point being identified in turn with one
% cross and a label: P1 for the first point of the list, P2 for the next, etc.
draw_numbered_points( Points, Canvas ) ->
	LabelledPoints = label_points( Points, _Acc=[], _InitialCount=1 ),
	%io:format( "Labelled points: ~p.~n", [LabelledPoints] ),
	[ gui:draw_labelled_cross( Location, _Edge=5, Label, Canvas )
	  || {Label,Location} <- LabelledPoints  ].


% Adds a numbered label to each point in list.  Transforms a list of points into
% a list of {PointLabel,Point} pairs while keeping its order.
label_points( [], Acc, _Count ) ->
	% Removes the reverse operation induced by iterating below in this function:
	lists:reverse(Acc);

label_points( [P|T], Acc, Count ) ->
	Label = lists:flatten( io_lib:format("P~B", [Count] ) ),
	label_points( T,[ {Label,P} |Acc], Count+1 ).
