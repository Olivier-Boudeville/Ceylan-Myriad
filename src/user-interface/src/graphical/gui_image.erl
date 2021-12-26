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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Sunday, November 14, 2021.


% @doc Gathers all elements relative to the management of <b>images</b>
% (loading, modifying, saving, scaling, resizing, clipping, etc.), in link to
% MyriadGUI, and in a platform-independent way.
%
% Images shall be considered as just a generic, platform independent buffer of
% RGB bytes with an optional buffer for the alpha bytes.
%
%  May be useful also for textures.
%
-module(gui_image).


% Implementation notes.
%
% We recommend using the following formats and extensions:
% - PNG (*.png) for lossless, bitmap-like images
% - JPEG (*.jpeg) for images akin to camera snapshots
%
% Note that apparently, according to our test, some images can be loaded fine
% (ex: "erlang.png") whereas some others not (ex: ""myriad-title.png").


% Relies on the wxWidgets backend.


-export([ create_from_file/1, create_from_file/2,
		  getSize/1,
		  load/2, load/3, scale/3, scale/4,
		  colorize/2,
		  % from_bitmap/1,
		  create_bitmap/1,
		  destruct/1 ]).


-type image() :: wxImage:wxImage().
% An image is a bitmap buffer of RGB bytes with an optional buffer for the alpha
% bytes.
%
% It is thus generic, independent from platforms and image file formats.


-type image_format() :: 'png' | 'jpeg'.
% Designates a file format able to store images.
%
% We prefer telling explicitly the type rather than trying to guess it from the
% extension of a filename, as it is clearer and more reliable.
%
% Refer to
% https://docs.wxwidgets.org/3.1/gdicmn_8h.html#a90a1eb6d85b5044a99b706fd979f27f5
% for more image formats.


-type image_quality() :: 'normal' | 'high'.
 % The requested quality for an image operation (ex: for a scaling).


-opaque bitmap() :: wxBitmap:wxBitmap().
% Platform-dependent bitmap, either monochrome or colour (with or without alpha
% channel).
%
% Intended to be a wrapper of whatever is the native image format, which is
% quickest/easiest to draw to a display context.


-export_type([ image/0, image_format/0, image_quality/0, bitmap/0 ]).


% For the wx defines:
-include_lib("wx/include/wx.hrl").



% Shorthands:

%-type count() :: basic_utils:count().

%-type ustring() :: text_utils:ustring().

-type any_file_path() :: file_utils:any_file_path().

-type media_type() :: web_utils:media_type().

-type integer_distance() :: linear:integer_distance().

-type dimensions() :: gui:dimensions().

-type color_by_decimal() :: gui_color:color_by_decimal().

-type rgba_color_buffer() :: gui_color:rgba_color_buffer().

-type alpha_buffer() :: gui_color:alpha_buffer().

-type wx_enum() :: gui_wx_backend:wx_enum().



% @doc Creates an image instance whose content is read from the specified file,
% trying to auto-detect the image format of that file.
%
-spec create_from_file( any_file_path() ) -> image().
create_from_file( AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),
	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Creates an image instance whose content is read from the specified file,
% expecting the image format of the file to be specified one.
%
-spec create_from_file( image_format(), any_file_path() ) -> image().
% Currently format is ignored (unclear how to use format, perhaps to be
% translted as a Mimetype):
create_from_file( ImageFormat, AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),

	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImageFormat, ImagePath } )

	end.



% @doc Returns the size of this image.
-spec getSize( image() ) -> dimensions().
getSize( Image ) ->
	{ wxImage:getWidth( Image ), wxImage:getHeight( Image ) }.



% @doc Scales the specified image to the specified dimensions, with a default
% quality.
%
-spec scale( image(), integer_distance(), integer_distance() ) -> void().
scale( Image, Width, Height ) ->
	wxImage:rescale( Image, Width, Height ).


% @doc Scales the specified image to the specified dimensions, with specified
% quality.
%
-spec scale( image(), integer_distance(), integer_distance(),
			 image_quality() ) -> void().
scale( Image, Width, Height, Quality ) ->
	WxQuality = to_wx_image_quality( Quality ),
	wxImage:rescale( Image, Width, Height,
					 _Opts=[ { quality, WxQuality } ] ).



% @doc Loads the image stored in the specified file in the specified image
% instance, trying to auto-detect the image format of that file.
%
-spec load( image(), any_file_path() ) -> void().
load( Image, ImagePath ) ->
	case wxImage:loadFile( Image, ImagePath ) of

		true ->
			ok;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.


% @doc Loads the image stored in the specified file into the specified image
% instance, expecting the image format of the file to be the specified one.
%
-spec load( image(), image_format(), any_file_path() ) -> void().
load( Image, ImageFormat, ImagePath ) ->
	WxImageFormat = to_wx_image_format( ImageFormat ),
	case wxImage:loadFile( Image, ImagePath,
						   _Opts=[ { type, WxImageFormat } ] ) of

		true ->
			ok;

		false ->
			throw( { image_loading_failed, ImagePath } )

	end.



% @doc Returns a colourized image, that is an image of the specified color,
% modulated by the specified alpha coordinates.
%
-spec colorize( color_by_decimal(), alpha_buffer() ) -> rgba_color_buffer().
colorize( AlphaBuffer, _Color={ R, G, B } ) ->
	% Binary comprehension (and wxImage:setData/3 tells that alpha buffer size
	% is width*height*3, hence dropping 2 out of 3 elements):
	%
	<< <<R:8,G:8,B:8,A:8>> || <<A:8,_:8,_:8>> <= AlphaBuffer >>.



% @doc Returns a bitmap created from the specified image path.
-spec create_bitmap( any_file_path() ) -> bitmap().
create_bitmap( ImagePath ) ->
	Image = wxImage:new( ImagePath ),
	ImgBitmap = wxBitmap:new( Image ),
	wxImage:destroy( Image ),
	ImgBitmap.


% @doc Converts a MyriadGUI image format into a wx one.
-spec to_wx_image_format( image_format() ) -> media_type().
to_wx_image_format( png ) ->
	?wxBITMAP_TYPE_PNG;

to_wx_image_format( jpeg ) ->
	?wxBITMAP_TYPE_JPEG;

to_wx_image_format( Other ) ->
	throw( { unknown_image_format, Other } ).



% @doc Converts a MyriadGUI image format into a wx one.
-spec to_wx_image_quality( image_quality() ) -> wx_enum().
to_wx_image_quality( normal ) ->
	?wxIMAGE_QUALITY_NORMAL;

to_wx_image_quality( high ) ->
	?wxIMAGE_QUALITY_HIGH;

to_wx_image_quality( Other ) ->
	throw( { unknown_image_quality, Other } ).



% @doc Declares that the specified instance can be destructed.
%
% Such it can be reference-counted, it may or may not result in an actual
% deallocation.
%
-spec destruct( image() ) -> void().
destruct( Image ) ->
	wxImage:destroy( Image ).
