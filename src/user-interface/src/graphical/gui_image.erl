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
% May be useful also for textures.
%
-module(gui_image).


% Implementation notes.
%
% We recommend using the following formats and extensions:
% - PNG (*.png) for lossless, bitmap-like images
% - JPEG (*.jpeg) for images akin to camera snapshots


% Relies on the wxWidgets backend.


-export([ create_from_file/1, create_from_file/2,
		  load/2, load/3, scale/3, scale/4 ]).
		  % from_bitmap/1 ]).



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


-export_type([ image/0, image_format/0, image_quality/0 ]).


% For the wx defines:
-include_lib("wx/include/wx.hrl").



% Shorthands:

%-type count() :: basic_utils:count().

%-type ustring() :: text_utils:ustring().

-type any_file_path() :: file_utils:any_file_path().

-type integer_distance() :: linear:integer_distance().

-type media_type() :: web_utils:media_type().

-type wx_enum() :: wx:wx_enum().



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
% Currently format is ignored:
create_from_file( ImageFormat, AnyImagePath ) ->

	ImagePath = text_utils:ensure_string( AnyImagePath ),

	Image = wxImage:new( ImagePath ),

	case wxImage:isOk( Image ) of

		true ->
			Image;

		false ->
			throw( { image_loading_failed, ImageFormat, ImagePath } )

	end.



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
