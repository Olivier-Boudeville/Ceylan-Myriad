% Copyright (C) 2021-2025 Olivier Boudeville
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
% Creation date: Friday, November 19, 2021.

-module(gui_font).

-moduledoc """
Gathering of various facilities for **font management**.
""".


-doc """
Designates a font object.

It applies to text rendering, including the ones done from the gui_text module;
once created, use gui_widget:set_font/{2,3,4} or gui_render:set_font/{3,4} to
make use of it. Do not forget deallocating it with destruct/1.
""".
-opaque font() :: wxFont:wxFont().



-doc """
The dimensions, in pixels, of characters drawn from a font.

Note that generally, due to capital letters, descent and other factors, instead
of relying on the size of a font, using the actual extent of a target text (see
get_precise_text_extent/2) is a lot more relevant.
""".
-type font_size() :: gui:dimensions().



-doc """
A font size, in pixels.

Note that generally, due to capital letters, descent and other factors, instead
of relying on the point size of a font, using the actual extent of a target text
(see get_precise_text_extent/2) is a lot more relevant.
""".
-type point_size() :: gui:length().



-doc "A font family.".
-type font_family() :: 'default_font_family' | 'decorative' | 'roman'
					 | 'script' | 'swiss' | 'modern' | 'teletype'.



-doc "A font style.".
-type font_style() :: 'normal' | 'italic' | 'slant'.



-doc "A font weight".
-type font_weight() :: 'thin' | 'extra_light' | 'light' | 'normal' | 'medium'
					 | 'semi_bold' | 'bold' | 'extra_bold' | 'heavy'
					 | 'extra_heavy'.



-doc "Currently the same as wx.".
-type text_encoding() :: wx_text_encoding().



-doc "A font option.".
-type font_option() :: { 'underline', boolean() }
					 | { 'face_name', any_string() }
					 | { 'encoding', text_encoding() }.



-doc """
Actual (opaque) data corresponding to a font.

A wx object.
""".
-opaque font_data() :: wxFontData:wxFontData().



-doc "Length from the baseline of a font to the bottom of the descender.".
-type descent() :: length().



-doc """
Any extra vertical space added to the font by the font designer (usually is
null).
""".
-type external_leading() :: length().


-doc """
A precise text extent.

Useful for example so that texts are rendered at the same height.
""".
-type precise_text_extent() ::
	{ width(), height(), descent(), external_leading() }.


-export_type([ font/0, font_size/0, point_size/0, font_family/0, font_style/0,
			   font_weight/0, text_encoding/0, font_option/0, font_data/0,
			   descent/0, external_leading/0, precise_text_extent/0 ]).


% Font-related instance-level operations.
-export([ create/1, create/2, create/3, create/4, create/5, destruct/1,
		  get_platform_dependent_description/1, get_user_friendly_description/1,
		  is_fixed_width/1, get_text_extent/2, get_precise_text_extent/2 ]).


% General font information.
-export([ list_families/0, list_styles/0, list_weights/0 ]).


% Exported helpers (and silencing):
-export([ to_wx_font_family/1, to_wx_font_style/1, to_wx_font_weight/1 ]).


% For wx defines:
-include("gui_internal_defines.hrl").


-type wx_font_family() :: wx_enum().

-type wx_font_style() :: wx_enum().

-type wx_font_weight() :: wx_enum().

-type wx_text_encoding() :: wx_enum().

-type wx_font_option() :: any().



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().

-type length() :: gui:length().
-type width() :: gui:width().
-type height() :: gui:height().
-type dimensions() :: gui:dimensions().

-type wx_enum() :: gui_wx_backend:wx_enum().




% Font-related instance-level operations.


-doc """
Creates a font object from the specified requirements, to determine the
appearance of rendered texts from now on.
""".
-spec create( font_size() | point_size() ) -> font().
create( FontSize ) ->
	create( FontSize, _FontFamily=default_font_family ).



-doc """
Creates a font object from the specified requirements, to determine the
appearance of rendered texts from now on.
""".
-spec create( font_size() | point_size(), font_family() ) -> font().
create( FontSize, FontFamily ) ->
	create( FontSize, FontFamily, _FontStyle=normal ).



-doc """
Creates a font object from specified requirements, to determine the appearance
of rendered text.
""".
-spec create( font_size() | point_size(), font_family(), font_style() ) ->
											font().
create( FontSize, FontFamily, FontStyle ) ->
	create( FontSize, FontFamily, FontStyle, _FontWeight=normal ).



-doc """
Creates a font object from specified requirements, to determine the appearance
of rendered text.
""".
-spec create( font_size() | point_size(), font_family(), font_style(),
			  font_weight() ) -> font().
create( FontSize, FontFamily, FontStyle, FontWeight ) ->
	create( FontSize, FontFamily, FontStyle, FontWeight, _FontOpts=[] ).



-doc """
Creates a font object from specified requirements, to determine the appearance
of rendered text.
""".
-spec create( font_size() | point_size(), font_family(), font_style(),
			  font_weight(), [ font_option() ] ) -> font().
create( FontSize, FontFamily, FontStyle, FontWeight, FontOpts ) ->

	cond_utils:if_defined( myriad_debug_gui_font, trace_utils:debug_fmt(
		"Creating a font of family '~ts', of size ~w, "
		"~ts style, ~ts weight and options ~p.",
		[ FontFamily, FontSize, FontStyle, FontWeight, FontOpts ] ) ),

	WxFontFamily = to_wx_font_family( FontFamily ),

	WxFontStyle = to_wx_font_style( FontStyle ),

	WxFontWeight = to_wx_font_weight( FontWeight ),

	WxFontOpts = to_wx_font_options( FontOpts ),

	Font = wxFont:new( FontSize, WxFontFamily, WxFontStyle, WxFontWeight,
					   WxFontOpts ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxFont:isOk( Font ) ),

	Font.



-doc "Destructs the specified font.".
-spec destruct( font() ) -> void().
destruct( Font ) ->
	wxFont:destroy( Font ).



-doc """
Returns the platform-dependent complete description of the specified font.

For example, "Sans 10".
""".
-spec get_platform_dependent_description( font() ) -> ustring().
get_platform_dependent_description( Font ) ->
	wxFont:getNativeFontInfoDesc( Font ).



-doc """
Returns a user-friendly description of the specified font.

For example, "Sans 10".
""".
-spec get_user_friendly_description( font() ) -> ustring().
get_user_friendly_description( Font ) ->
	wxFont:getNativeFontInfoUserDesc( Font ).



-doc "Tells whether the specified font is fixed-width.".
-spec is_fixed_width( font() ) -> boolean().
is_fixed_width( Font ) ->
	wxFont:isFixedWidth( Font ).


-doc """
Returns the extent used by the rendering of the specified single-line text with
the specified font.

Note that the returned height may be larger than the actual one of the text, due
to the margin taken for letters possibly going though the baseline (like 'g'),
the descent.
""".
-spec get_text_extent( ustring(), font() ) -> dimensions().
get_text_extent( Text, Font ) ->

	cond_utils:if_defined( myriad_debug_gui_font, trace_utils:debug_fmt(
		"Getting extent of text '~ts' for font ~p.", [ Text, Font ] ) ),

	% We have to create dummy bitmap and device contexts in order to determine
	% these dimensions:

	TmpBmp = wxBitmap:new( _W=200, _H=200 ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxBitmap:isOk( TmpBmp ) ),

	TmpDC = wxMemoryDC:new( TmpBmp ),

	cond_utils:if_defined( myriad_debug_gui_memory, true = wxDC:isOk( TmpDC ) ),

	wxMemoryDC:setFont( TmpDC, Font ),

	Dims = wxDC:getTextExtent( TmpDC, Text ),

	wxMemoryDC:destroy( TmpDC ),

	wxBitmap:destroy( TmpBmp ),

	Dims.



-doc """
Returns the precise extent used by the rendering of the specified single-line
text with the specified font.
""".
-spec get_precise_text_extent( ustring(), font() ) -> precise_text_extent().
get_precise_text_extent( Text, Font ) ->

	cond_utils:if_defined( myriad_debug_gui_font, trace_utils:debug_fmt(
		"Getting precise extent of text '~ts' for font ~p.", [ Text, Font ] ) ),

	% We have to create dummy bitmap and device contexts in order to determine
	% these information:

	TmpBmp = wxBitmap:new( _W=200, _H=200 ),

	cond_utils:if_defined( myriad_debug_gui_memory,
						   true = wxBitmap:isOk( TmpBmp ) ),

	TmpDC = wxMemoryDC:new( TmpBmp ),

	cond_utils:if_defined( myriad_debug_gui_memory, true = wxDC:isOk( TmpDC ) ),

	wxMemoryDC:setFont( TmpDC, Font ),

	% The option (and return type) is the difference with a mere
	% get_text_extent/2:
	%
	PExtent = wxDC:getTextExtent( TmpDC, Text, _Opts=[ { theFont, Font } ] ),

	wxMemoryDC:destroy( TmpDC ),

	wxBitmap:destroy( TmpBmp ),

	PExtent.



% General font information.


-doc "Returns a list of the base font families.".
-spec list_families() -> [ font_family() ].
list_families() ->
	[ default_font_family, decorative, roman, script, swiss, modern, teletype ].



-doc "Returns a list of the supported font styles.".
-spec list_styles() -> [ font_style() ].
list_styles() ->
	[ normal, italic, slant ].



-doc "Returns a list of the supported font weights.".
-spec list_weights() -> [ font_weight() ].
list_weights() ->
	[ thin, extra_light, light, normal, medium, semi_bold, bold, extra_bold,
	  heavy , extra_heavy ].



% Helpers.


% Next constants will have to be integrated in gui_constants.


-doc "Converts the specified font family into a wx one.".
-spec to_wx_font_family( font_family() ) -> wx_font_family().
to_wx_font_family( default_font_family ) ->
	?wxFONTFAMILY_DEFAULT;

to_wx_font_family( decorative ) ->
	?wxFONTFAMILY_DECORATIVE;

to_wx_font_family( roman ) ->
	?wxFONTFAMILY_ROMAN;

to_wx_font_family( script ) ->
	?wxFONTFAMILY_SCRIPT;

to_wx_font_family( swiss ) ->
	?wxFONTFAMILY_SWISS;

to_wx_font_family( modern ) ->
	?wxFONTFAMILY_MODERN;

to_wx_font_family( teletype ) ->
	?wxFONTFAMILY_TELETYPE;

to_wx_font_family( Other ) ->
	throw( { unknown_font_family, Other } ).



-doc "Converts the specified font style into a wx one.".
-spec to_wx_font_style( font_style() ) -> wx_font_style().
to_wx_font_style( normal ) ->
	?wxFONTSTYLE_NORMAL;

to_wx_font_style( italic ) ->
	?wxFONTSTYLE_ITALIC;

to_wx_font_style( slant ) ->
	?wxFONTSTYLE_SLANT;

to_wx_font_style( Other ) ->
	throw( { unknown_font_style, Other } ).



-doc "Converts the specified font weight into a wx one.".
-spec to_wx_font_weight( font_weight() ) -> wx_font_weight().
to_wx_font_weight( thin ) ->
	?wxFONTWEIGHT_THIN;

to_wx_font_weight( extra_light ) ->
	?wxFONTWEIGHT_EXTRALIGHT;

to_wx_font_weight( light ) ->
	?wxFONTWEIGHT_LIGHT;

to_wx_font_weight( normal ) ->
	?wxFONTWEIGHT_NORMAL;

to_wx_font_weight( medium ) ->
	?wxFONTWEIGHT_MEDIUM;

to_wx_font_weight( semi_bold ) ->
	?wxFONTWEIGHT_SEMIBOLD;

to_wx_font_weight( bold ) ->
	?wxFONTWEIGHT_BOLD;

to_wx_font_weight( extra_bold ) ->
	?wxFONTWEIGHT_EXTRABOLD;

to_wx_font_weight( heavy ) ->
	?wxFONTWEIGHT_HEAVY;

to_wx_font_weight( extra_heavy ) ->
	?wxFONTWEIGHT_EXTRAHEAVY;

to_wx_font_weight( Other ) ->
	throw( { unknown_font_weight, Other } ).



-doc "Converts the specified font options into wx ones.".
-spec to_wx_font_options( [ font_option() ] ) -> [ wx_font_option() ].
to_wx_font_options( FontOpts ) ->
	[ to_wx_font_option( FO ) || FO <- FontOpts ].



-doc "Converts the specified font option into a wx one.".
-spec to_wx_font_option( font_option() ) -> wx_font_option().
to_wx_font_option( FontOpt={ underline, _Bool } ) ->
	FontOpt;

to_wx_font_option( _FontOpt={ face_name, FaceName } ) ->
	{ faceName, FaceName };

to_wx_font_option( FontOpt={ encoding, _Enum } ) ->
	FontOpt;

to_wx_font_option( OtherFontOpt ) ->
	throw( { unsupported_font_option, OtherFontOpt } ).
