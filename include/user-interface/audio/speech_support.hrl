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
% Creation date: Tuesday, November 30, 2021.


% The state of our speech service:
-record( speech_state, {

	% Information regarding any clould instance that would typically used for
	% TTS:
	%
	cloud_instance_info :: maybe( web_utils:cloud_instance_info() ),

	% The state of the JSON parser used to interact with the cloud instance:
	json_parser_state :: maybe( json_utils:parser_state() ),

	% The HTTP options that all requests shall use:
	http_options :: web_utils:http_options(),

	% The name of the application requesting speech services (less than 255
	% characters).
	%
	requester_app_name = <<"Myriad speech support">> :: text_utils:bin_string(),

	% The audio settings regarding the generated output:
	audio_settings :: audio_utils:audio_stream_settings() } ).



% Information regarding a voice for TTS:
-record( voice_info, {

	% The full name (just informative) of this voice:
	name :: speech_support:voice_name(),

	% The actual (reference) identifier of this voice:
	id :: speech_support:voice_id(),

	% The type of this voice:
	type :: speech_support:voice_type(),

	% The (main) gender of this voice:
	gender :: speech_support:voice_gender(),

	% The styles of speech (if any known) supported by this voice:
	styles :: maybe( [ speech_support:supported_style() ] ),

	% The specific roles that this voice may play:
	roles_played :: maybe( [ speech_support:role_played() ] ),

	% The (main) locale corresponding to the language spoken by this voice:
	locale :: locale_utils:bin_locale(),

	% The description of the (main) locale corresponding to the language spoken
	% by this voice:
	%
	locale_description :: locale_utils:bin_locale_description(),


	% The extra locales (if any) that this voices may speak:
	secondary_locales :: [ locale_utils:bin_locale() ],


	% The name for simple display (ex: <<"Hoda">>):
	display_name :: text_utils:bin_string(),

	% The name according to its locale (ex: <<"هدى">>):
	local_name :: text_utils:bin_string(),

	% The sample rate of the rendering of this voice:
	sample_rate :: audio_utils:sample_rate() } ).
