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
% Creation date: Monday, November 29, 2021.


% @doc Gathering of various facilities regarding speech support, i.e. TTS (Text
% to Speech), in order to obtain an audio content corresponding to a specified
% text.
%
-module(speech_support).


% Implementation notes:
%
% We are here neither generating our own speech (way to difficult) nor using
% open solutions like espeak or Festival. Instead we access to the REST API of a
% (commercial) speech provider offering high-end TTS.
%
% We identified three quality providers:
% - Google Wavenet, see https://cloud.google.com/text-to-speech
% - Amazon Polly, see https://aws.amazon.com/fr/polly
% - Microsoft Azure Neural, see REF1:
% https://azure.microsoft.com/en-us/services/cognitive-services/text-to-speech/
%
% We preferred here the Microsoft offer. See REF1 for all details about the
% supported languages, locales, etc.
%
% We rely on user-defined credentials to one's Microsoft Azure account in order
% to use the TTS services.
%
% In practice a series of HTTP requests are made to interact with the service:
% the text to be spoken and the associated data are sent, and the corresponding
% audio samples are fetched.

% The text to be spoken can be specified either as simple text or, better, as
% SSML (see https://en.wikipedia.org/wiki/Speech_Synthesis_Markup_Language).

% Neural voices are created from samples that use a 24 khz sample rate.



-type tts_provider() :: 'google' % Google (Wavenet)
					  | 'aws'    % Amazon Polly
					  | 'azure'. % Microsoft Azure (Neural)
% A known provider of TTS.
%
% See also web_utils:cloud_provider/0.


-type voice_id() :: { tts_provider(), voice_id_at_provider() }.
% The absolute (reference) identifier of a voice (ex: {azure,
% <<"fr-FR-DeniseNeural">>}.


-type voice_id_at_provider() :: bin_string().
% The identifier of a voice (ex: <<"fr-FR-DeniseNeural">>, <<"ar-SA-Naayf">>) in
% the context of a specific TTS provider.


-type voice_name() :: bin_string().
% The full name (just informative) of a voice.
%
% Ex: <<"Foobar Server Speech Text to Speech Voice (xr-XG, Yoda)">>.


-type voice_type() :: 'normal'  % Basic.
					| 'neural'. % AI-produced.
% The type of a voice.


-type voice_gender() :: 'male' | 'female'.
% The gender of a voice.


-type supported_style() :: 'general'
						 | 'senior'
						 | 'child'
						 | 'assistant'
						 | 'news_reading'
						 | 'news_reading_casual'
						 | 'news_reading_formal'
						 | 'story_narrating'
						 | 'work_narrating'
						 | 'conversing'
						 | 'customer_support'
						 | 'calm'
						 | 'fearful'
						 | 'angry'
						 | 'sad'
						 | 'envious'
						 | 'affectionate'
						 | 'gentle'
						 | 'depressed'
						 | 'serious'
						 | 'disgruntled'
						 | 'cheerful'
						 | 'embarrassed'
						 | 'empathetic'
						 | 'lyrical'.
% Defines a style of speech that may be supported by voices.


-type role_played() :: { voice_gender(), age_played() } | 'narrator'.
% A role that a voice may play.


-type age_played() :: 'child' | 'young_adult' | 'senior' | 'older_adult'.
% An age that a voice may roleplay.


% For the voice_info and speech_state records:
-include("speech_support.hrl").

% For records like azure_instance_info:
-include("web_utils.hrl").


-type voice_info() :: #voice_info{}.
% Information regarding a voice for TTS.

-type voice_table() :: table( voice_id(), voice_info() ).
% A table associating the information regarding a voice based on its identifier.


-type ssml_text() :: any_string().
% A text to be spoken, encoded in Speech Synthesis Markup Language (SSML).



-opaque speech_state() :: #speech_state{}.
% The state of the speech support, to be carried between calls.


-export_type([ tts_provider/0, voice_id/0, voice_id_at_provider/0,
			   voice_name/0, voice_type/0, voice_gender/0,
			   supported_style/0,
			   voice_info/0, voice_table/0,
			   ssml_text/0,
			   speech_state/0 ]).

-export([ check_availability/0,
		  get_default_audio_settings/0, get_audio_format_string/1,
		  start/1,
		  list_voices/1, record_speech/4,
		  stop/1,
		  filter_by_gender/2, filter_by_locale/2,
		  speech_state_to_string/1,
		  voice_table_to_string/1,
		  voice_info_to_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type bin_locale() :: locale_utils:bin_locale().

-type json_term() :: json_utils:json_term().

-type any_file_path() :: file_utils:any_file_path().

-type audio_stream_settings() ::audio_utils:audio_stream_settings().


-define( azure_speech_api_endpoint,
		 "api.cognitive.microsoft.com/sts/v1.0/issuetoken" ).



% @doc Tells whether the speech support is available:
% - if true, returns a preliminary, configured speech state
% - if false, returns an extra textual diagnosis
%
% Side-effect: ensures that a Myriad preferences server is running.
%
-spec check_availability() -> { 'true', speech_state() }
							| { 'false', ustring() }.
check_availability() ->

	case json_utils:is_parser_available() of

		true ->
			ParserState = json_utils:start_parser(),

			SpeechState = #speech_state{
				json_parser_state=ParserState,
				http_options=[ { ssl, web_utils:get_ssl_verify_options() } ],
				audio_settings=get_default_audio_settings() },

			check_tts_provider_availability( SpeechState );

		false ->
			{ false, "no JSON parser found available" }

	end.



% @doc Returns the default settings enforced for the speech audio output.
-spec get_default_audio_settings() -> audio_stream_settings().
get_default_audio_settings() ->
	% Corresponds to 'ogg-48khz-16bit-mono-opus', probably close to the best
	% available quality:
	%
	% (another option could be 'raw-48khz-16bit-mono-pcm' to perform the
	% encoding by oneself afterwards)
	%
	{ _StdSamplingRate=48, _ChannelLayout=mono, _BitDepth=16,
	  _ContainerFormat=ogg, _AudioFormat=opus }.



% @doc Tells whether the speech support is available:
% - if true, returns a preliminary, configured speech state
% - if false, returns an extra textual diagnosis
%
% Side-effect: ensures that a Myriad preferences server is running.
%
-spec check_tts_provider_availability( speech_state() ) ->
			{ 'true', speech_state() } | { 'false', ustring() }.
check_tts_provider_availability( SpeechState ) ->

	% We look for the relevant credentials, expected to be found in the user
	% preferences:

	PrefServerPid = preferences:start(),

	InstPrefKeyForKey = azure_speech_instance_key,
	InstPrefKeyForLoc = azure_speech_instance_location,

	[ MaybeInstanceKey, MaybeInstanceLocation ] = preferences:get(
		[ InstPrefKeyForKey, InstPrefKeyForLoc ], PrefServerPid ),

	case MaybeInstanceKey of

		undefined ->
			{ false, text_utils:format( "no key found in the user preferences "
				"to designate a Microsoft Azure instance for speech "
				"(no '~ts' entry found)", [ InstPrefKeyForKey ] ) };

		InstKey ->
			case MaybeInstanceLocation of

				undefined ->
					{ false, text_utils:format( "no location found in the "
						"user preferences to designate a Microsoft Azure "
						"instance for speech (no '~ts' entry found)",
						[ InstPrefKeyForLoc ] ) };

				InstLoc ->
					AzureInstInfo = web_utils:get_azure_instance_information(
										InstKey, InstLoc ),
					{ true, SpeechState#speech_state{
								cloud_instance_info=AzureInstInfo } }

			end

	end.



% @spec Returns the audio format description string corresponding to specified
% settings.
%
% Not all combinations are supported:
% - sampling rate is in [8, 16, 24, 48]
% - channel layout can at least currently only be 'mono'
% - bit depth is 8 or 16, otherwise it is a bit rate in [32, 48, 64, 96, 128,
%   160, 192]
% - recommended container formats are ogg or riff
% - strongly recommended audio format is opus
%
% Refer to
% https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/rest-text-to-speech#audio-outputs for more information.
%
-spec get_audio_format_string( audio_stream_settings() ) -> bin_string().
get_audio_format_string( _AudioStreamSettings={ StdSamplingRate,
		ChannelLayout=mono, BitLevel, ContainerFormat, AudioFormat } ) ->

	BitStr = case BitLevel of

		8 ->
			"8bit";

		16 ->
			"16bit";

		_ ->
			text_utils:format( "~tskbitrate", [ BitLevel ] )

	end,

	% Hope for the best (ex: <<"ogg-24khz-16bit-mono-opus">>):
	BinFormat = text_utils:bin_format( "~ts-~Bkhz-~ts-~ts-~ts",
		[ ContainerFormat, StdSamplingRate, BitStr, ChannelLayout,
		  AudioFormat ] ),

	trace_utils:debug_fmt( "Returning audio format '~ts'.", [ BinFormat ] ),

	BinFormat.



% @doc Starts the speech support.
-spec start( speech_state() ) -> speech_state().
start( SpeechState ) ->
	web_utils:start( _Opts=ssl ),
	SpeechState.



% @doc Returns the available information about all the voices offered by the
% current TTS provider.
%
-spec list_voices( speech_state() ) -> voice_table().
list_voices( #speech_state{ cloud_instance_info=#azure_instance_info{
								instance_key=InstKey,
								instance_location=InstLoc },
							json_parser_state=ParserState,
							http_options=HTTPOptions } ) ->

	Uri = text_utils:format(
		"https://~ts.tts.speech.microsoft.com/cognitiveservices/voices/list",
		[ InstLoc ] ),

	Headers = #{ <<"Ocp-Apim-Subscription-Key">> => InstKey },

	case web_utils:get( Uri, Headers, HTTPOptions ) of

		{ _HTTPStatusCode=200, _Headers, JsonBinBody } ->

			file_utils:write_whole( "voice-listing.json", JsonBinBody ),

			JsonTerm = json_utils:from_json( JsonBinBody, ParserState ),

			%trace_utils:debug_fmt( "Fetched following decoded JSON "
			%                       "content:~n ~p", [ JsonTerm ] ),

			register_voices( _TTSProvider=azure, JsonTerm,
							 _InitTable=table:new() );

		{ HTTPErrorCode, _Headers, BinBody } ->
			trace_utils:error_fmt( "Failed to list voices (~ts; body: ~p)",
				[ web_utils:interpret_http_status_code( HTTPErrorCode ),
				  BinBody ] ),

			throw( { unable_to_list_voices, HTTPErrorCode } );

		{ error, ErrorReason } ->
			trace_utils:error_fmt( "Failed to list voices, reason: ~ts",
								   [ ErrorReason ] ),

			throw( { unable_to_list_voices, ErrorReason } )

	end.



% @doc Registers the specified (non-deprecated, non-preview) voices of specified
% TTS provider in specified voice table.
%
-spec register_voices( tts_provider(), json_term(), voice_table() ) ->
															voice_table().
register_voices( _TTSProvider, _JsonTerm=[], VoiceTable ) ->
	VoiceTable;

register_voices( TTSProvider, _JsonTerm=[ VoiceMap | T ], VoiceTable ) ->

	%trace_utils:debug_fmt( "Registering voice ~p", [ VoiceMap ] ),

	% Extract first all base (always_-pecified) information:
	{ [ VIdAtProvider, ReadType, ReadGender, ReadSampleRate, ReadStatus,
		ReadName, ReadLocale, ReadDisplayName, ReadLocalName, ReadLocDesc ],
	  ExtractedVoiceMap } = table:extract_entries(
		[ <<"ShortName">>, <<"VoiceType">>, <<"Gender">>, <<"SampleRateHertz">>,
		  <<"Status">>, <<"Name">>, <<"Locale">>, <<"DisplayName">>,
		  <<"LocalName">>, <<"LocaleName">> ], VoiceMap ),

	VId = { TTSProvider, VIdAtProvider },

	Type = case ReadType of

		<<"Neural">> ->
			neural

	end,

	Gender = case ReadGender of

		<<"Male">> ->
			male;

		<<"Female">> ->
			female

	end,

	SampleRate = text_utils:binary_to_integer( ReadSampleRate ),

	% Now managing optional information:

	{ Styles, StyleShrunkVMap } = case table:extract_entry_if_existing(
						<<"StyleList">>, ExtractedVoiceMap ) of

		false ->
			{ undefined, ExtractedVoiceMap };

		{ ReadStyles, StVMap } ->
			{ convert_styles( ReadStyles ), StVMap }

	end,

	{ SecLocales, SecLocShrunkVMap } = case table:extract_entry_if_existing(
						<<"SecondaryLocaleList">> , StyleShrunkVMap ) of

		false ->
			{ [], StyleShrunkVMap };

		P -> %{ SecLocs, LocVMap } ->
			P

	end,

	{ Roles, RoleShrunkVMap } = case table:extract_entry_if_existing(
						<<"RolePlayList">>, SecLocShrunkVMap ) of

		false ->
			{ [], SecLocShrunkVMap };

		{ ReadRoles, RlVMap } ->
			{ convert_roles_played( ReadRoles ), RlVMap }

	end,

	FinalVMap = RoleShrunkVMap,

	case table:enumerate( FinalVMap ) of

		[] ->
			ok;

		UnexpectedEntries ->
			trace_utils:warning_fmt( "For voice '~ts', ~B unexpected (ignored) "
				"entries:~n ~p", [ VIdAtProvider, length( UnexpectedEntries ),
								   UnexpectedEntries ] )

	end,

	case ReadStatus of

		<<"GA">> ->
			VoiceRec = #voice_info{
					name=ReadName,
					id=VId,
					type=Type,
					gender=Gender,
					styles=Styles,
					roles_played=Roles,
					locale=ReadLocale,
					locale_description=ReadLocDesc,
					secondary_locales=SecLocales,
					display_name=ReadDisplayName,
					local_name=ReadLocalName,
					sample_rate=SampleRate },

			NewVoiceTable = table:add_new_entry( VId, VoiceRec, VoiceTable ),

			register_voices( TTSProvider, T, NewVoiceTable );

		<<"Preview">> ->
			register_voices( TTSProvider, T, VoiceTable );

		<<"Deprecated">> ->
			register_voices( TTSProvider, T, VoiceTable )

	end.



% @doc Returns a voice table corresponding to the specified one where only the
% voice of the specified gender would be kept.
%
-spec filter_by_gender( voice_gender(), voice_table() ) -> voice_table().
filter_by_gender( Gender, VoiceTable ) ->
	table:fold( fun( VId, VInfo=#voice_info{ gender=G }, Acc )
									when G =:= Gender ->
					table:add_entry( VId, VInfo, Acc );

					( _VId, _VInfo, Acc ) ->
						Acc
				end,
				table:new(),
				VoiceTable ).



% @doc Returns a voice table corresponding to the specified one where only the
% voice of the specified spoken locale (as primary or secondary) would be kept.
%
-spec filter_by_locale( bin_locale(), voice_table() ) -> voice_table().
filter_by_locale( SpokenLocale, VoiceTable ) ->
	table:fold( fun( VId, VInfo=#voice_info{ locale=Loc,
											 secondary_locales=SecLocs },
					 Acc ) ->
						case Loc =:= SpokenLocale
								orelse lists:member( SpokenLocale, SecLocs ) of

							true ->
								table:add_entry( VId, VInfo, Acc );

							false ->
								Acc
						end;

					( _VId, _VInfo, Acc ) ->
						Acc
				end,
				table:new(),
				VoiceTable ).


% @doc Records the speech corresponding to the specified SSML message when
% speeched by the specified voice.
%
-spec record_speech( ssml_text(), voice_id(), any_file_path(),
					 speech_state() ) -> void().
record_speech( SSMLText, _VoiceId={ azure, BinVoiceProviderId }, OutputFilePath,
			   #speech_state{
					cloud_instance_info=#azure_instance_info{
											instance_key=InstKey,
											instance_location=InstLoc },
					http_options=HTTPOptions,
					requester_app_name=BinAppName,
					audio_settings=AudioSettings } ) ->

	Uri = text_utils:format(
		"https://~ts.tts.speech.microsoft.com/cognitiveservices/v1",
		[ InstLoc ] ),

	ContentType = "application/ssml+xml",

	BinAudioFormatStr = get_audio_format_string( AudioSettings ),

	Headers = #{ <<"Ocp-Apim-Subscription-Key">> => InstKey,
				 <<"X-Microsoft-OutputFormat">> => BinAudioFormatStr,
				 <<"User-Agent">> => BinAppName },

	Locale = "en-US",
	Gender = "Male",

	BinSSMLBody = text_utils:bin_format(
		"<speak version='1.0' xml:lang='~ts'>"
		"   <voice xml:lang='~ts' xml:gender='~ts' name='~ts'>~ts</voice>"
		"</speak>",
		[ Locale, Locale, Gender, BinVoiceProviderId, SSMLText ] ),

	%trace_utils:debug_fmt( "XML record body:~n ~ts", [ Body ] ),

	case web_utils:post( Uri, Headers, HTTPOptions, BinSSMLBody,
						 ContentType ) of

		{ _HTTPStatusCode=200, _Headers, BinAudio } ->

			%trace_utils:debug_fmt( "Fetched following audio content "
			%   "of type ~ts:~n ~p",
			%   [ type_utils:interpret_type_of( BinAudio ), BinAudio ] ),

			file_utils:write_whole( OutputFilePath, BinAudio );

		{ HTTPErrorCode, _Headers, BinBody } ->
			trace_utils:error_fmt( "Failed to list voices (~ts; body: ~p)",
				[ web_utils:interpret_http_status_code( HTTPErrorCode ),
				  BinBody ] ),

			throw( { unable_to_list_voices, HTTPErrorCode } );

		{ error, ErrorReason } ->
			trace_utils:error_fmt( "Failed to list voices, reason: ~ts",
								   [ ErrorReason ] ),

			throw( { unable_to_list_voices, ErrorReason } )

	end.



% @doc Returns a textual description of the specified speech state.
-spec speech_state_to_string( speech_state() ) -> ustring().
speech_state_to_string( #speech_state{ cloud_instance_info=InstInfo } ) ->
	text_utils:format( "speech taken in charge by a ~ts",
		[ web_utils:cloud_instance_info_to_string( InstInfo ) ] ).



% @doc Returns a textual description of the specified voice identifier.
-spec voice_id_to_string( voice_id() ) -> ustring().
voice_id_to_string( _VoiceId={ TTSProvider, BinVoiceName } ) ->
	text_utils:format( "voice '~ts' from ~ts",
		[ BinVoiceName, tts_provider_to_string( TTSProvider ) ] ).



% @doc Returns a textual description of the specified TTS provider.
-spec tts_provider_to_string( tts_provider() ) -> ustring().
tts_provider_to_string( _TTSProvider=google ) ->
	"Google Wavenet";

tts_provider_to_string( _TTSProvider=aws ) ->
	"Amazon Polly";

tts_provider_to_string( _TTSProvider=azure ) ->
	"Microsoft Azure".



% @doc Returns a textual description of the specified voice table.
-spec voice_table_to_string( voice_table() ) -> ustring().
voice_table_to_string( VoiceTable ) ->

	case table:values( VoiceTable ) of

		[] ->
			"no voice registered";

		[ SingleVoice ] ->
			text_utils:format( "a single voice registered: ~ts",
				[ voice_info_to_string( SingleVoice ) ] );

		Voices ->
			text_utils:format( "~B voices registered: ~ts",
				[ length( Voices ), text_utils:strings_to_string(
					[ voice_info_to_string( V ) || V <- Voices ] ) ] )

	end.



% @doc Returns a textual description of the specified voice.
-spec voice_info_to_string( voice_info() ) -> ustring().
voice_info_to_string( #voice_info{
						name=FullName,
						id=Id,
						type=Type,
						gender=Gender,
						styles=MaybeStyles,
						roles_played=MaybeRoles,
						locale=Locale,
						locale_description=LocaleDesc,
						secondary_locales=SecondaryLocales,
						display_name=DisplayName,
						local_name=LocalName,
						sample_rate=SampleRate } ) ->

	SecLocStr = case SecondaryLocales of

		[] ->
			"";

		[ SecLoc ] ->
			text_utils:format( "and supporting '~ts' as secondary locale",
							   [ SecLoc ] );

		SecLocs ->
			text_utils:format( "and supporting ~B secondary locales: ~ts",
							   [ length( SecLocs ),
				text_utils:binaries_to_listed_string( SecLocs ) ] )

	end,

	StyleStr = case MaybeStyles of

		undefined ->
			"";

		[] ->
			"";

		[ SingleStyle ] ->
			text_utils:format( ", supporting the '~ts' style",
							   [ SingleStyle ] );

		Styles ->
			text_utils:format( ", supporting ~B styles: ~ts",
							   [ length( Styles ),
				text_utils:atoms_to_listed_string( Styles ) ] )

	end,

	RoleStr = case MaybeRoles of

		undefined ->
			"";

		[] ->
			"";

		[ SingleRole ] ->
			text_utils:format( ", supporting the '~ts' role",
							   [ role_to_string( SingleRole ) ] );

		Roles ->
			text_utils:format( ", supporting ~B roles: ~ts",
				[ length( Roles ),
				  text_utils:strings_to_listed_string(
					[ role_to_string( R ) || R <- Roles ] ) ] )

	end,


	text_utils:format( "~ts (full name is '~ts', display one is '~ts', "
		"local one is '~ts') ~ts of type ~ts, speaking the ~ts locale (~ts)"
		"~ts~ts~ts, with a sampling rate of ~B Hz",
		[ voice_id_to_string( Id ), FullName, DisplayName, LocalName, Gender,
		  Type, Locale, LocaleDesc, SecLocStr, StyleStr, RoleStr,
		  SampleRate ] ).



% @doc Returns a textual description of the specified role.
-spec role_to_string( role_played() ) -> ustring().
role_to_string( { _Gender=male, _AgePlayed=child } ) ->
	"boy";

role_to_string( { _Gender=female, _AgePlayed=child } ) ->
	"girl";


role_to_string( { _Gender=male, _AgePlayed=young_adult } ) ->
	"young man";

role_to_string( { _Gender=female, _AgePlayed=young_adult } ) ->
	"young woman";


role_to_string( { _Gender=male, _AgePlayed=senior } ) ->
	"senior man";

role_to_string( { _Gender=female, _AgePlayed=senior } ) ->
	"senior woman";


role_to_string( { _Gender=male, _AgePlayed=older_adult } ) ->
	"older man";

role_to_string( { _Gender=female, _AgePlayed=older_adult } ) ->
	"older woman";

role_to_string( narrator ) ->
	"narrator".



% @doc Stops the speech support.
-spec stop( speech_state() ) -> void().
stop( _SpeechState=#speech_state{ json_parser_state=ParserState } ) ->
	json_utils:stop_parser( ParserState ),
	web_utils:stop().



% Helpers


% Converts provider styles into our own.
convert_styles( [] ) ->
	[];

% First, translations:
convert_styles( [ <<"assistant">> | T ] ) ->
	[ assistant | convert_styles( T ) ];

convert_styles( [ <<"chat">> | T ] ) ->
	[ conversing | convert_styles( T ) ];

convert_styles( [ <<"narration">> | T ] ) ->
	[ story_narrating | convert_styles( T ) ];

convert_styles( [ <<"narrative">> | T ] ) ->
	[ story_narrating | convert_styles( T ) ];

convert_styles( [ <<"narration-professional">> | T ] ) ->
	[ work_narrating | convert_styles( T ) ];

convert_styles( [ <<"customerservice">> | T ] ) ->
	[ customer_support | convert_styles( T ) ];

convert_styles( [ <<"newscast">> | T ] ) ->
	[ news_reading | convert_styles( T ) ];

convert_styles( [ <<"newscast-casual">> | T ] ) ->
	[ news_reading_casual | convert_styles( T ) ];

convert_styles( [ <<"newscast-formal">> | T ] ) ->
	[ news_reading_formal | convert_styles( T ) ];

convert_styles( [ <<"Envy">> | T ] ) ->
	[ envious | convert_styles( T ) ];

% Then either literal re-use or not known:
convert_styles( [ Other | T ] ) ->
	case lists:member( Other, [ <<"general">>, <<"child">>, <<"assistant">>,
			<<"calm">>, <<"fearful">>, <<"angry">>, <<"sad">>, <<"envious">>,
			<<"affectionate">>, <<"gentle">>, <<"depressed">>, <<"serious">>,
			<<"disgruntled">>, <<"cheerful">>, <<"embarrassed">>,
			<<"empathetic">>, <<"lyrical">> ] ) of

		true ->
			[ text_utils:binary_to_atom( Other ) | convert_styles( T ) ];

		false ->
			trace_utils:warning_fmt( "Unknown speech style: '~ts'; ignored.",
									 [ Other ] ),
			convert_styles( T )

	end.


% Converts provider a role play into our own.
convert_roles_played( [] ) ->
	[];

convert_roles_played( [ <<"Girl">> | T ] ) ->
	[ { female, child } | convert_roles_played( T ) ];

convert_roles_played( [ <<"Boy">> | T ] ) ->
	[ { male, child } | convert_roles_played( T ) ];

convert_roles_played( [ <<"YoungAdultFemale">> | T ] ) ->
	[ { female, young_adult } | convert_roles_played( T ) ];

convert_roles_played( [ <<"YoungAdultMale">> | T ] ) ->
	[ { male, young_adult } | convert_roles_played( T ) ];

convert_roles_played( [ <<"SeniorFemale">> | T ] ) ->
	[ { female, senior }  | convert_roles_played( T ) ];

convert_roles_played( [ <<"SeniorMale">> | T ] ) ->
	[ { male, senior }  | convert_roles_played( T ) ];

convert_roles_played( [ <<"OlderAdultFemale">> | T ] ) ->
	[ { female, older_adult } | convert_roles_played( T ) ];

convert_roles_played( [ <<"OlderAdultMale">> | T ] ) ->
	[ { male, older_adult } | convert_roles_played( T ) ];

convert_roles_played( [ <<"Narrator">> | T ] ) ->
	[ narrator | convert_roles_played( T ) ];

convert_roles_played( [ Other | T ] ) ->
	trace_utils:warning_fmt( "Unknown roleplay: '~ts'; ignored.", [ Other ] ),
	convert_roles_played( T ).
