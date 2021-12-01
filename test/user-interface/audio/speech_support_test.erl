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


% This is a test of Myriad's <b>speech support</b>, more precisely the
% generation of Text-to-Speech (TTS) audio content.
%
% See the speech_support.erl tested module.
%
-module(speech_support_test).


% Usage notes:
%
% For best quality, we rely here on a third-party neural-based only TTS provider
% (Microsoft Azure, at least currently).
%
% For this test to actually perform TTS, it must be run interactively (not as
% batch, as we do not want to make the user spend cloud credits each time a test
% suite is run), and a suitable Azure account must be found in the user Ceylan
% preferences (see ~/.ceylan-settings.etf).


% For run/0 export and al:
-include("test_facilities.hrl").

% For the voice_info record:
-include("speech_support.hrl").


% Shorthands:

-type speech_state() :: speech_support:speech_state().



% @doc The actual speech test.
-spec run_speech_test() -> void().
run_speech_test() ->

	case speech_support:check_availability() of

		{ true, CfgSpeechState } ->
			test_facilities:display( "Speech support found available: ~ts.",
				[ speech_support:speech_state_to_string( CfgSpeechState ) ] ),

			SpeechState = speech_support:start( CfgSpeechState ),

			run_test_tts( SpeechState ),

			speech_support:stop( SpeechState );

		{ false, ReasonStr } ->
			test_facilities:display( "Speech support not found available: ~ts; "
									 "no test performed.", [ ReasonStr ] )

	end.



% @doc The actual TTS test.
-spec run_test_tts( speech_state() ) -> void().
run_test_tts( SpeechState ) ->

	test_facilities:display( "Testing TTS service." ),

	VoiceTable = speech_support:list_voices( SpeechState ),

	test_facilities:display(
		speech_support:voice_table_to_string( VoiceTable ) ),

	FemaleVoiceTable = speech_support:filter_by_gender( female, VoiceTable ),


	FrenchSpokenLocale = <<"fr-FR">>,

	FrenchVoiceTable =
		speech_support:filter_by_locale( FrenchSpokenLocale, VoiceTable ),

	EnglishSpokenLocale = <<"en-GB">>,

	EnglishVoiceTable =
		speech_support:filter_by_locale( EnglishSpokenLocale, VoiceTable ),

	test_facilities:display( "On the ~B voices, ~B are female, ~B have "
		"~ts as spoken locale (as primary or secondary), ~B for ~ts.",
		[ table:size( VoiceTable ), table:size( FemaleVoiceTable ),
		  table:size( FrenchVoiceTable ), FrenchSpokenLocale,
		  table:size( EnglishVoiceTable ), EnglishSpokenLocale ] ),

	FrenchVoiceInfo = pair:second( hd( table:enumerate( FrenchVoiceTable ) ) ),

	FrenchSSMLText = "Ces paroles ont été générées via la synthèse vocale "
		"mise en place par Ceylan-Myriad. N'est-ce point merveilleux ?",

	speech_support:record_speech( FrenchSSMLText,
		FrenchVoiceInfo#voice_info.id, "speech-test-french.ogg", SpeechState ),

	_EnglishVoiceInfo =
		pair:second( hd( table:enumerate( EnglishVoiceTable ) ) ),

	_EnglishSSMLText = "Welcome to the Myriad support for speech synthesis.",

	%speech_support:record_speech( EnglishSSMLText,
	%	EnglishVoiceInfo#voice_info.id, "speech-test-english.ogg",
	%	SpeechState ).

ok.

% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the speech support test, being in batch mode)" );

		false ->
			run_speech_test()

	end,

	test_facilities:stop().