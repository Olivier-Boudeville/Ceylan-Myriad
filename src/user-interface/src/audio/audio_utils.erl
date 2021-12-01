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
% Creation date: Tuesday, November 30, 2021


% @doc This module concentrates <b>audio-related elements</b>.
%
% See speech_support.erl for TTS.
%
-module(audio_utils).



% Implementation notes:
%


-type sample_rate() :: unit_utils:integer_hertz() .
% The sample rate of an audio content (ex: 16000 Hz).


% Generally in [8, 16, 24, 48]:
-type standard_sampling_rate() :: pos_integer().
% A standard sample rate, in kHz (ex: 24 kHz).


-type bit_rate() :: pos_integer().
% The bit rate of an audio content, in kilobits per second (ex: 192 kbps).


-type bit_depth() :: pos_integer().
% The number of bits to which each audio sample is quantized.
%
% Ex: 8-bit or 16-bit.


-type channel_layout() :: 'mono' | 'stereo' | '5.1'.
% The number and layout of audio channels.
%
% See the 'Standard speaker channels' section of
% https://en.wikipedia.org/wiki/Surround_sound for further details.


-type container_format() :: 'raw' | 'ogg' | 'webm' | 'riff'.
% Describes how to store metadata and possibly multiple audio streams in a
% binary stream.
%
% See the 'Audio coding formats support' of
% https://en.wikipedia.org/wiki/Comparison_of_video_container_formats for
% further details.


-type audio_format() :: 'raw' | 'pcm' | 'mp3' | 'vorbis' | 'aac' | 'flac'
					  | 'opus' | 'mulaw' | 'alaw' | 'truesilk'.
% A content representation format for storage or transmission of digital audio,
% i.e. how audio content is encoded.
%
% Refer to https://en.wikipedia.org/wiki/Comparison_of_audio_coding_formats for
% further details.


-type audio_stream_settings() ::
		{ standard_sampling_rate(), channel_layout(), bit_depth() | bit_rate(),
		  container_format(), audio_format() }.
% The main settings that apply for an audio stream.


-type codec() :: bin_string().
% An implementation in charge of coding/decoding an audio format (ex: the codecs
% of FFmpeg).


-export_type([ sample_rate/0, standard_sampling_rate/0,
			   bit_rate/0, bit_depth/0,
			   channel_layout/0,
			   container_format/0, audio_format/0, audio_stream_settings/0,
			   codec/0 ]).


% Shorthands:

-type bin_string() :: text_utils:bin_string().
