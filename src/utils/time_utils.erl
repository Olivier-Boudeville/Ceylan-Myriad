% Copyright (C) 2015-2017 Olivier Boudeville
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
% Creation date: Friday, July 24, 2015



% Gathering of time management facilities.
%
% See time_utils_test.erl for the corresponding test.
%
-module(time_utils).


% Implementation notes:
%
% Native support in Erlang for time-related operations is mostly located in the
% calendar module.
%
% A typical date format of interest here is: "Friday, July 24, 2015".
%
% A month is a positive integer, a canonical month is in [1,12].



% Month management support:
-export([ canonicalise_month/1, check_month_canonical/1, check_month_order/2,
		  month_to_string/1, week_day_to_string/1 ]).


% Date support:
-export([ compare_dates/2, check_date_order/2, get_date_difference/2 ]).


% As calendar:daynum is not exported:
%
% (Monday is 1, Tuesday is 2, etc.)
%
-type day_index() :: 1..7.


% Calendar date; used to be less precise calendar:date():
-type date() ::  { unit_utils:year(), unit_utils:canonical_month(),
				   unit_utils:canonical_day() }.


% Time in the day; used to be { hour(), minute(), second() } or calendar:time():
-type time() :: { unit_utils:canonical_hour(), unit_utils:canonical_minute(),
				  unit_utils:canonical_second() }.


-export_type([ day_index/0, date/0, time/0 ]).


% For rough, averaged conversions:
-export([ years_to_seconds/1, months_to_seconds/1, weeks_to_seconds/1,
		  days_to_seconds/1, hours_to_seconds/1 ]).


% Timestamp-related section.
%
-export([ get_timestamp/0,
		  get_textual_timestamp/0, get_textual_timestamp/1,
		  get_textual_timestamp_for_path/0, get_textual_timestamp_for_path/1,
		  timestamp_to_string/1, string_to_timestamp/1,
		  get_duration/2, get_duration_since/1, get_textual_duration/2,
		  get_precise_timestamp/0, get_precise_duration/2,
		  get_precise_duration_since/1,
		  get_date_after/2 ]).



% Used to be calendar:datetime(), now uses our types:
-type timestamp() :: { date(), time() }.


-type precise_timestamp() :: { unit_utils:megaseconds(), unit_utils:seconds(),
							   unit_utils:microseconds() }.


% Cannot find the definition of the built-in timeout() type:
-type time_out() :: 'infinity' | unit_utils:milliseconds().


% Designates an integer number of seconds since or before Unix time epoch, which
% is 1970-01-01 00:00 UTC.
%
-type posix_seconds() :: integer().


-export_type([ timestamp/0, precise_timestamp/0, time_out/0, posix_seconds/0 ]).




% Month section.



% Canonicalises specified month.
%
-spec canonicalise_month( unit_utils:month() ) -> unit_utils:canonical_month().
canonicalise_month( M ) when is_integer( M ) andalso M >= 0 ->

	% Positive guard useful, as -1 rem 12 = -1 (hence not in [0,11]).

	% In [1;12]:
	case M rem 12 of

		0 ->
			12;

		Other ->
			Other

	end.



% Checks that specified month is a canonical one.
%
-spec check_month_canonical( unit_utils:month() ) -> basic_utils:void().
check_month_canonical( Month ) when is_integer( Month ) andalso Month >= 1
									andalso Month =< 12 ->
	ok;

check_month_canonical( Month ) ->
	throw( { non_canonical_month, Month } ).



% Ensures that the starting canonical month is strictly before the stopping one.
%
-spec check_month_order( unit_utils:absolute_month(),
						  unit_utils:absolute_month() ) -> basic_utils:void().
check_month_order( Start={ StartYear, StartMonth },
				   Stop= { StopYear, StopMonth } ) ->

	check_month_canonical( StartMonth ),
	check_month_canonical( StopMonth ),

	case ( StartYear < StopYear ) orelse ( StartYear =:= StopYear andalso
										   StartMonth < StopMonth ) of

		true ->
			ok;

		_False ->
			throw( { wrong_month_order, Start, Stop } )

	end.



% Converts a month (an integer in [1,12] or a 12-multiple thereof, like 23) into
% its common name.
%
-spec month_to_string( unit_utils:month() ) -> string().
month_to_string( _MonthIndex=1 ) ->
	"January";

month_to_string( _MonthIndex=2 ) ->
	"February";

month_to_string( _MonthIndex=3 ) ->
	"March";

month_to_string( _MonthIndex=4 ) ->
	"April";

month_to_string( _MonthIndex=5 ) ->
	"May";

month_to_string( _MonthIndex=6 ) ->
	"June";

month_to_string( _MonthIndex=7 ) ->
	"July";

month_to_string( _MonthIndex=8 ) ->
	"August";

month_to_string( _MonthIndex=9 ) ->
	"September";

month_to_string( _MonthIndex=10 ) ->
	"October";

month_to_string( _MonthIndex=11 ) ->
	"November";

month_to_string( _MonthIndex=12 ) ->
	"December";

month_to_string( MonthIndex ) ->
	month_to_string( canonicalise_month( MonthIndex ) ).






% Returns the common name of a day (ex: "Tuesday") based on the specified date
% or on the specified index in the week.
%
-spec week_day_to_string( date() | day_index() ) -> string().
week_day_to_string( Date={ _Y, _M, _D } ) ->
	Day = calendar:day_of_the_week( Date ),
	week_day_to_string( Day ) ;

week_day_to_string( _DayIndex=1 ) ->
	"Monday";

week_day_to_string( _DayIndex=2 ) ->
	"Tuesday";

week_day_to_string( _DayIndex=3 ) ->
	"Wednesday";

week_day_to_string( _DayIndex=4 ) ->
	"Thursday";

week_day_to_string( _DayIndex=5 ) ->
	"Friday";

week_day_to_string( _DayIndex=6 ) ->
	"Saturday";

week_day_to_string( _DayIndex=7 ) ->
	"Sunday";

week_day_to_string( DayIndex ) ->
	week_day_to_string( ( DayIndex rem 7 ) + 1 ).



% Date section.


% Checks that specified date is a canonical one.
%
-spec check_date_canonical( date() ) -> basic_utils:void().
check_date_canonical( _Date={ Year, Month, Day } ) when
	  is_integer( Year ) andalso is_integer( Month ) andalso
	  is_integer( Day ) andalso Month >= 1 andalso Month =< 12
	  andalso Day >= 1 andalso Day =< 31 ->
	ok;

check_date_canonical( Date ) ->
	throw( { non_canonical_date, Date } ).




% Compares the specified two dates: tells whether the first date is strictly
% before, after or the same as the second one.
%
% Note: both dates are expected to be in canonical form (ex: not more 12 months
% or 31 days in the specified date).
%
-spec compare_dates( date(), date() ) -> basic_utils:comparison_result().
compare_dates( FirstDate, SecondDate ) ->

	check_date_canonical( FirstDate ),
	check_date_canonical( SecondDate ),

	compare_helper( FirstDate, SecondDate ).



compare_helper( _FirstDate={ Yf, _Mf, _Df },
				_SecondDate={ Ys, _Ms, _Ds } ) when Yf < Ys ->
	lower;

compare_helper( _FirstDate={ Yf, _Mf, _Df },
				_SecondDate={ Ys, _Ms, _Ds } ) when Yf > Ys ->
	higher;

% From here, Yf =:= Ys:
compare_helper( _FirstDate={ _Y, Mf, _Df },
				_SecondDate={ _Y, Ms, _Ds } )  when Ms < Mf ->
	lower;

compare_helper( _FirstDate={ _Y, Mf, _Df },
				_SecondDate={ _Y, Ms, _Ds } ) when Ms > Mf ->
	higher;

% From here, Yf =:= Ys and Mf =:= Ms:
compare_helper( _FirstDate={ _Y, _M, Df },
				_SecondDate={ _Y, _M, Ds } )  when Df < Ds ->
	lower;

compare_helper( _FirstDate={ _Y, _M, Df },
				_SecondDate={ _Y, _M, Ds } ) when Df > Ds ->
	higher;

% Df =:= Ds, equality:
%compare_helper( _FirstDate={ _Y, _M, _D },
%				_SecondDate={ _Y, _M, _D } ) ->
compare_helper( _FirstDate, _SecondDate ) ->
	equal.



% Ensures that the starting canonical date is strictly before the stopping one.
%
% Note: both dates are expected to be in canonical form (ex: not more than 12
% months or 31 days in the specified date).
%
-spec check_date_order( date(), date() ) -> basic_utils:void().
check_date_order( StartDate, StopDate ) ->

	case compare_dates( StartDate, StopDate ) of

		lower ->
			ok;

		% Equal or higher:
		_ ->
			throw( { wrong_date_order, StartDate, StopDate } )

	end.



% Returns the signed duration, in days, between the two specified dates.
%
-spec get_date_difference( date(), date() ) -> unit_utils:days().
get_date_difference( FirstDate, SecondDate ) ->

	FirstDayCount = calendar:date_to_gregorian_days( FirstDate ),
	SecondDayCount = calendar:date_to_gregorian_days( SecondDate ),

	SecondDayCount - FirstDayCount.



% Time conversion section, based only on rather approximated values (ex: the
% number of days varies from a year to another, so any constant value cannot be
% accurate).


% Converts a duration in years into a duration in seconds, supposing a year has
% 365 days and 6 hours (i.e. a quarter of one day, to account for leap years).
%
-spec years_to_seconds( unit_utils:years() ) -> unit_utils:float_seconds().
years_to_seconds( YearDuration ) ->
	% 365.25 days per year one average here:
	YearDuration * 365.25 * 24 * 3600.


% Converts a duration in months into a duration in seconds, supposing a month is
% 1/12 of an average year.
%
-spec months_to_seconds( unit_utils:months() ) -> unit_utils:float_seconds().
months_to_seconds( MonthDuration ) ->
	MonthDuration * 365.25 / 12 * 24 * 3600.


% Converts a duration in weeks into a duration in seconds.
%
-spec weeks_to_seconds( unit_utils:weeks() ) -> unit_utils:seconds().
weeks_to_seconds( WeekDuration ) ->
	WeekDuration * 7 * 24 * 3600.


% Converts a duration in days into a duration in seconds.
%
-spec days_to_seconds( unit_utils:days() ) -> unit_utils:seconds().
days_to_seconds( DayDuration ) ->
	DayDuration * 24 * 3600.


% Converts a duration in hours into a duration in seconds.
%
-spec hours_to_seconds( unit_utils:hours() ) -> unit_utils:seconds().
hours_to_seconds( HourDuration ) ->
	HourDuration * 3600.




% Timestamp section.


% Timestamp-related functions.


% Returns a tipmestamp tuple describing the current time.
%
% Ex: { {Year,Month,Day}, {Hour,Minute,Second} } = time_utils:get_timestamp()
% may return '{ {2007,9,6}, {15,9,14} }'.
%
-spec get_timestamp() -> timestamp().
get_timestamp() ->
	% Was: { erlang:date(), erlang:time() }.
	% Better:
	erlang:localtime().



% Returns a string corresponding to the current timestamp, like:
% "2009/9/1 11:46:53".
%
-spec get_textual_timestamp() -> string().
get_textual_timestamp() ->
	get_textual_timestamp( get_timestamp() ).


% Returns a string corresponding to the specified timestamp, like:
% "2009/9/1 11:46:53".
%
-spec get_textual_timestamp( timestamp() ) -> string().
get_textual_timestamp( { { Year, Month, Day }, { Hour, Minute, Second } } ) ->
	io_lib:format( "~p/~p/~p ~B:~2..0B:~2..0B",
				   [ Year, Month, Day, Hour, Minute, Second ] ).



% Returns a string corresponding to the current timestamp and able to be a part
% of a path, like: "2010-11-18-at-13h-30m-35s.".
%
-spec get_textual_timestamp_for_path() -> string().
get_textual_timestamp_for_path() ->
	get_textual_timestamp_for_path( get_timestamp() ).


% Returns a string corresponding to the specified timestamp and able to be a
% part of a path, like: "2010-11-18-at-13h-30m-35s.".
%
-spec get_textual_timestamp_for_path( timestamp() ) -> string().
get_textual_timestamp_for_path( { { Year, Month, Day },
								  { Hour, Minute, Second } } ) ->
	io_lib:format( "~p-~p-~p-at-~Bh-~2..0Bm-~2..0Bs",
				   [ Year, Month, Day, Hour, Minute, Second ] ).



% Alias of get_textual_timestamp/1, defined for clarity.
%
-spec timestamp_to_string( timestamp() ) -> string().
timestamp_to_string( Timestamp ) ->
	get_textual_timestamp( Timestamp ).



% Parses back a timestamp in the form of "14/4/2011 18:48:51" into a
% timestamp(), i.e. { _Date={Year,Month,Day}, _Time={Hour,Minute,Second} }.
%
-spec string_to_timestamp( string() ) -> timestamp().
string_to_timestamp( TimestampString ) ->

	case string:tokens( TimestampString, _Sep=" :/" ) of

		[ DayString, MonthString, YearString, HourString, MinuteString,
		  SecondString ] ->

			Day   = text_utils:string_to_integer( DayString ),
			Month = text_utils:string_to_integer( MonthString ),
			Year  = text_utils:string_to_integer( YearString ),

			Hour   = text_utils:string_to_integer( HourString ),
			Minute = text_utils:string_to_integer( MinuteString ),
			Second = text_utils:string_to_integer( SecondString ),

			 { { Year, Month, Day }, { Hour, Minute, Second } };

		_ ->
			throw( { timestamp_parsing_failed, TimestampString } )

	end.



% Returns the (signed) duration in seconds between the two specified timestamps,
% using the first one as starting time and the second one as stopping time.
%
-spec get_duration( timestamp(), timestamp() ) -> unit_utils:seconds().
get_duration( FirstTimestamp, SecondTimestamp ) ->

	First  = calendar:datetime_to_gregorian_seconds( FirstTimestamp ),

	Second = calendar:datetime_to_gregorian_seconds( SecondTimestamp ),

	Second - First.



% Returns the (signed) duration in seconds between the specified start timestamp
% and the current time.
%
-spec get_duration_since( timestamp() ) -> unit_utils:seconds().
get_duration_since( StartTimestamp ) ->
	get_duration( StartTimestamp, get_timestamp() ).



% Returns a textual description of the duration between the two specified
% timestamps.
%
% See also: text_utils:duration_to_string/1, which is smarter.
%
-spec get_textual_duration( timestamp(), timestamp() ) -> string().
get_textual_duration( FirstTimestamp, SecondTimestamp ) ->

	{ Days, { Hour, Minute, Second } } = calendar:seconds_to_daystime(
		get_duration( FirstTimestamp, SecondTimestamp ) ),

	lists:flatten( io_lib:format( "~B day(s), ~B hour(s), ~B minute(s) "
								  "and ~B second(s)",
								  [ Days, Hour, Minute, Second ] ) ).




% Returns a timestamp that is as precise as possible: {MegaSecs,Secs,MicroSecs},
% where:
%
% - MegaSecs is an integer number of millions of seconds
%
% - Secs is an integer number of second which is less than one million
%
% - MicroSecs is an integer number of microseconds
%
-spec get_precise_timestamp() -> precise_timestamp().
get_precise_timestamp() ->
	% Was initially: erlang:now().
	% os:timestamp() was then a bit lighter (not monotonic)
	%
	% Finally preferred (still not monotonic), since release 18.0:
	%
	erlang:timestamp().



% Returns the (signed) duration in milliseconds between the two specified
% precise timestamps (as obtained thanks to get_precise_duration/0), using the
% first one as starting time and the second one as stopping time.
%
-spec get_precise_duration( precise_timestamp(), precise_timestamp() ) ->
								  unit_utils:milliseconds().
get_precise_duration( _FirstTimestamp={ A1, A2, A3 },
					  _SecondTimestamp={ B1, B2, B3 } ) ->

	% Seconds to be converted in milliseconds:
	1000 * ( ( B1 - A1 ) * 1000000 + B2 - A2 ) + round( ( B3 - A3 ) / 1000 ).



% Returns the (signed) duration in milliseconds between the specified precise
% timestamp (as obtained thanks to get_precise_duration/0) and the current time.
%
-spec get_precise_duration_since( precise_timestamp() ) ->
								  unit_utils:milliseconds().
get_precise_duration_since( StartTimestamp ) ->
	get_precise_duration( StartTimestamp, get_precise_timestamp() ).



% Returns the date corresponding to the specified one augmented of the specified
% number of days (possibly a negative number).
%
-spec get_date_after( date(), unit_utils:days() ) -> date().
get_date_after( BaseDate, Days ) ->

	DayCount = calendar:date_to_gregorian_days( BaseDate ) + Days,

	calendar:gregorian_days_to_date( DayCount ).
