% Copyright (C) 2020-2025 Olivier Boudeville
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
% Creation date: Thursday, July 16, 2020.

-module(locale_utils).

-moduledoc """
Gathering of various convenient facilities regarding the support of various
**locales** (e.g. for the management of per-country bank holidays) and related
character sets.
""".



-doc "Type to designate countries of interest.".
-type country() :: 'france' | 'united_kingdom' | 'usa' | 'china' | 'japan'
				   | 'unreferenced_country' | atom().



-doc """
A locale as a BCP 47 plain string, like "fr-FR", corresponding to "French
(France)" here.

See <http://cldr.unicode.org/> and
<https://en.wikipedia.org/wiki/IETF_language_tag> for further details.
""".
-type string_locale() :: ustring().



-doc """
A locale as a BCP 47 binary, like `<<"fr-FR">>`, corresponding to "French
(France)" here.

See <http://cldr.unicode.org/> and
<https://en.wikipedia.org/wiki/IETF_language_tag> for further details.

""".
-type bin_locale() :: bin_string().



-doc """
A locale as a BCP 47 plain string or binary, like "fr-FR" or `<<"fr-FR">>`,
corresponding to "French (France)" here.

See <http://cldr.unicode.org/> and
<https://en.wikipedia.org/wiki/IETF_language_tag> for further details.
""".
-type any_locale() :: any_string().



-doc """
A description of a locale (e.g. "Afrikaans (South Africa)").
""".
-type locale_description() :: ustring().



-doc """
A description of a locale (e.g. `<<"Afrikaans (South Africa)">>`).
""".
-type bin_locale_description() :: bin_string().



-doc """
A locale with a character set, like "fr_FR.UTF-8".
""".
-type locale_charset() :: ustring().


-export_type([ country/0,
			   string_locale/0, bin_locale/0, any_locale/0,
			   locale_description/0, bin_locale_description/0,
			   locale_charset/0 ]).


-export([ get_locale_charset/0 ]).



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().



-doc """
Returns the current locale with a character set, like "fr_FR.UTF-8".
""".
-spec get_locale_charset() -> ustring().
get_locale_charset() ->
	case system_utils:get_environment_variable( "LC_ALL" ) of

		R when R =:= false orelse R =:="" ->
			case system_utils:get_environment_variable( "LANG" ) of

				R when R =:= false orelse R =:="" ->
					BaseLocale = "en_US.UTF-8",
					trace_utils:warning_fmt( "Unable to determine the current "
						"locale, assuming '~ts'.", [ BaseLocale ] ),
					BaseLocale;

				R ->
					R

			end;

		R ->
			R

	end.
