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
% Creation date: Sunday, December 5, 2021.


% @doc Gathering of management facilities for <b>XML</b> processing.
%
% See xml_utils_test.erl for the corresponding test.
%
% Mostly a wrapper around the standard, always available xmerl modules.
%
-module(xml_utils).


-type xml_text() :: ustring().
% A raw string containing XML (including markup), typically at least an extract
% of an XML document.


-opaque xml_content() :: list().
% An in-memory representation of an XML content, mostly as a tree of markup
% elements, typically expressed in the "simple-form" xmerl format, that is a
% mixture of simple-form ({Tag, Attributes, Content}, {Tag, Content} or Tag),
% and xmerl records (as xmlElement and xmlText).


-export_type([ xml_text/0, xml_content/0 ]).


-export([ to_xml_text/1, xml_to_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().


% Always available with the standard Erlang distribution:
-include_lib("xmerl/include/xmerl.hrl").


% @doc Escapes specified text (not expected to contain markup), so that it can
% be included safely within an XML content.
%
-spec to_xml_text( any_string() ) -> ustring().
to_xml_text( Text ) ->
	% As our HTML escaping goes a little beyond the strictly necessary and
	% matches the XML requirements:
	%
	web_utils:escape_as_html_content( Text ).



% @doc Returns a string corresponding to the specified XML content.
%
% The returned serialised form is ready to be written on a file, sent over the
% network, etc.
%
-spec xml_to_string( xml_content() ) -> ustring().
xml_to_string( XMLContent ) ->

   % See
   % https://www.erlang.org/doc/apps/xmerl/xmerl_ug.html#example--create-xml-out-of-arbitrary-data:

	RootElem = #xmlElement{ content=XMLContent },
	xmerl:export_simple( [ RootElem ], xmerl_xml ).
