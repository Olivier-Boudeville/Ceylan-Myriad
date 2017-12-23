% Copyright (C) 2003-2017 Olivier Boudeville
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
% Creation date: Tuesday, January 29, 2013


% Header to export gui-related defines.
%
% See gui.erl for the corresponding implementation.


% Context sent together with an event, which can be most of the time ignored.
%
-record( gui_event_context, {


		   % The identifier of the event source (generally not useful as the
		   % gui_object is enough):
		   %
		   id :: gui:id(),


		   % Usually of no use, as the user has been means of preserving a state
		   % (ex: in the main loop)
		   %
		   user_data = [] :: gui:user_data(),


		   % The lower-level event (if any) resulting in our event:
		   %
		   % (useful for example when deciding to propagate it upward in the
		   % widget hierarchy)
		   %
		   backend_event = undefined :: gui:backend_event()


}).

-type gui_event_context() :: #gui_event_context{}.
