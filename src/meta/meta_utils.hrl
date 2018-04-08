% Copyright (C) 2014-2018 Olivier Boudeville (olivier.boudeville@esperide.com)
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Tuesday, December 30, 2014.



% For all modules being bootstrap ones, the 'table' pseudo-module is not
% available (as these modules are not processed by the 'Common' parse
% transform).
%
% So no table pseudo-module can be available for them, only ?table is available
% - not the other *_table counterparts (once these bootstrapped modules are
% compiled, if they relied on foo_hashtable, then the parse transform could not
% operate on any module compiled before foo_hashtable):
%
-define( table, map_hashtable ).
