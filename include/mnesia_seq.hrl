%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc Sequence numbers for Mnesia.
%%%
%%% <p>Types for a Mnesia sort-of sequence generator.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://sourceforge.net/users/mpquique/]
%%% @version 0.1, {12 May 2004} {@time}.
%%% @end

-ifndef(mnesia_seq).
-define(mnesia_seq, true).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {sequence, Tab, Value}
%%    Tab   = atom()
%%    Value = int()
%%
%% %@doc Monolithically increasing numbers, starting in 0.  There's one
%% sequence per table.
%%
%% <dl>
%%   <dt>tab: </dt><dd>Table name.</dd>
%%   <dt>value: </dt><dd>Current value of the sequence.</dd>
%% </dl>
%% %@end 
-record(sequence, {tab, value}).

-endif.  % -ifndef(mnesia_seq)

