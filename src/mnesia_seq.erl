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
%%% <p>Functions for a Mnesia sort-of sequence generator.</p>
%%%
%%% <h3>Important</h3>
%%%
%%% <p>In order to use this module you need to create the table:</p>
%%% 
%%% <tt>mnesia:create_table(sequence, [{disc_copies, node()}, {attributes, record_info(fields, sequence)}])).</tt>
%%%
%%% <p>Please refer to <tt>../include/mnesia_seq.hrl</tt>.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://sourceforge.net/users/mpquique/]
%%% @version 0.1, {12 May 2004} {@time}.
%%% @end
-module(mnesia_seq).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("mnesia_seq.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([next/1]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec next(Tab) -> int()
%%    Tab = atom()
%%
%% @doc Gets the next value on the sequence for the table <tt>Tab</tt>.
%% @end 
next(Tab) -> mnesia:dirty_update_counter(sequence, Tab, 1).


%%%===================================================================
%%% Internal functions
%%%===================================================================
