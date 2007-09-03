%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc Binaries library.
%%%
%%% <p>My own binary-manipulation functions.</p>
%%%
%%% <h2>Changes 1.1 -&gt; 1.2</h2>
%%%
%%% [21 Dec 2004]
%%%
%%% <ul>
%%%   <li>Functions <a href="#from_hexlist-1">from_hexlist/1</a> and 
%%%     <a href="#to_hexlist-1">to_hexlist/1</a> added.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0, {18 Feb 2004} {@time}.
%%% @end
-module(binary).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([from_hexlist/1, to_hexlist/1, take_until/3]).

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
%% @spec from_hexlist(HexList) -> Bin
%%    HexList = [Hex]
%%    Bin = binary()
%%    Hex = int()
%%
%% @doc Creates a binary from an hex list.
%%
%% <p><tt>Hex</tt> is <tt>$1|$2|$3|$4|$5|$6|$7|$8|$9|$A|$B|$C|$D|$E|$F</tt>
%% </p>
%%
%% @see to_hexlist/1
%% @see my_lists:hexlist_to_intlist/1
%% @end 
from_hexlist(HexList) ->
    list_to_binary(my_lists:hexlist_to_intlist(HexList)).


%% @spec to_hexlist(Bin) -> HexList
%%    Bin = binary() | [binary()]
%%    HexList = [Hex]
%%    Hex = int()
%%
%% @doc Creates an hex list from a binary.
%%
%% <p><tt>Hex</tt> is <tt>$1|$2|$3|$4|$5|$6|$7|$8|$9|$A|$B|$C|$D|$E|$F</tt>
%% </p>
%%
%% @see from_hexlist/1
%% @see my_lists:intlist_to_hexlist/1
%% @end 
to_hexlist(Bin) when is_binary(Bin) -> 
    my_lists:intlist_to_hexlist(binary_to_list(Bin));
to_hexlist(Bins) when is_list(Bins) ->
    lists:append(lists:map(fun to_hexlist/1, Bins)).

%% @spec take_until(Binary, Pattern, Size) -> Result
%%    Binary       = bin()
%%    Pattern      = bin()
%%    Size         = int()
%%    Result       = {ok, UntilPattern, Rest}                 | 
%%                   {error, {not_found, Pattern, UntilSize}}
%%    UntilPattern = bin()
%%    Rest         = bin()
%%    Prefix       = bin()
%%
%% @doc Gets the leading octets of a <tt>Binary</tt> until <tt>Pattern</tt> is 
%% found or <tt>Size</tt> octets are inspected.
%%
%% <p>If found before <tt>Size</tt> bytes are consumed, a binary with the 
%% leading octets <tt>UntilPattern</tt> is returned along the remainder of the 
%% binary.</p>
%%
%% <p><tt>Pattern</tt> is <b>not</b> included in the response, <b>nor</b> 
%% removed from <tt>Binary</tt>.</p>
%%
%% <p>If <tt>Pattern</tt> is not found within the first <tt>Size</tt> octets of
%% <tt>Binary</tt>, an error is reported, <tt>Pattern</tt> and the first 
%% <tt>Size</tt> bytes are returned as additional information.</p>
%%
%% @see take_until/4
%% @end
take_until(Binary, Pattern, Size) ->
    case take_until(Binary, Pattern, Size, []) of
        not_found ->
            MinSize = lists:min([Size, size(Binary)]),
            <<UntilSize:MinSize/binary-unit:8, _Rest/binary>> = Binary,
            {error, {not_found, Pattern, UntilSize}};
        {ok, UntilPattern, Rest} ->
            {ok, UntilPattern, Rest}
    end.

%% @doc Auxiliary function for take_until/3
%%
%% @see take_until/3
%% @end
take_until(<<>>, _Pattern, _Size, _Acc) ->
    not_found;
take_until(_Binary, Pattern, Size, _Acc) when size(Pattern) > Size ->
    not_found;
take_until(Binary, Pattern, Size, Acc) ->
    Len = size(Pattern),
    case Binary of
        <<Pattern:Len/binary-unit:8, _Rest/binary>> ->
            {ok, list_to_binary(lists:reverse(Acc)), Binary};
        <<Octet:8, Rest/binary>> ->
            take_until(Rest, Pattern, Size - 1, [Octet|Acc])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
