%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
%%

%%%
% @doc My Lists Library.
%
% <p>Additional functions for list processing.</p>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1, {19 Feb 2003} {@time}.
% @end
%%
-module(my_lists).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Imports.  
%
% <p>Imports are explicitly made on the function calls.  These lines are
% commented out and only included for informative purposes.</p>
%%--------------------------------------------------------------------
% -import(lists, [delete/2, member/2, reverse/1, seq/2, sort/1]).
% -import(random, [uniform/1]).

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([asearch/2,
         cut/2, 
         has_duplicates/1,
         random/2, 
         random_seq/2,
         search/2,
         split/2,
         to_integer/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
%-export([]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec asearch(APred, List) -> Result
%    List = [term()]
%    Result = {value, Element} | false
%    Element = term()
%
% @doc where
% <ul>
%   <li><tt>APred = fun(X) -> true | approximate | false</tt></li>
% </ul>
%
% <p>Gets the first <tt>Element</tt> on the <tt>List</tt> where
% <tt>APred(Element) /= false</tt>.</p>
%
% <p><tt>APred</tt> is an <i>approximate</i> predicate that returns
% <tt>true</tt>, <tt>false</tt> or <tt>approximate</tt>.</p>
%
% <p>If matching is ambiguous, there is more than one element on the list
% where the approximate-predicate returns <tt>approximate</tt>, 
% <tt>false</tt> is returned.  This behaviour may be changed in the future
% returning the atom <tt>ambiguous</tt> is such circumstances.</p>
%
% @see my_string:aequal/2
% @see my_string:aequal/6
% @end
%
% %@equiv
%%
asearch(APred, List) ->
    ASearch = fun([], _, ambiguous) -> 
                      false;
                 ([], _, Value) ->
                      Value;
                 ([H|T], F, Value) -> 
                      case APred(H) of
                          true ->
                              {value, H};
                          approximate when Value == false ->
                              % First approximate match...go on searching
                              F(T, F, {value, H});
                          approximate ->  
                              % Keep on searching for an exact match
                              F(T, F, ambiguous);
                          false ->
                              F(T, F, Value)
                      end
              end,
    ASearch(List, ASearch, false).


%%%
% @spec cut(List, Index) -> {Prefix, Suffix}
%    List   = [term()]
%    Index  = int()
%    Prefix = [term()]
%    Suffix = [term()]
%
% @doc Cuts a List by an Index into two sublists.  Prefix ++ Suffix == List.
%
% <p>
% <tt>&gt; my_lists:cut([a, b, c, d, e], 3).
% <br/>{[a, b, c], [d, e]}
% </tt></p>
% @end
%
% %@see
%
% %@equiv
%%
cut(List, Index) when length(List) >= Index, Index >= 0 ->
    cut(List, [], Index).

%%%
% @doc Auxiliary function for cut/2
%
% @see my_lists:cut/2
% @end
%%
cut(Suffix, RevPrefix, 0) ->
    {lists:reverse(RevPrefix), Suffix};

cut([H|T], RevPrefix, N) ->
    cut(T, [H|RevPrefix], N - 1).


%%%
% @spec has_duplicates(List) -> bool()
%    List = term()
%
% @doc Determines if a list has duplicates or not.
% @end
%
% %@see
%
% %@equiv
%%
has_duplicates([]) ->
    false;

has_duplicates([H|T]) ->
    case lists:member(H, T) of
        true ->
            true;
        _False ->
            has_duplicates(T)
    end.
    

%%%
% @spec random(Max, Length) -> List
%    Max    = int()
%    Lenght = int()
%    List   = [int()]
%
% @doc Creates a list of random integers with <tt>Length</tt> elementes.  
%
% <p>Duplicates allowed, the values will be between 1 and <tt>Max</tt> 
% (both included).</p>
%
% @see random_seq/2
% @end
%%
random(Max, Length) ->
    random(Max, Length, []).

%%%
% @doc Auxiliary function for random/2
%
% @see random/2
% @end
%%
random(Max, 0, List) ->
    List;

random(Max, Length, List) ->
    random(Max, Length - 1, [random:uniform(Max)|List]).


%%%
% @spec random_seq(Max, Length) -> List
%    Max    = int()
%    Lenght = int()
%    List   = [int()]
%
% @doc Creates an ordered list of random integers with <tt>Length</tt> 
% elements.  
%
% <p>No duplicates allowed, the values will be between 1 and <tt>Max</tt> 
% (both included).</p>
%
% @see random/2
% @see random_del/3
% @see random_add/3
% @end
%%
random_seq(Max, Length) when (Length * 2) > Max ->
    random_del(Max, Max - Length, lists:seq(1, Max));

random_seq(Max, Length) ->
    lists:sort(random_add(Max, Length, [])).


%%%
% @spec search(Pred, List) -> Result
%    List = [term()]
%    Result = {value, Element} | false
%    Element = term()
%
% @doc where
% <ul>
%   <li><tt>Pred = fun(X) -> bool()</tt></li>
% </ul>
%
% <p>Gets the first <tt>Element</tt> on the <tt>List</tt> where
% <tt>Pred(Element) == true</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
search(Pred, []) ->
    false;

search(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            {value, H};
        _False ->
            search(Pred, T)
    end.


%%%
% @spec split(List, N) -> {List1, List2}
%    List  = [Element]
%    List1 = [Element] 
%    List2 = [Element]
%    Element = term()
%    N = int()
%
% @doc Splits a <tt>List</tt> into two lists where<br/>
% <tt>List == List1 ++ List2</tt> and <tt>length(List1) == N</tt>.
%
% <p>I couldn't find it, but I'm sure this function must already exist.</p>
% @end
%
% %@see
%
% %@equiv
%%
split(List, N) when N >= 0, length(List) >= N ->
    split(List, N, []).

%%%
% @doc Auxiliary function for split/2
%
% @see my_lists:split/2
% @end
%%
split(List, 0, Acc) ->
    {lists:reverse(Acc), List};

split([H|T], N, Acc) ->
    split(T, N-1, [H|Acc]).


%%%
% @spec to_integer(OctetList) -> int()
%    OctetList = [Octet]
%    Octet     = char()
%
% @doc Converts a list of octets to an integer.
%
% <p>Please notice the difference:
% <br/>
% <tt>
% <br/>48&gt; my_lists:to_integer("100").
% <br/>3223600
% <br/>49&gt; list_to_integer("100").
% <br/>100
% <br/>50&gt; my_lists:to_integer("F").
% <br/>70
% <br/>51&gt; list_to_integer("F").
% <br/>=ERROR REPORT==== 21-Mar-2003::12:38:43 ===
% <br/>Error in process &lt;0.118.0&gt; with exit value: {badarg,
% <br/>[{erlang,list_to_integer,["F"]},{erl_eval,expr,3},{erl_eval,exprs,4},
% <br/>{shell,eval_loop,2}]}
% <br/>** exited: {badarg,[{erlang,list_to_integer,["F"]},
% <br/>                    {erl_eval,expr,3},
% <br/>                    {erl_eval,exprs,4},
% <br/>                    {shell,eval_loop,2}]} **
% </tt></p>
%
% @see list_to_integer/1
% @end
%
% %@equiv
%%
to_integer(OctetList) ->
    Size = length(OctetList) * 8,
    <<Value:Size/integer>> = list_to_binary(OctetList),
    Value.


%%%===================================================================
% Internal functions
%%====================================================================
%%%
% @spec random_del(Max, Count, List1) -> List2
%    Max   = int()
%    Count = int()
%    List1 = [int()]
%    List2 = [int()]
%
% @doc Removes <tt>Count</tt> random elements from <tt>List1</tt>.  
% Every removed element will be smaller (or equal) than <tt>Max</tt>.
%
% @see random_add/3
% @see random_seq/2
% @end
%%
random_del(Max, 0, List) ->
    List;

random_del(Max, Count, List1) ->
    List2 = lists:delete(random:uniform(Max), List1),
    random_del(Max, Count - (length(List1) - length(List2)), List2).

%%%
% @spec random_add(Max, Count, List1) -> List2
%
% @doc Adds <tt>Count</tt> random elements to <tt>List1</tt>.  Every 
% added element will be smaller (or equal) than <tt>Max</tt>.  Only NOT 
% members of <tt>List1</tt> are to be added.
%
% @see random_del/3
% @see random_seq/2
% @end
%%
random_add(Max, 0, List) ->
    List;

random_add(Max, Count, List) ->
    Element = random:uniform(Max),
    case lists:member(Element, List) of
        true -> random_add(Max, Count, List);
        _    -> random_add(Max, Count - 1, [Element|List])
    end.
