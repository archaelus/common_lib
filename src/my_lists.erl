%%% Copyright (C) 2003 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc My Lists Library.
%%%
%%% <p>Additional functions for list processing.</p>
%%%
%%% <h2>Changes 0.2 -&gt; 1.0</h2>
%%%
%%% [12 May 2004]
%%%
%%% <ul>
%%%   <li>New functions: <a href="#first-2">first/2</a>, 
%%%     <a href="#is_deep-1">is_deep/1</a>, 
%%%     <a href="#keyindex-3">keyindex/3</a> and 
%%%     <a href="#splitwith-2">splitwith/2</a>.
%%%   </li>
%%% </ul>
%%%
%%% <h2>Changes 1.0 -&gt; 1.1</h2>
%%%
%%% [29 Jul 2004]
%%%
%%% <ul>
%%%   <li>New functions: <a href="#from_number-2">from_number/1</a> and
%%%     <a href="#to_number-1">to_number/1</a>.
%%%   </li>
%%% </ul>
%%%
%%% <h2>Changes 1.1 -&gt; 1.2</h2>
%%%
%%% [10 Dec 2004]
%%%
%%% <ul>
%%%   <li>New function: <a href="#delete-3">delete/3</a> added.</li>
%%% </ul>
%%%
%%% [18 Dec 2004]
%%%
%%% <ul>
%%%   <li>New function: <a href="#intersec-2">intersec/2</a> added.</li>
%%% </ul>
%%%
%%% [21 Dec 2004]
%%%
%%% <ul>
%%%   <li>Functions <a href="#hexlist_to_intlist-1">hexlist_to_intlist/1</a>,
%%%     <a href="#intlist_to_hexlist-1">intlist_to_hexlist/1</a> and
%%%     <a href="#split2-1">split2/1</a>.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {19 Feb 2003} {@time}.
%%% @end
-module(my_lists).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Imports.  
%%%
%%% <p>Imports are explicitly made on the function calls.  These lines are
%%% commented out and only included for informative purposes.</p>
%%%-------------------------------------------------------------------
% -import(lists, [delete/2, member/2, reverse/1, seq/2, sort/1]).
% -import(random, [uniform/1]).

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([asearch/2,
         cut/2, 
         delete/3,
         first/2,
         from_number/1,
         has_duplicates/1,
         hexlist_to_intlist/1,
         intersec/2,
         intlist_to_hexlist/1,
         is_deep/1,
         keyindex/3,
         random/2, 
         random_seq/2,
         search/2,
         split/2,
         split2/1,
         splitwith/2,
         to_integer/1,
         to_number/1]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
%-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec asearch(APred, List) -> Result
%%    List = [term()]
%%    Result = {value, Element} | false
%%    Element = term()
%%
%% @doc Gets the first <tt>Element</tt> on the <tt>List</tt> where
%% <tt>APred(Element) /= false</tt>.
%%
%% <ul>
%%   <li><tt>APred = fun(X) -> true | approximate | false</tt></li>
%% </ul>
%%
%% <p><tt>APred</tt> is an <i>approximate</i> predicate that returns
%% <tt>true</tt>, <tt>false</tt> or <tt>approximate</tt>.</p>
%%
%% <p>If matching is ambiguous, there is more than one element on the list
%% where the approximate-predicate returns <tt>approximate</tt>, 
%% <tt>false</tt> is returned.  This behaviour may be changed in the future
%% returning the atom <tt>ambiguous</tt> is such circumstances.</p>
%%
%% @see my_string:aequal/2
%% @see my_string:aequal/6
%% @end
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


%% @spec cut(List, Index) -> {Prefix, Suffix}
%%    List   = [term()]
%%    Index  = int()
%%    Prefix = [term()]
%%    Suffix = [term()]
%%
%% @doc Cuts a List by an Index into two sublists.  Prefix ++ Suffix == List.
%%
%% <p>
%% <tt>&gt; my_lists:cut([a, b, c, d, e], 3).
%% <br/>{[a, b, c], [d, e]}
%% </tt></p>
%% @end
cut(List, Index) when length(List) >= Index, Index >= 0 ->
    cut(List, [], Index).

%% @doc Auxiliary function for cut/2
%%
%% @see my_lists:cut/2
%% @end
cut(Suffix, RevPrefix, 0) ->
    {lists:reverse(RevPrefix), Suffix};
cut([H|T], RevPrefix, N) ->
    cut(T, [H|RevPrefix], N - 1).


%% @spec delete(Element, N, TupleList1) -> TupleList2
%%    Element = term()
%%    N = int()
%%    TupleList1 = [tuple()]
%%    TupleList2 = [tuple()]
%%
%% @doc Returns a copy of <tt>TupleList1</tt> where the all the occurrences of 
%% a tuple whose <tt>N</tt>th element is <tt>Element</tt> is deleted, if 
%% present.
%% @end 
delete(Element, N, TupleList) ->
    delete(Element, N, TupleList, []).

%% @doc Auxiliary function for delete/3
%%
%% @see delete/3
%% @end 
delete(_Element, _N, [], Acc) ->
    lists:reverse(Acc);
delete(Element, N, [H|T], Acc) when element(N, H) == Element ->
    delete(Element, N, T, Acc);
delete(Element, N, [H|T], Acc) ->
    delete(Element, N, T, [H|Acc]).


%% @spec first(Pred, List) -> Element
%%    List    = [Element]
%%    Element = term()
%%
%% @doc Gets the first <tt>Element</tt> of <tt>List</tt> where <tt>Pred</tt>
%% is <tt>true</tt>.
%%
%% <p>This function fails if no element on <tt>List</tt> satisfies 
%% <tt>Pred</tt>.</p>
%% @end 
first(Pred, [H|T]) ->
    case Pred(H) of
        true  -> H;
        false -> first(Pred, T)
    end.

    
%% @spec from_number(Number) -> List
%%    Number = int() | float()
%%    List   = string()
%%
%% @doc Returns the string representation of a <tt>Number</tt>, integer or
%% float.
%% @end 
from_number(Number) ->
    case catch integer_to_list(Number) of
        {'EXIT', _Reason} ->
            float_to_list(Number);
        L ->
            L
    end.


%% @spec has_duplicates(List) -> bool()
%%    List = term()
%%
%% @doc Determines if a list has duplicates or not.
%% @end
has_duplicates([]) ->
    false;
has_duplicates([H|T]) ->
    case lists:member(H, T) of
        true ->
            true;
        _False ->
            has_duplicates(T)
    end.


%% @spec hexlist_to_intlist(HexList) -> IntList
%%    HexList = [Hex]
%%    Hex = int()
%%    IntList = [int()]
%%
%% @doc Creates an integer list from an hex string.
%%
%% <p>Where <tt>Hex</tt> is 
%% <tt>$1|$2|$3|$4|$5|$6|$7|$8|$9|$A|$B|$C|$D|$E|$F</tt></p>
%%
%% @see intlist_to_hexlist/1
%% @end 
hexlist_to_intlist(List) ->
    lists:map(fun(X) -> httpd_util:hexlist_to_integer(X) end, split2(List)).


%% @spec intersec(List1, List2) -> List3
%%    List1 = [term()]
%%    List2 = [term()]
%%    List3 = [term()]
%%
%% @doc Returns the intersection of two lists.  <tt>List3</tt> contains all
%% elements in <tt>List1</tt> which also belong to <tt>List2</tt>.
%% @end 
intersec(List1, List2) ->
    intersec(List1, List2, []).

%% @doc Auxiliary function for intersec/2
%%
%% @see intersec/2
%% @end 
intersec([], _List2, Acc) ->
    lists:reverse(Acc);
intersec([H|T], List2, Acc) ->
    case lists:member(H, List2) of
        true ->
            intersec(T, List2, [H|Acc]);
        false ->
            intersec(T, List2, Acc)
    end.


%% @spec intlist_to_hexlist(IntList) -> HexList
%%    IntList = [int()]
%%    HexList = [Hex]
%%    Hex = int()
%%
%% @doc Creates an hex string from an integer list.
%%
%% <p>Where <tt>Hex</tt> is <tt>$1|$2|$3|$4|$5|$6|$7|$8|$9|$A|$B|$C|$D|$E|$F
%% </tt></p>
%%
%% @see hexlist_to_intlist/1
%% @end 
intlist_to_hexlist(List) -> 
    intlist_to_hexlist(List, []).

%% @doc Auxiliary function for intlist_to_hexlist/1
%%
%% @see intlist_to_hexlist/1
%% @end 
intlist_to_hexlist([], Acc) ->
    lists:reverse(Acc);
intlist_to_hexlist([0|T], Acc) ->
    intlist_to_hexlist(T, [$0,$0|Acc]);
intlist_to_hexlist([H|T], Acc) when H < 16 ->
    [A] = erlang:integer_to_list(H, 16),
    intlist_to_hexlist(T, [A,$0|Acc]);
intlist_to_hexlist([H|T], Acc) ->
    [A,B] = erlang:integer_to_list(H, 16),
    intlist_to_hexlist(T, [B,A|Acc]).


%% @spec is_deep(List) -> bool()
%%    List    = [Element]
%%    Element = term()
%%
%% @doc Returns <tt>true</tt> if <tt>List</tt> is a deep list, <tt>false</tt>
%% otherwise.
%% @end 
is_deep([H|_]) when list(H) -> true;
is_deep([_|T])              -> is_deep(T);
is_deep(_)                  -> false.


%% @spec keyindex(Key, N, TupleList) -> Index
%%
%% @doc Gets the <tt>Index</tt> of the tuple on <tt>TupleList</tt> whose
%% Nth element is <tt>Key</tt>.
%%
%% <p>The functions fails if no element with such a <tt>Key</tt> exists on
%% <tt>TupleList</tt>.</p>
%% @end 
keyindex(Key, N, TupleList) ->
    keyindex(Key, N, TupleList, 1).

%% @doc Auxiliary function for keyindex/3.
%%
%% @see keyindex/3
%% @end 
keyindex(Key, N, [H|_], Index) when element(N, H) == Key ->
    Index;
keyindex(Key, N, [_|T], Index) ->
    keyindex(Key, N, T, Index + 1).
    

%% @spec random(Max, Length) -> List
%%    Max    = int()
%%    Lenght = int()
%%    List   = [int()]
%%
%% @doc Creates a list of random integers with <tt>Length</tt> elementes.  
%%
%% <p>Duplicates allowed, the values will be between 1 and <tt>Max</tt> 
%% (both included).</p>
%%
%% @see random_seq/2
%% @end
random(Max, Length) ->
    random(Max, Length, []).

%% @doc Auxiliary function for random/2
%%
%% @see random/2
%% @end
random(_Max, 0, List) ->
    List;
random(Max, Length, List) ->
    random(Max, Length - 1, [random:uniform(Max)|List]).


%% @spec random_seq(Max, Length) -> List
%%    Max    = int()
%%    Lenght = int()
%%    List   = [int()]
%%
%% @doc Creates an ordered list of random integers with <tt>Length</tt> 
%% elements.  
%%
%% <p>No duplicates allowed, the values will be between 1 and <tt>Max</tt> 
%% (both included).</p>
%%
%% @see random/2
%% @see random_del/3
%% @see random_add/3
%% @end
random_seq(Max, Length) when (Length * 2) > Max ->
    random_del(Max, Max - Length, lists:seq(1, Max));
random_seq(Max, Length) ->
    lists:sort(random_add(Max, Length, [])).


%% @spec search(Pred, List) -> Result
%%    List = [term()]
%%    Result = {value, Element} | false
%%    Element = term()
%%
%% @doc Gets the first <tt>Element</tt> on the <tt>List</tt> where
%% <tt>Pred(Element) == true</tt>.
%%
%% <ul>
%%   <li><tt>Pred = fun(X) -> bool()</tt></li>
%% </ul>
%% @end
search(_Pred, []) ->
    false;
search(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            {value, H};
        _False ->
            search(Pred, T)
    end.


%% @spec split(List, N) -> {List1, List2}
%%    List  = [Element]
%%    List1 = [Element] 
%%    List2 = [Element]
%%    Element = term()
%%    N = int()
%%
%% @doc Splits a <tt>List</tt> into two lists where<br/>
%% <tt>List == List1 ++ List2</tt> and <tt>length(List1) == N</tt>.
%%
%% <p>I couldn't find it, but I'm sure this function must already exist.</p>
%% @end
split(List, N) when N >= 0, length(List) >= N ->
    split(List, N, []).

%% @doc Auxiliary function for split/2
%%
%% @see my_lists:split/2
%% @end
split(List, 0, Acc) ->
    {lists:reverse(Acc), List};
split([H|T], N, Acc) ->
    split(T, N-1, [H|Acc]).


%% @spec split2(List) -> Chunks
%%    List = [term()]
%%    Chunks = [Chunk]
%%
%% @doc Splits a <tt>List</tt> into two elements lists.
%% <br/><br/>
%% <tt>split2("2AF1B3") == ["2A", "F1", "B3"]</tt>
%% @end
split2(List) ->
    split2(List, []).

%% @doc Auxiliary function for split2/1
%%
%% @see split2/1
%% @end 
split2([], Acc) ->
    lists:reverse(Acc);
split2([H1,H2|T], Acc) ->
    split2(T, [[H1,H2]|Acc]);
split2([H|T], Acc) ->
    split2(T, [[H]|Acc]).


%% @spec splitwith(Pred, List) -> {List1, List2}
%%    List  = [term()]
%%    List1 = [term()]
%%    List2 = [term()]
%%
%% @doc Splits a <tt>List</tt> into two lists where elements on <tt>List1</tt>
%% satisfy <tt>Pred</tt>.  <tt>List2</tt> holds the elements for which
%% <tt>Pred</tt> is <tt>false</tt>.
%%
%% <ul>
%%   <li><tt>Pred = fun(X) -> bool()</tt></li>
%% </ul>
%%
%% <p>Ordering is preserved.</p>
%% @end 
splitwith(Pred, L) ->
    splitwith(Pred, L, [], []).

%% @doc Auxiliary function for splitwith/2.
%%
%% @see splitwith/2
%% @end 
splitwith(_Pred, [], L1, L2) ->
    {lists:reverse(L1), lists:reverse(L2)};
splitwith(Pred, [H|T], L1, L2) ->
    case Pred(H) of
        true  -> splitwith(Pred, T, [H|L1], L2);
        false -> splitwith(Pred, T, L1, [H|L2])
    end.


%% @spec to_integer(OctetList) -> int()
%%    OctetList = [Octet]
%%    Octet     = char()
%%
%% @doc Converts a list of octets to an integer.
%%
%% <p>Please notice the difference:
%% <br/>
%% <tt>
%% <br/>48&gt; my_lists:to_integer("100").
%% <br/>3223600
%% <br/>49&gt; list_to_integer("100").
%% <br/>100
%% <br/>50&gt; my_lists:to_integer("F").
%% <br/>70
%% <br/>51&gt; list_to_integer("F").
%% <br/>=ERROR REPORT==== 21-Mar-2003::12:38:43 ===
%% <br/>Error in process &lt;0.118.0&gt; with exit value: {badarg,
%% <br/>[{erlang,list_to_integer,["F"]},{erl_eval,expr,3},{erl_eval,exprs,4},
%% <br/>{shell,eval_loop,2}]}
%% <br/>** exited: {badarg,[{erlang,list_to_integer,["F"]},
%% <br/>                    {erl_eval,expr,3},
%% <br/>                    {erl_eval,exprs,4},
%% <br/>                    {shell,eval_loop,2}]} **
%% </tt></p>
%%
%% @see list_to_integer/1
%% @end
to_integer(OctetList) ->
    Size = length(OctetList) * 8,
    <<Value:Size/integer>> = list_to_binary(OctetList),
    Value.


%% @spec to_number(List) -> Number
%%    List   = string()
%%    Number = int() | float()
%%
%% @doc Returns the <tt>Number</tt>, integer or float, given the string 
%% representation.
%% @end 
to_number(List) ->                                                       
    case catch list_to_integer(List) of
        {'EXIT', _Reason} ->
            list_to_float(List);
        I -> 
            I
    end.
                      

%%%===================================================================
% Internal functions
%%%===================================================================
%% @spec random_del(Max, Count, List1) -> List2
%%    Max   = int()
%%    Count = int()
%%    List1 = [int()]
%%    List2 = [int()]
%%
%% @doc Removes <tt>Count</tt> random elements from <tt>List1</tt>.  
%% Every removed element will be smaller (or equal) than <tt>Max</tt>.
%%
%% @see random_add/3
%% @see random_seq/2
%% @end
random_del(_Max, 0, List) ->
    List;
random_del(Max, Count, List1) ->
    List2 = lists:delete(random:uniform(Max), List1),
    random_del(Max, Count - (length(List1) - length(List2)), List2).

%% @spec random_add(Max, Count, List1) -> List2
%%
%% @doc Adds <tt>Count</tt> random elements to <tt>List1</tt>.  Every 
%% added element will be smaller (or equal) than <tt>Max</tt>.  Only NOT 
%% members of <tt>List1</tt> are to be added.
%%
%% @see random_del/3
%% @see random_seq/2
%% @end
random_add(_Max, 0, List) ->
    List;
random_add(Max, Count, List) ->
    Element = random:uniform(Max),
    case lists:member(Element, List) of
        true -> random_add(Max, Count, List);
        _    -> random_add(Max, Count - 1, [Element|List])
    end.
