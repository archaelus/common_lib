%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@udc.es>
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
%%

%%%
% @doc My string library.
%
% <p>Additional functions for string processing.</p>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1, {23 Sep 2003} {@time}.
% @end
%%
-module(my_string).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Imports.  
%
% <p>Imports are explicitly made on the function calls.  These lines are
% commented out and only included for informative purposes.</p>
%%--------------------------------------------------------------------
% -import(lists, [dropwhile/2, map/2, member/2, reverse/1, splitwith/2]

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([aequal/2,
         aequal/6,
         chop_token/2, 
         chop_tokens/3,
         lowercase/1,
         replace_chars/3,
         split_by_word/2, 
         strip/2, 
         strip/3,
         uppercase/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([]).

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
% @spec aequal(String1, Pattern) -> Result
%    String1 = string()
%    Pattern = string()
%    Result = true | approximate | false
%
% @doc Approximate matching of strings.  Returs <tt>true</tt> if
% <tt>String1</tt> is equal to <tt>Pattern</tt> allowing 1 error
% (substitution, insertion, deletion or transposition).
%
% <p><tt>&gt; my_string:aequal("hello", "helo").<br/>
% approximate
% </tt></p>
%
% @see aequal/6
% @end
%
% %@equiv
%%
aequal(String, Pattern) ->
    case aequal(String, Pattern, 1, 0, 0, 0) of % Substitution
        false ->
            case aequal(String, Pattern, 0, 0, 1, 0) of % Deletion
                false ->
                    case aequal(String, Pattern, 0, 0, 0, 1) of %Transposition
                        false ->
                            aequal(String, Pattern, 0, 1, 0, 0); % Insertion
                        WithTransposition ->
                            WithTransposition
                    end;
                WithDeletion ->
                    WithDeletion
            end;
        WithSubstitution ->
            WithSubstitution
    end.


%%%
% @spec aequal(String, Pattern, Subs, Ins, Dels, Trans) -> Result
%    String = string()
%    Pattern = string()
%    Subs = int() 
%    Ins = int()
%    Dels = int()
%    Trans = int()
%    Result = true | approximate | false
%
% @doc Approximate matching of strings.  Returs <tt>true</tt> if
% <tt>String</tt> is equal to <tt>Pattern</tt> allowing 
% <tt>Subs</tt> substitution errors, <tt>Ins</tt> insertion errors,
% <tt>Dels</tt> deletion errors and <tt>Trans</tt> transposition
% errors.
%
% @see aequal/2
% @end
%
% %@equiv
%%
aequal(_String, _Pattern, S, I, D, T) when S < 0; I < 0; D < 0; T < 0 ->
    false;

aequal(String, Pattern, S, I, D, T) ->
    case common_prefix(String, Pattern) of
        {[], [], _Prefix} ->
            true;
        {[], Suffix2, _Prefix} when length(Suffix2) =< D ->
            approximate;
        {[], _Suffix2, _Prefix} ->
            false;
        {Suffix1, [], _Prefix} when length(Suffix1) =< I ->
            approximate;
        {_Suffix, [], _Prefix} ->
            false;
        {[X,Y|Suffix1], [Y,X|Suffix2], _Prefix} ->
            case aequal(Suffix1, Suffix2, S, I, D, T-1) of
                false ->
                    % Instead of a transposition it could be a deletion...
                    case aequal([X,Y|Suffix1], [X|Suffix2], S, I, D-1, T) of
                        false ->
                            % ...an insertion...
                            case aequal([Y|Suffix1],[Y,X|Suffix2],S,I-1,D,T) of
                                false ->
                                    % ...or even a couple of substitutions
                                    case aequal(Suffix1,Suffix2, S-2,I,D,T) of
                                        true ->
                                            approximate;
                                        Other ->
                                            Other
                                    end;
                                _WithInsertion ->
                                    approximate
                            end;
                        _WithDeletion ->
                            approximate
                    end;
                _WithTransposition ->
                    approximate
            end;
        {[H1|Suffix1], [H2|Suffix2], _Prefix} ->
            case aequal(Suffix1, Suffix2, S-1, I, D, T) of
                false ->
                    case aequal([H1|Suffix1], Suffix2, S, I, D-1, T) of
                        false ->
                            case aequal(Suffix1, [H2|Suffix2], S, I-1, D, T) of
                                true ->
                                    approximate;
                                Other ->
                                    Other
                            end;
                        _WithDeletion ->
                            approximate
                    end;
                _WithSubstitution ->
                    approximate
            end
    end.


%%%
% @spec chop_token(String, SeparatorList) -> {Token, RestOfString}
%    String = string()
%    SeparatorList = string()
%    Token = string()
%    RestOfString = string()
%
% @doc Gets the first token in <tt>String</tt>, separated by the
% characters in <tt>SeparatorList</tt>.
%
% @see string:tokens/2
% @see string:sub_word/2
% @see chop_tokens/3
% @end
%
% %@equiv
%%
chop_token(String, SeparatorList) ->
    Stripped = strip(String, left, SeparatorList),
    lists:splitwith(fun(X) -> not lists:member(X,SeparatorList) end, Stripped).


%%%
% @spec chop_tokens(String, N, SeparatorList) -> {Tokens, RestOfString}
%    String = string()
%    SeparatorList = string()
%    RestOfString = string()
%    Tokens = [string()]
%    N = int()
%
% @doc Gets the first <tt>N</tt> tokens in <tt>String</tt>, separated 
% by the characters in <tt>SeparatorList</tt>.  If there are less than 
% <tt>N</tt> tokens the tuple <tt>{AvailableTokens, "" }</tt> is
% returned.
%
% @see string:tokens/2
% @see string:sub_word/2
% @see chop_token/2
% @end
%
% %@equiv
%%
chop_tokens(String, N, SeparatorList) when N > 0 ->
    chop_tokens(String, N, SeparatorList, []).

%%%
% @doc Auxiliary function for chop_tokens/3
%
% @see chop_tokens/3
% @end
%%
chop_tokens(String, N, SeparatorList, Tokens) when String == ""; N == 0 ->
    {lists:reverse(Tokens), String};

chop_tokens(String, N, SeparatorList, Tokens) ->
    {Token, RestOfString} = chop_token(String, SeparatorList),
    chop_tokens(RestOfString, N-1, SeparatorList, [Token|Tokens]).


%%%
% @spec lowercase(String) -> LString
%    String  = string()
%    LString = string()
%
% @doc Returns a new <tt>LString</tt> where every letter is in lowercase.
%
% @see uppercase/1
% @end
%
% %@equiv
%%
lowercase(String) ->
    ToLower = fun(L) when L >= $A, L =< $Z -> L + 32;
                 (209)                     -> 241;
                 (Other)                   -> Other
              end,
    lists:map(ToLower, String).


%%%
% @spec replace_chars(String1, Characters, Char) -> String2
%    String1 = string()
%    String2 = string()
%    Characters = string()
%    Char = char()
%
% @doc Replaces on <tt>String1</tt> every character in 
% <tt>Characters</tt> list by <tt>Char</tt>.
%
% <p><tt>&gt; replace_chars("hello_world.", "_.", $ ).
% <br/>"hello world "
% </tt></p>
% @end
%
% %@see
%
% %@equiv
%%
replace_chars(String, Characters, Char) ->
    Replace = fun(X) -> case lists:member(X, Characters) of
                            true   -> Char;
                            _False -> X
                        end
              end,
    lists:map(Replace, String).


%%%
% @spec split_by_word(String, Number) -> {Prefix, Suffix}
%    String = string()
%    Prefix = string()
%    Suffix = string()
%    Number = int()
%
% @doc Splits a <tt>String</tt> into two parts by a given word.  
% <tt>Prefix</tt> contains <tt>Number</tt> words and
% <tt>Suffix</tt> the rest of the string.  Words are separated by blanks.
% <br/><br/>
% <tt>String == Prefix ++ Suffix</tt>
% @end
% 
% %@see
%
% %@equiv
%%
split_by_word(String, N) ->
    split_by_word(String, N, []).

%%%
% @doc Auxiliary function for split_by_word/2
%
% @see split_by_word/2
% @end
%%
split_by_word(String, N, Acc) when String == ""; N =< 0 ->
    {lists:reverse(Acc), String};

split_by_word([H1,H2|T], N, Acc) when H1 /= $ , H2 == $ ->
    split_by_word([H2|T], N-1, [H1|Acc]);

split_by_word([H|T], N, Acc) ->
    split_by_word(T, N, [H|Acc]).


%%%
% @spec strip(String, SeparatorList) -> Stripped
%
% @equiv my_string:strip(String, both, SeparatorList)
% @end
%%
strip(String, SeparatorList) ->
    strip(String, both, SeparatorList).


%%%
% @spec strip(String, Direction, SeparatorList) -> Stripped
%    String = string()
%    Stripped = string()
%    Direcction = left | right | both
%    SeparatorList = string()
%
% @doc Returns a string, where leading and/or trailling characters in
% <tt>SeparatorList</tt> have been removed.  <tt>Direction</tt> can
% be <tt>left</tt>, <tt>right</tt> or <tt>both</tt> and indicates
% from which direction characters are to be removed.  The function strip/2 is
% equivalent to strip(String, both, SeparatorList).
%
% @see string:strip/1
% @see string:strip/2
% @see string:strip/3
% @end
%
% %@equiv
%%
strip(String, left, SeparatorList) ->
    lists:dropwhile(fun(X) -> lists:member(X, SeparatorList) end, String);

strip(String, right, SeparatorList) ->
    lists:reverse(strip(lists:reverse(String), left, SeparatorList));

strip(String, both, SeparatorList) ->
    strip(strip(String, left, SeparatorList), right, SeparatorList).


%%%
% @spec uppercase(String) -> UString
%    String = string()
%    UString = string()
%
% @doc Returns a new <tt>UString</tt> where every letter is in uppercase.
%
% @see lowercase/1
% @end
%
% %@equiv
%%
uppercase(String) ->
    ToUpper = fun(L) when L >= $a, L =< $z -> L - 32;
                 (241)                     -> 209;
                 (Other)                   -> Other
              end,
    lists:map(ToUpper, String).

%%%===================================================================
% Internal functions
%%====================================================================
%%%
% @spec common_prefix(String1, String2) -> {Suffix1, Suffix2, Prefix}
%    String1 = string()
%    String2 = string()
%    Suffix1 = string()
%    Suffix2 = string()
%    Prefix  = string()
%
% @doc Gets the common prefix of two strings.
%
% <p><tt>
% Prefix ++ Suffix1 = String1
% Prefix ++ Suffix2 = String2
% </tt></p>
% @end
%
% %@see
%
% %@equiv
%%
common_prefix(String1, String2) ->
    common_prefix(String1, String2, []).


%%%
% @doc Auxiliary function for common_prefix/2
%
% @see common_prefix/2
% @end
%%
common_prefix([H|Suffix1], [H|Suffix2], Prefix) ->
    common_prefix(Suffix1, Suffix2, [H|Prefix]);

common_prefix(Suffix1, Suffix2, Prefix) ->
    {Suffix1, Suffix2, lists:reverse(Prefix)}.

