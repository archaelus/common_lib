%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
% @doc My string library.
%
% <p>Additional functions for string processing.</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [18 Feb 2004]
%
% <ul>
%   <li>Functions <a href="#is_dec-1">is_dec/1</a> and <a href="#is_hex-1">
%     is_hex/1</a> added.</li>
%   <li>Functions <a href="#is_atime-1">is_atime/1</a> and 
%     <a href="#is_rtime-1">is_rtime/1</a> added.
%   </li>
% </ul>
%
%
% <h2>Changes 0.2 -&gt; 0.3</h2>
%
% [17 Mar 2004]
%
% <ul>
%   <li>Added the function <a href="#normalize-1">normalize/1</a>.</li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
         is_dec/1,
         is_hex/1,
         is_atime/1,
         is_rtime/1,
         lowercase/1,
		 normalize/1,
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
%% These macros were borrowed from xmerl.hrl
-define(SPACE, 32).
-define(CR,    13).
-define(LF,    10).
-define(TAB,    9).

%% whitespace consists of 'space', 'carriage return', 'line feed' or 'tab'
%% comment out this line to regerate edocs. Don't know why ???
-define(WHITESPACE(H), H == ?SPACE; H == ?CR; H == ?LF; H == ?TAB).
-define(WHITESPACES, [?SPACE, ?CR, ?LF, ?TAB]).

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
% @spec is_dec(String) -> bool()
%    String = string()
%
% @doc Checks if String is a sequence of decimal digits, every character
% belongs to [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9].
%
% <p>If <tt>String</tt> is empty, <tt>true</tt> is returned.</p>
%
% @see is_hex/1
% @end
%
% %@equiv
%%
is_dec([]) ->
    true;

is_dec([Digit|Rest]) when (Digit >= 48) and (Digit =< 57) ->
    is_dec(Rest);

is_dec(_String) ->
    false.


%%%
% @spec is_hex(String) -> bool()
%    String = string()
%
% @doc Checks if String is a sequence of hexadecimal digits, every character
% belongs to [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F,
% $a, $b, $c, $d, $e, $f].
%
% <p>If <tt>String</tt> is empty, <tt>true</tt> is returned.</p>
%
% @see is_dec/1
% @end
%
% %@equiv
%%
is_hex([]) ->
    true;

is_hex([Digit|Rest]) when (Digit >= 47) and (Digit =< 57);
                          (Digit >= 65) and (Digit =< 70);
                          (Digit >= 97) and (Digit =< 102) ->
    is_hex(Rest);

is_hex(_String) ->
    false.


%%%
% @spec is_atime(String) -> bool()
%    String = string()
%
% @doc Checks if <tt>String</tt> is a representacion of an absolute time 
% value, given in the format "YYMMDDhhmmsstnnp".  Where
%
% <dl>
%   <dt>YY</dt><dd>00 - 99</dd>
%   <dt>MM</dt><dd>01 - 12</dd>
%   <dt>DD</dt><dd>01 - 31</dd>
%   <dt>hh</dt><dd>00 - 23</dd>
%   <dt>mm</dt><dd>00 - 59</dd>
%   <dt>ss</dt><dd>00 - 59</dd>
%   <dt>t</dt><dd>0 - 9</dd>
%   <dt>nn</dt><dd>00 - 48</dd>
%   <dt>p</dt><dd>+ | -</dd>
% </dl>
%
% <p>If <tt>String</tt> is empty, <tt>true</tt> is returned.</p>
%
% @see is_rtime/1
% @end
%
% %@equiv
%%
is_atime([]) ->
    true;

is_atime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2,P]) when P == $+;
                                                                   P == $- ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2]) of
        true ->
            Date   = {list_to_integer([Y1,Y2]),
                      list_to_integer([M1,M2]),
                      list_to_integer([D1,D2])},
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            To_UTC = list_to_integer([N1,N2]),
            case calendar:valid_date(Date) of
                true when Hour < 24, Minute < 60, Second < 60, To_UTC < 49 ->
                    true;
                _Otherwise ->
                    false
            end;
        false ->
            false
    end;

is_atime(_String) ->
    false.


%%%
% @spec is_rtime(String) -> bool()
%    String = string()
%
% @doc Checks if <tt>String</tt> is a representacion of a relative time value,
% given in the format "YYMMDDhhmmsstnnp".  Where
%
% <dl>
%   <dt>YY</dt><dd>00 - 99</dd>
%   <dt>MM</dt><dd>01 - 12</dd>
%   <dt>DD</dt><dd>01 - 31</dd>
%   <dt>hh</dt><dd>00 - 23</dd>
%   <dt>mm</dt><dd>00 - 59</dd>
%   <dt>ss</dt><dd>00 - 59</dd>
%   <dt>t</dt><dd>0</dd>
%   <dt>nn</dt><dd>00</dd>
%   <dt>p</dt><dd>R</dd>
% </dl>
%
% <p>If <tt>String</tt> is empty, <tt>true</tt> is returned.</p>
%
% @see is_atime/1
% @end
%
% %@equiv
%%
is_rtime([]) ->
    true;

is_rtime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,$0,$0,$0,$R]) ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2]) of
        true ->
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            if
                (Hour < 24) and (Minute < 60) and (Second < 60) -> 
                    true;
                true ->
                    false
            end;
        false ->
            false
    end;

is_rtime(_String) ->
    false.


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
% @spec normalize(String) -> NString
%    String  = string()
%    NString = string()
%
% @doc Returns a new <tt>NString</tt> where spaces are normalized.
% @end
%
% %@see
% %@equiv
%%
normalize(String) ->
	normalize(strip(String, ?WHITESPACES), []).


%%%
% @doc Auxiliary function for normalize/1
%
% @see normalize/1
% @end 
%%
normalize([], Acc) ->
	lists:reverse(Acc);

normalize([H|T], Acc) when ?WHITESPACE(H) ->
	normalize(strip(T, left, ?WHITESPACES), [?SPACE|Acc]);

normalize([H|T], Acc) ->
	normalize(T, [H|Acc]).
	

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

