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

%%% @doc Statistics library.
%%%
%%% <p>Functions for statistics.</p>
%%%
%%%
%%% @copyright 2003 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0, {19 Feb 2003} {@time}.
%%% @end
-module(stats).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([combinations/2]).

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
%% @spec combinations(M, N) -> R
%%    M = int()
%%    N = int()
%%    R = error | int()
%%
%% @doc Computes the combinatios of <tt>M</tt> elements taken by
%% <tt>N</tt>.
%% 
%% @see div_factorial/2
%% @see factorial/1
%% @end
combinations(M, N) when (M < 0) or (N < 0) or (M < N) ->
	 error;					
combinations(M, N) ->
	 trunc(div_factorial(M, N) / factorial(M - N)).

%%%===================================================================
% Internal functions
%%%===================================================================
%% @spec factorial(N) -> M
%%    N = int()
%%    M = int()
%%
%% @doc Computes the factorial of an integer.  The result is
%% undetermined if a negative number is given to the function.
%% @end
factorial(0)            -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% @spec div_factorial(M, N) -> R
%%    M = int()
%%    N = int()
%%    R = int()
%%
%% @doc  Given two integers, this function computes the integer division of 
%% their factorials.  The result will be undetermined if <tt>N</tt> is
%% smaller than 0 or any of the arguments is a negative number.
%% @end
div_factorial(M, M)            -> 1;
div_factorial(M, N) when N > M -> 0;
div_factorial(M, N)            -> M * div_factorial(M - 1, N).
