%%% Copyright (C) 2005 Enrique Marcote Peña <mpquique@sourceforge.net>
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

%%% @doc Math functions.
%%%
%%% <p>Like the math module but using degrees instead of radians.</p>
%%%
%%% @copyright 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@sourceforge.net>
%%%         [http://oserl.sourceforge.net]
%%% @version 1.2, { 7 Nov 2005} {@time}.
%%% @end
-module(my_math).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([cos/1, sin/1, radians/1]).

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
%% @spec cos(Degrees) -> Value
%%     Degrees = int()
%%     Value = float()
%%
%% @doc Computes the cosine of an angle given in <tt>Degrees</tt>.
%%
%% @see math:cos/1
%% @end 
cos(0)   ->  1.0;
cos(90)  ->  0.0;
cos(180) -> -1.0;
cos(270) ->  0.0;
cos(D)   -> math:cos(radians(D)).

%% @spec sin(Degrees) -> Value
%%     Degrees = int()
%%     Value = float()
%%
%% @doc Computes the sine of an angle given in <tt>Degrees</tt>.
%%
%% @see math:sin/1
%% @end 
sin(0)   ->  0.0;
sin(90)  ->  1.0;
sin(180) ->  0.0;
sin(270) -> -1.0;
sin(D)   -> math:sin(radians(D)).

%% @spec radians(Degrees) -> Radians
%%     Degrees = int()
%%     Radians = float()
%%
%% @doc Gets the <tt>Radians</tt> given a value in <tt>Degrees</tt>.
%% @end 
radians(D) ->
    (D * math:pi()) / 180.

%%%===================================================================
%%% Internal functions
%%%===================================================================
