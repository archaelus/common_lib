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

%%% @doc Report
%%%
%%% <p>Reports error, status and messages to the error_logger in a common
%%% format from every amuse module.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <quique@bali.udc.es>
%%%         [http://sourceforge.net/users/mpquique/]
%%% @version 1.0, { 9 Dec 2004} {@time}.
%%% @end
-module(report).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([error/4, info/3, msg/1, msg/2]).

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
%% @spec error(Application, ErrorContext, Reason, Offender) -> ok
%%    Application = term()
%%    ErrorContext = term()
%%    Reason = term()
%%    Offender = term()
%%
%% @doc Reports an error to the error_logger.
%% @end 
error(Application, Error, Reason, Offender) ->
    Msg = [{application, Application}, 
           {error, Error}, 
           {reason, Reason},                
           {offender, Offender}],
    error_logger:error_report(Msg).


%% @spec info(Application, Info, Details) -> ok
%%    Application = term()
%%    Info = term()
%%    Details = term()
%%
%% @doc Reports info to the error_logger.
%% @end 
info(Application, Info, Details) ->
    Msg = [{application, Application}, {info, Info}, {details, Details}],
    error_logger:info_report(Msg).


%% @spec msg(Message) -> ok
%%    Message = string()
%%
%% @doc Reports an error_msg to the error_logger.
%% @end 
msg(Message) ->
    msg(Message, []).


%% @spec msg(Message, Args) -> ok
%%    Message = string()
%%    Args = [term()]
%%
%% @doc Reports an error_msg to the error_logger.
%% @end 
msg(Message, Args) ->
    error_logger:error_msg(Message, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================



