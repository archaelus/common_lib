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
% @doc Calendar library.
%
% <p>My own calendar functions.</p>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1, {19 Feb 2003} {@time}.
% @end
%%
-module(my_calendar).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([day_of_current_week_to_date/1, 
         expired/1, 
         then/1, 
         time_since/1, 
         time_until/1]).

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
% @spec day_of_current_week_to_date(DayNumber) -> {Year, Month, Day}
%    DayNumber = int()
%    Year      = int()
%    Month     = int()
%    Day       = int()
%
% @doc Returns the date for a given day of current week.
% @end
%%
day_of_current_week_to_date(DayNumber) ->
    CurrentDayNumber = calendar:date_to_gregorian_days(date()),
    CurrentDayOfWeek = calendar:day_of_the_week(date()),
    calendar:gregorian_days_to_date(
      CurrentDayNumber + DayNumber - CurrentDayOfWeek).

%%%
% @spec expired(Time) -> true | false
%    Time = {MegaSecs, Secs, MicroSecs}
%    MegaSecs = int()
%    Secs = int()
%    MicroSecs = int()
%
% @doc Returs the atom <tt>true</tt> if the time returned by the BIF now/0
% is greater than <tt>Time</tt>, <tt>false</tt> otherwise.
% @end
%
% %@see
%
% %@equiv
%%
expired({MegaSecs, Secs, MicroSecs}) ->
    {MegaSecsNow, SecsNow, MicroSecsNow} = now(),
    if
        MegaSecsNow > MegaSecs ->
            true;
        (MegaSecsNow == MegaSecs), SecsNow > Secs ->
            true;
        (MegaSecsNow == MegaSecs),(SecsNow == Secs),MicroSecsNow > MicroSecs ->
            true;
        true ->
            false
    end.
    

%%%
% @spec then(TimeLapse) -> {MegaSecs, Secs, MicroSecs}
%    TimeLapse = int()
%    MegaSecs = int()
%    Secs = int()
%    MicroSecs = int()
%
% @doc This function adds <tt>TimeLapse</tt> microseconds to the result of 
% the BIF now/0.
% @end
%
% %@see
%
% %@equiv
%%
then(TimeLapse) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    TotalMicroSecs = MicroSecs + TimeLapse,
    SecsIncr       = trunc(TotalMicroSecs / 1000000),
    MicroSecsRest  = TotalMicroSecs - (SecsIncr * 1000000),
    TotalSecs      = Secs + SecsIncr,
    MegaSecsIncr   = trunc(TotalSecs / 1000000),
    {MegaSecs + MegaSecsIncr,TotalSecs - (MegaSecsIncr*1000000),MicroSecsRest}.


%%%
% @spec time_since(Time) -> TimeLapse
%    Time = {MegaSecs, Secs, MicroSecs}
%    TimeLapse = int()
%    MegaSecs = int()
%    Secs = int()
%    MicroSecs = int()
%
% @doc Returns the microseconds since <tt>Time</tt>.
% @end
%
% %@see
%
% %@equiv
%%
time_since({MegaSecs, Secs, MicroSecs}) ->
    {MegaSecsNow, SecsNow, MicroSecsNow} = now(),
    MegaSecsDiff  = MegaSecsNow - MegaSecs,
    SecsDiff      = SecsNow     - Secs,
    ((MegaSecsDiff * 1000000) + SecsDiff * 1000000) + MicroSecsNow - MicroSecs.


%%%
% @spec time_until(Time) -> TimeLapse
%    Time = {MegaSecs, Secs, MicroSecs}
%    TimeLapse = int()
%    MegaSecs = int()
%    Secs = int()
%    MicroSecs = int()
%
% @doc Returns the microseconds until <tt>Time</tt>.
% @end
%
% %@see
%
% %@equiv
%%
time_until({MegaSecs, Secs, MicroSecs}) ->
    {MegaSecsNow, SecsNow, MicroSecsNow} = now(),
    MegaSecsDiff  = MegaSecs  - MegaSecsNow,
    SecsDiff      = Secs      - SecsNow,
    ((MegaSecsDiff * 1000000) + SecsDiff * 1000000) + MicroSecs - MicroSecsNow.

%%%===================================================================
% Internal functions
%%====================================================================


