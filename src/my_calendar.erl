%%% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc Calendar library.
%%%
%%% <p>My own calendar functions.</p>
%%%
%%% <h2>Changes 0.2 -&gt; 1.0</h2>
%%%
%%% [12 May 2004]
%%%
%%% <ul>
%%%   <li>New functions: <a href="#day-0">day/0</a>, 
%%%     <a href="#day-1">day/1</a>, <a href="#day-3">day/3</a>, 
%%%     <a href="#week-0">week/0</a>, <a href="#week-1">week/1</a> and 
%%%     <a href="#week-3">week/3</a>.
%%%   </li>
%%% </ul>
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0, {19 Feb 2003} {@time}.
%%% @end
-module(my_calendar).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([day/0,
         day/1,
         day/3,
         day_of_current_week_to_date/1, 
         expired/1, 
         then/1, 
         time_since/1, 
         time_until/1,
         week/0,
         week/1,
         week/3]).

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
%% @spec day() -> int()
%%
%% @doc Returns the day number on current year for today.
%% @end 
day() ->
    {Date, Time} = calendar:local_time(),
    day(Date).


%% @spec day(Date) -> int()
%%    Date  = {Year, Month, Day}
%%    Year  = int()
%%    Month = int()
%%    Day   = int()
%%
%% @doc Returns the day number on that <tt>Year</tt> for <tt>Date</tt>.
%% @end 
day({Year, Month, Day}) ->
    day(Year, Month, Day).


%% @spec day(Year, Month, Day) -> int()
%%    Year  = int()
%%    Month = int()
%%    Day   = int()
%%
%% @doc Returns the day number on that <tt>Year</tt> for the <tt>Day</tt> of
%% <tt>Month</tt>.
%% @end 
day(Year, Month, Day) ->
    January1st = calendar:date_to_gregorian_days(Year, 1, 1),
    calendar:date_to_gregorian_days(Year, Month, Day) - January1st + 1.


%% @spec day_of_current_week_to_date(DayNumber) -> {Year, Month, Day}
%%    DayNumber = int()
%%    Year      = int()
%%    Month     = int()
%%    Day       = int()
%%
%% @doc Returns the date for a given day of current week.
%% @end
day_of_current_week_to_date(DayNumber) ->
    CurrentDayNumber = calendar:date_to_gregorian_days(date()),
    CurrentDayOfWeek = calendar:day_of_the_week(date()),
    calendar:gregorian_days_to_date(
      CurrentDayNumber + DayNumber - CurrentDayOfWeek).

%% @spec expired(Time) -> true | false
%%    Time = {MegaSecs, Secs, MicroSecs}
%%    MegaSecs = int()
%%    Secs = int()
%%    MicroSecs = int()
%%
%% @doc Returs the atom <tt>true</tt> if the time returned by the BIF now/0
%% is greater than <tt>Time</tt>, <tt>false</tt> otherwise.
%% @end
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
    

%% @spec then(TimeLapse) -> {MegaSecs, Secs, MicroSecs}
%%    TimeLapse = int()
%%    MegaSecs = int()
%%    Secs = int()
%%    MicroSecs = int()
%%
%% @doc This function adds <tt>TimeLapse</tt> microseconds to the result of 
%% the BIF now/0.
%% @end
then(TimeLapse) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    TotalMicroSecs = MicroSecs + TimeLapse,
    SecsIncr       = trunc(TotalMicroSecs / 1000000),
    MicroSecsRest  = TotalMicroSecs - (SecsIncr * 1000000),
    TotalSecs      = Secs + SecsIncr,
    MegaSecsIncr   = trunc(TotalSecs / 1000000),
    {MegaSecs + MegaSecsIncr,TotalSecs - (MegaSecsIncr*1000000),MicroSecsRest}.


%% @spec time_since(Time) -> TimeLapse
%%    Time = {MegaSecs, Secs, MicroSecs}
%%    TimeLapse = int()
%%    MegaSecs = int()
%%    Secs = int()
%%    MicroSecs = int()
%%
%% @doc Returns the microseconds since <tt>Time</tt>.
%% @end
time_since({MegaSecs, Secs, MicroSecs}) ->
    {MegaSecsNow, SecsNow, MicroSecsNow} = now(),
    MegaSecsDiff  = MegaSecsNow - MegaSecs,
    SecsDiff      = SecsNow     - Secs,
    ((MegaSecsDiff * 1000000) + SecsDiff * 1000000) + MicroSecsNow - MicroSecs.


%% @spec time_until(Time) -> TimeLapse
%%    Time = {MegaSecs, Secs, MicroSecs}
%%    TimeLapse = int()
%%    MegaSecs = int()
%%    Secs = int()
%%    MicroSecs = int()
%%
%% @doc Returns the microseconds until <tt>Time</tt>.
%% @end
time_until({MegaSecs, Secs, MicroSecs}) ->
    {MegaSecsNow, SecsNow, MicroSecsNow} = now(),
    MegaSecsDiff  = MegaSecs  - MegaSecsNow,
    SecsDiff      = Secs      - SecsNow,
    ((MegaSecsDiff * 1000000) + SecsDiff * 1000000) + MicroSecs - MicroSecsNow.


%% @spec week() -> int()
%%
%% @doc Returns the week number on current year for today.
%% @end 
week() ->
    {Date, Time} = calendar:local_time(),
    week(Date).


%% @spec week(Date) -> int()
%%    Date  = {Year, Month, Day}
%%    Year  = int()
%%    Month = int()
%%    Day   = int()
%%
%% @doc Returns the week number on that <tt>Year</tt> for <tt>Date</tt>.
%% @end 
week({Year, Month, Day}) ->
    week(Year, Month, Day).


%% @spec week(Year, Month, Day) -> int()
%%    Year  = int()
%%    Month = int()
%%    Day   = int()
%%
%% @doc Returns the week number on that <tt>Year</tt> for the <tt>Day</tt> of
%% <tt>Month</tt>.
%% @end 
week(Year, Month, Day) ->
    DaysUntil1stSunday = 7 - calendar:day_of_the_week(Year, 1, 1) + 1,
    DaysSince1stSunday = day(Year, Month, Day) - DaysUntil1stSunday,
    if
        (DaysSince1stSunday rem 7) > 0 -> (DaysSince1stSunday div 7) + 2;
        true                           -> (DaysSince1stSunday div 7) + 1
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


