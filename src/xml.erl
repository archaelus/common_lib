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

%%% @doc XML Utility library.
%%%
%%% <p>Some utility functions for XML.  This module might not be 
%%% necessary if I ever get to understand how XPath and some other features
%%% of XMerL work.  Until that happens, I need this workaround functions
%%% to help writing my XMerL-related code.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {29 Jul 2004} {@time}.
%%% @end
-module(xml).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("xmerl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export(['@'/2, 
         '@'/3, 
         export_document/1, 
         export_document/2, 
         export_element/1,
         export_simple/1,
         file/1, 
         insert_content/3,
         insert_element/2,
         insert_element/3,
         select_child_@/3, 
         string/1, 
         value_of/1]).

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
%% @spec '@'(Element, AttrName) -> atom()
%%    Element = xmlElement()
%%    AttrName = atom()
%%
%% @doc Gets the value of the attribute named <tt>AttrName</tt>.  It the
%% attribute doesn't exist in <tt>Element</tt> the function fails.
%% @end 
'@'(Element, AttrName) ->
    {value, A} = lists:keysearch(AttrName, 2, Element#xmlElement.attributes),
    A#xmlAttribute.value.


%% @spec '@'(Element1, AttrName, AttrValue) -> Element2
%%    Element1 = xmlElement()
%%    AttrName = atom()
%%    AttrValue = IOlist() | atom() | integer()
%%    Element2 = xmlElement()
%%
%% @doc Sets the value of the attribute named <tt>AttrName</tt>.  It the
%% attribute doesn't exist in <tt>Element</tt> the function fails.
%% @end 
'@'(#xmlElement{attributes = L} = E, N, V) ->
    {value, A} = lists:keysearch(N, 2, L),
    E#xmlElement{attributes = lists:keyreplace(N,2,L,A#xmlAttribute{value=V})}.



%% @spec export_document(Term) -> string()
%%    Term = term()
%%
%% @doc Exports normal, well-formed XML content.
%%
%% @see export_document/2
%%
%% @equiv export_document(Term, [])
%% @end 
export_document(Term) ->
    export_document(Term, []).


%% @spec export_document(Term, AList) -> string()
%%    Term  = term()
%%    AList = [{Name, Value}]
%%    Name  = atom()
%%    Value = string()
%%
%% @doc Exports normal, well-formed XML content.
%%
%% <p><strong>Known issues:</strong> It doesn't insert attributes in the root 
%% element.  Use <a href="#export_document-1">export_document/1</a> instead.
%% </p>
%%
%% @see xmerl:export/3
%% @end 
export_document(Term, AList) ->
    F = fun({N, V}, P) -> {#xmlAttribute{name=N, pos=P, value=V}, P + 1} end,
    L = element(1, lists:mapfoldl(fun(X, A) -> F(X, A) end, 1, AList)),
    lists:flatten(xmerl:export_simple([Term], xmerl_xml, L)).


%% @spec export_element(Term) -> string()
%%    Term  = term()
%%
%% @doc Exports a normal XML element directly, without further context.
%%
%% @see xmerl:export_element/2
%%
%% @equiv lists:flatten(xmerl:export_element(Term, xmerl:callbacks(xmerl_xml)))
%% @end 
export_element(Term) ->
    lists:flatten(xmerl:export_element(Term, xmerl:callbacks(xmerl_xml))).


%% @spec export_simple(Term) -> term()
%%    Term  = term()
%%
%% @doc Translates a XMerL record into a simple erlang tuple.
%% @end 
export_simple(#xmlText{value = V}) -> 
    V;
export_simple(#xmlComment{value = V}) ->
    V;
export_simple(#xmlAttribute{name = N, value = V}) ->
    {N, V};
export_simple(#xmlElement{name = N, attributes = [], content = C}) ->
    {N, lists:map(fun export_simple/1, C)};
export_simple(#xmlElement{name = N, attributes = A, content = C}) ->
    {N, lists:map(fun export_simple/1, A), lists:map(fun export_simple/1, C)}.


%% @spec file(File) -> term()
%%    File = string()
%%
%% @doc Scans a XML file into a XML erlang term, removing spaces and
%% XML comments.
%%
%% @end 
file(File) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
                  {Acc, P, S};  % new return format
             (#xmlComment{pos = P}, Acc, S) ->
                  {Acc, P, S};  % new return format
             (X, Acc, S) ->
                  {[X|Acc], S}
          end,
    element(1, xmerl_scan:file(File, [{space,normalize}, {acc_fun, Acc}])).


%% @spec insert_content(Element1, Path, Value) -> Element2
%%    Element1 = xmlElement()
%%    Path     = string()
%%    Value    = string()
%%    Element2 = xmlElement()
%%
%% @doc Replaces the content of a XML with <tt>Value</tt>.  Target element the
%% child of <tt>Element1</tt> referenced by <tt>XPath</tt>.
%%
%% <p><tt>XPath</tt> must reference an existing node otherwise the function
%% will fail.  If more than one node with that path exists, only the content of
%% the last one will be changed.</p>
%% @end
insert_content(Element, [], Value) ->
    Element#xmlElement{content = [content(Element, Value)]};
insert_content(Element, Path, Value) ->
    {Token, Rest} = my_string:chop_token(Path, "/"),
    Content = insert_content(Element, list_to_atom(Token), Rest, Value),
    Element#xmlElement{content = Content}.

%% @doc Auxiliary function for insert_content/3
%%
%% @see insert_content/3
%% @end 
insert_content(Element, Name, Path, Value) ->
    Content = lists:reverse(Element#xmlElement.content),
    insert_content(Element, Content, Name, Path, Value, []).

%% @doc Auxiliary function for insert_content/4
%%
%% <p>Returns the new content for <tt>Element</tt> where the content of a
%% descendant will be inserted/changed.</p>
%%
%% @see insert_content/4
%% @end 
insert_content(Parent, [#xmlElement{name = N} = H|T], N, Path, Value, Acc) ->
    % The root of the path (given by N) matches the name of H
    Child = insert_content(H, Path, Value), % get into H and insert_content 
    lists:reverse(T) ++ [Child|Acc];
insert_content(Parent, [H|T], Name, Path, Value, Acc) ->
    insert_content(Parent, T, Name, Path, Value, [H|Acc]).


%% @spec insert_element(Element1, Path) -> Element2
%%    Element1 = xmlElement()
%%    Path     = string()
%%    Element2 = xmlElement()
%%
%% @doc Inserts an empty XML element in <tt>Element1</tt> given the 
%% <tt>Path</tt> to the new element.
%%
%% <p><tt>Path</tt> must be a valid relative path inside <tt>Element1</tt>.
%% </p>
%%
%% @see insert_element/3
%%
%% @equiv insert_element(Element1, Path, [])
%% @end 
insert_element(Element, Path) ->
    insert_element(Element, Path, []).


%% @spec insert_element(Element1, Path, Value) -> Element2
%%    Element1 = xmlElement()
%%    Path     = string()
%%    Value    = string()
%%    Element2 = xmlElement()
%%
%% @doc Inserts a XML element in <tt>Element1</tt> given the 
%% <tt>Path</tt> and <tt>Value</tt> of the new element.
%%
%% <p><tt>Path</tt> must be a valid relative path inside <tt>Element1</tt>.
%% </p>
%%
%% <p>New element is inserted at the very last permitted position.</p>
%% @end
insert_element(Element, Path, Value) ->
    case my_string:chop_token(Path, "/") of
        {Token, []} ->
            Child = child(Element, list_to_atom(Token), Value),
            Element#xmlElement{content=Element#xmlElement.content ++ [Child]};
        {Token, Rest} ->
            Content = insert_element(Element,list_to_atom(Token),Rest,Value),
            Element#xmlElement{content = Content}
    end.

%% @doc Auxiliary function for insert_element/3
%%
%% <p>Returns the new content for <tt>Element</tt> where a new child with
%% named <tt>Path/Name</tt> with value <tt>Value</tt> is inserted.</p>
%%
%% <p>New child is inserted at the end.</p>
%%
%% @see insert_element/3
%% @end 
insert_element(Element, Name, Path, Value) ->
    Content = lists:reverse(Element#xmlElement.content),
    insert_element(Element, Content, Name, Path, Value,[]).

%% @doc Auxiliary function for insert_element/4
%%
%% @see insert_element/4
%% @end 
insert_element(Parent, [], Name, Path, Value, Acc) ->
    % Need to create a new child named Name in the content of Parent.
    Child = insert_element(child(Parent, Name, []), Path, Value),
    Acc ++ [Child];
insert_element(Parent, [#xmlElement{name = N} = H|T], N, Path, Value, Acc) ->
    % The new child (given by Path and Value) goes inside H, the root of the
    % path (given by N) matches the name of H.
    Child = insert_element(H, Path, Value),
    lists:reverse(T) ++ [Child|Acc];
insert_element(Parent, [H|T], Name, Path, Value, Acc) ->
    insert_element(Parent, T, Name, Path, Value, [H|Acc]).


%% @spec select_child_@(Element, Id, Value) -> Children
%%    Element  = xmlElement()
%%    Id       = term()
%%    Value    = term()
%%    Children = [xmlElement()]
%%
%% @doc <tt>Children</tt> are those children of <tt>Element</tt> where the
%% attribute named <tt>Id</tt> has the value <tt>Value</tt>.
%%
%% <p>Is equivalent to <tt>child::node()[@Id='Value']</tt>.</p>
%% @end 
select_child_@(Element, Id, Value) ->
    select_child_@(xmerl_xs:select("child::node()", Element), Id, Value, []).

%% @doc Auxiliary function for select_child_@/3
%%
%% @see select_child_@/3
%% @end 
select_child_@([], _Id, _Value, Acc) ->
    lists:reverse(Acc);
select_child_@([E|T], Id, Value, Acc) ->
    case lists:keysearch(Id, 2, E#xmlElement.attributes) of
        {value, Attr} when Attr#xmlAttribute.value == Value ->
            select_child_@(T, Id, Value, [E|Acc]);
        _Otherwise ->
            select_child_@(T, Id, Value, Acc)
    end.


%% @spec string(Str) -> term()
%%    Str = string()
%%
%% @doc Scans a XML string into a XML erlang term, removing spaces and
%% XML comments.
%%
%% @end 
string(Str) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
                  {Acc, P, S};  % new return format
             (#xmlComment{pos = P}, Acc, S) ->
                  {Acc, P, S};  % new return format
             (X, Acc, S) ->
                  {[X|Acc], S}
          end,
    element(1, xmerl_scan:string(Str, [{space,normalize}, {acc_fun, Acc}])).


%% @spec value_of(XmlElement) -> string()
%%    XmlElement = xmlElement()
%%
%% @doc Gets the value of a <tt>XmlElement</tt>.
%%
%% @see xmerl_xs:value_of/1
%%
%% @equiv lists:flatten(xmerl_xs:value_of(XmlElement))
%% @end 
value_of(XmlElement) ->
    lists:flatten(xmerl_xs:value_of(XmlElement)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @spec child(Element1, Name, Value) -> Element2
%%    Element1 = xmlElement()
%%    Name     = atom()
%%    Value    = string()
%%    Element2 = xmlElement()
%%
%% @doc Creates a new child with name <tt>Name</tt> and value <tt>Value</tt>
%% for a given <tt>Element1</tt>.
%% @end 
child(#xmlElement{content = C} = E, Name, Value) ->
    Parents = [{E#xmlElement.name, E#xmlElement.pos}|E#xmlElement.parents],
    Pos = length(C) + 1,
    Content = if
                  Value == [] ->
                      [];
                  true ->
                      [#xmlText{parents = [{Name, Pos}|Parents], 
                                pos = 1,
                                language = E#xmlElement.language,
                                value = Value}]
              end,
    #xmlElement{name = Name,
                expanded_name = Name,
                nsinfo = E#xmlElement.nsinfo,
                namespace = E#xmlElement.namespace,
                parents = Parents,
                pos = Pos,
                content = Content,
                language = E#xmlElement.language,
                directory = E#xmlElement.directory}.


%% @spec content(Element1, Value) -> Element2
%%    Element1 = xmlElement()
%%    Value    = string()
%%    Element2 = xmlElement()
%%
%% @doc Creates the content of a XML element given a string <tt>Value</tt>.
%% @end 
content(#xmlElement{parents = P} = E, Value) ->
    #xmlText{parents = [{E#xmlElement.name, E#xmlElement.pos}|P], 
             pos = 1,
             language = E#xmlElement.language,
             value = Value}.
