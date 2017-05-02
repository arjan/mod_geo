%% @author Marc Worrell <marc@worrell.nl>
%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2012 Marc Worrell
%% @copyright 2014 Arjan Scherpenisse
%% @doc Storing lat/lng in resource pivot columns and providing search routines.

%% Copyright 2012 Marc Worrell, 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_geo).

-mod_title("Geo support").
-mod_description("Geocoding and geo calculations, supported by Google Maps").
-mod_prio(400).
-mod_depends([mod_l10n]).

-export(
   [
    event/2,
    observe_search_query/2,
    observe_pivot_fields/3,
    find_geocode_api/4,
    find_geocode_api/3
   ]).

-include_lib("zotonic.hrl").


%% @doc Handle an address lookup from the admin.
%% @todo Maybe add check if the user is allowed to use the admin.
event(#postback_notify{message="address_lookup"}, Context) ->
    Fields = [
        {address_street_1, z_context:get_q("street", Context)},
        {address_city, z_context:get_q("city", Context)},
        {address_state, z_context:get_q("state", Context)},
        {address_postcode, z_context:get_q("postcode", Context)},
        {address_country, z_context:get_q("country", Context)},
        {address_country_name, z_context:get_q("country_name", Context)}
    ],

    {ok, Type, Qmaps} = q(Fields, address_country, Context),
    {ok, _Type2, Qosm} = q(Fields, address_country_name, Context),

    case find_geocode_api(Qmaps, Qosm, Type, Context) of
        {error, _} ->
            z_script:add_script("map_mark_location_error();", Context);
        {ok, {Lat, Long}} ->
            z_script:add_script(io_lib:format("map_mark_location(~p,~p);", [Long, Lat]), Context)
    end.


observe_search_query(#search_query{}=Q, Context) ->
    z_geo_search:search_query(Q, Context).


%% @doc Check if the latitude/longitude are set, if so the pivot the pivot_geocode.
%%      If not then try to derive the lat/long from the rsc's address data.
observe_pivot_fields(#pivot_fields{id=Id, rsc=R}, KVs, Context) ->
    case {catch z_convert:to_float(proplists:get_value(location_lat, R)),
          catch z_convert:to_float(proplists:get_value(location_lng, R))} of
        {Lat, Long} when is_float(Lat), is_float(Long) ->
            KVs; %% OK, do not change
        _ ->
                                                % Optionally geocode the address in the resource.
                                                % When successful this will spawn a new geocode pivot.
            case optional_geocode(R, Context) of
                reset -> 
                    KVs;
                {ok, Lat, Long, QHash} ->
                    m_rsc:update(Id, [{location_lat, Lat}, {location_lng, Long}, {geocode_qhash, QHash}], z_acl:sudo(Context)),
                    KVs;
                ok -> 
                    KVs
            end
    end.



%% @doc Check if we should lookup the location belonging to the resource.
%%      If so we store the quadtile code into the resource without a re-pivot.
optional_geocode(R, Context) ->
    Lat = proplists:get_value(location_lat, R),
    Long = proplists:get_value(location_long, R),
    case z_utils:is_empty(Lat) andalso z_utils:is_empty(Long) of
        false ->
            reset;
        true ->
            case q(R, Context) of
                {ok, _, <<>>} ->
                    reset;
                {ok, country, _} ->
                    %% dont do automatic geocoding when only country is known
                    reset;
                {ok, Type, Q} ->
                    LocHash = lists:flatten(z_utils:checksum(Q, Context)),
                    case proplists:get_value(pivot_geocode_qhash, R) of
                        LocHash ->
                                                % Not changed since last lookup 
                            ok;
                        _ ->
                                                % Changed, and we are doing automatic lookups
                            Qosm = q(R, address_country_name, Context),
                            case find_geocode_api(Q, Qosm, Type, Context) of
                                {error, _} ->
                                    reset;
                                {ok, {NewLat,NewLong}} ->
                                    {ok, NewLat, NewLong, LocHash}
                            end
                    end
            end
    end.

find_geocode_api(Q, Type, Context) ->
    find_geocode_api(Q, Q, Type, Context).

%% @doc Check with Google and OpenStreetMap if they know the address
find_geocode_api(_Qmaps, Qosm, country, _Context) ->
    Qq = mochiweb_util:quote_plus(Qosm),
    openstreetmap(Qq);
find_geocode_api(Qmaps, Qosm,  _Type, Context) ->
    Qq = mochiweb_util:quote_plus(Qmaps),
    case googlemaps_check(Qq, Context) of
        {error, _} ->
            Qqo = mochiweb_util:quote_plus(Qosm),
            openstreetmap(Qqo);
        {ok, {_Lat, _Long}} = Ok->
            Ok
    end.


openstreetmap(Q) ->
    Url = "http://nominatim.openstreetmap.org/search?format=json&limit=1&addressdetails=0&q="++Q,
    case get_json(Url) of
        {ok, [{struct, Props}|_]} ->
            case {z_convert:to_float(proplists:get_value(<<"lat">>, Props)),
                  z_convert:to_float(proplists:get_value(<<"lon">>, Props))}
            of
                {Lat, Long} when is_float(Lat), is_float(Long) ->
                    {ok, {Lat, Long}};
                _ ->
                    {error, not_found}
            end;
        {ok, []} ->
            lager:debug("OpenStreetMap empty return for ~p", [Q]),
            {error, not_found};
        {ok, JSON} ->
            lager:error("OpenStreetMap unknown JSON ~p on ~p", [JSON, Q]),
            {error, unexpected_result};
        {error, Reason} = Error ->
            lager:warning("OpenStreetMap returns ~p for ~p", [Reason, Q]),
            Error
    end.

googlemaps_check(Q, Context) ->
    case z_depcache:get(googlemaps_error, Context) of
        undefined ->
            case googlemaps(Q) of
                {error, query_limit} = Error ->
                    lager:warning("Geomap: Google reached query limit, disabling for 1800 sec"),
                    z_depcache:set(googlemaps_error, Error, 1800, Context),
                    Error;
                Result ->
                    Result
            end;
        {ok, Error} -> 
            lager:debug("Geomap: skipping Google lookup due to ~p", [Error]),
            Error
    end.

googlemaps(Q) ->
    Url = "http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address="++Q,
    case get_json(Url) of
        {ok, []} ->
            lager:debug("Google maps empty return for ~p", [Q]),
            {error, not_found};
        {ok, {struct, Props}} ->
            case proplists:get_value(<<"status">>, Props) of
                <<"OK">> ->
                    [{struct, Result}|_] = proplists:get_value(<<"results">>, Props),
                    case proplists:get_value(<<"geometry">>, Result) of
                        undefined ->
                            lager:info("Google maps result without geometry: ~p", [Props]),
                            {error, no_result};
                        {struct, GPs} ->
                            case proplists:get_value(<<"location">>, GPs) of
                                {struct, Ls} ->
                                    case {z_convert:to_float(proplists:get_value(<<"lat">>, Ls)),
                                          z_convert:to_float(proplists:get_value(<<"lng">>, Ls))}
                                    of
                                        {Lat, Long} when is_float(Lat), is_float(Long) ->
                                            {ok, {Lat, Long}};
                                        _ ->
                                            {error, not_found}
                                    end;
                                undefined ->
                                    lager:info("Google maps geometry without location: ~p", [Props]),
                                    {error, no_result}
                            end
                    end;
                <<"ZERO_RESULTS">> ->
                    {error, not_found};
                <<"OVER_QUERY_LIMIT">> ->
                    {error, query_limit};
                Status ->
                    lager:warning("Google maps status ~p on ~p", [Status, Q]),
                    {error, unexpected_result}
            end;
        {ok, JSON} ->
            lager:error("Google maps unknown JSON ~p on ~p", [JSON, Q]),
            {error, unexpected_result};
        {error, Reason} = Error ->
            lager:warning("Google maps returns ~p on ~p", [Reason, Q]),
            Error
    end.



get_json(Url) ->
    lager:debug("Geo lookup: ~p", [Url]),
    case httpc:request(get, {Url, []}, [{autoredirect, true}, {relaxed, true}, {timeout, 10000}], []) of
        {ok, {
           {_HTTP, 200, _OK},
           Headers,
           Body
          }} ->
            case proplists:get_value("content-type", Headers) of
                "application/json" ++ _ ->
                    {ok, mochijson2:decode(Body)};
                CT ->
                    {error, {unexpected_content_type, CT}}
            end;
        {error, _Reason} = Err ->
            Err;
        {ok, {{_, 503, _}, _, _}} ->
            {error, no_service};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, _Other} ->
            {error, unexpected_result}
    end.


q(R, Context) ->
    q(R, address_country, Context).

q(R, CountryField, Context) ->
    Fs = iolist_to_binary([
        p(address_street_1, $,, R),
        p(address_city, $,, R),
        p(address_state, $,, R),
        p(address_postcode, $,, R)
    ]),
    case Fs of
        <<>> ->
            {ok, country, iolist_to_binary(p(CountryField, <<>>, R))};
        _ ->
            Country = iolist_to_binary(country_name(p(CountryField, <<>>, R), Context)),
            {ok, full, <<Fs/binary, Country/binary>>}
    end.

p(F, Sep, R) ->
    case proplists:get_value(F, R) of
        <<>> -> <<>>;
        [] -> <<>>;
        undefined -> <<>>;
        V -> [V, Sep]
    end.


country_name([], _Context) -> <<>>;
country_name(<<>>, _Context) -> <<>>;
country_name(undefined, _Context) -> <<>>;
country_name(<<"gb-nir">>, _Context) -> <<"Northern Ireland">>;
country_name(Iso, Context) ->
    m_l10n:country_name(Iso, en, Context).

