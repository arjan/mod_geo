-module(z_geo_search).

-export([search_query/2]).

-include_lib("zotonic.hrl").

-define(PI, 3.141592653589793).

%% @doc Geo-related searches
search_query(#search_query{search={geo_nearby, Args}}, Context) ->
    Cats = case proplists:get_all_values(cat, Args) of
               [] -> [];
               Cs -> [{"r", lists:flatten(Cs)}]
           end,

    Distance = z_convert:to_float(proplists:get_value(distance, Args, 10)),
    {Lat, Lng} = get_query_center(Args, Context),
    {LatMin, LngMin, LatMax, LngMax} = get_lat_lng_bounds(Lat, Lng, Distance),
    
    #search_sql{
       select="r.id",
       from="rsc r",
       where="$1 < pivot_location_lat AND $2 < pivot_location_lng AND pivot_location_lat < $3 AND pivot_location_lng < $4",
       cats=Cats,
       order = "(pivot_location_lat-$5)*(pivot_location_lat-$5) + (pivot_location_lng-$6)*(pivot_location_lng-$6)",
       args=[LatMin, LngMin, LatMax, LngMax, Lat, Lng],
       tables=[{rsc,"r"}]
      };

search_query(#search_query{}, _Context) ->
    undefined. %% fall through


get_query_center(Args, Context) ->
    case {proplists:get_value(lat, Args), proplists:get_value(lng, Args)} of
        {undefined, undefined} ->
            case proplists:get_value(id, Args) of
                undefined ->
                    throw({error, missing_geo_search_parameters});
                Id0 ->
                    Id = m_rsc:name_to_id_check(Id0, Context),
                    case {m_rsc:p(Id, location_lat, Context), m_rsc:p(Id, location_lng, Context)} of
                        {Lat, Lng} when is_float(Lat), is_float(Lng) ->
                            {Lat, Lng};
                        _ ->
                            throw({error, {rsc_without_location, Id}})
                    end
            end;
        {Lat, Lng} ->
            {z_convert:to_float(Lat),
             z_convert:to_float(Lng)}
    end.



%% @doc see http://stackoverflow.com/questions/12424710/php-finding-latitude-and-longitude-boundaries-based-on-a-central-lat-lng-and-di
%% Radius is in kilometers.
get_lat_lng_bounds(Lat, Lng, Radius) ->
    EarthRadius = 6371.009,
    Fraq = Radius / EarthRadius,

    LatMax = Lat + rad2deg(Fraq),
    LatMin = Lat - rad2deg(Fraq),

    %% longitude boundaries (longitude gets smaller when latitude increases)
    LngMax = Lng + rad2deg(Fraq) / math:cos(deg2rad(Lat)),
    LngMin = Lng - rad2deg(Fraq) / math:cos(deg2rad(Lat)),

    {LatMin, LngMin, LatMax, LngMax}.


rad2deg(Rad) -> Rad * 57.29577951308232. %% angle / Math.PI * 180
deg2rad(Deg) -> Deg * 0.01745329251994329.
