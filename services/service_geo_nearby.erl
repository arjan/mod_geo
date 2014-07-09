
-module(service_geo_nearby).

-export([process_get/2]).
-include_lib("zotonic.hrl").

process_get(_, Context) ->
    Args = lists:foldl(fun(P, Acc) ->
                                 case z_context:get_q(atom_to_list(P), Context) of
                                     undefined -> Acc;
                                     V -> [{P, V}|Acc] end
                         end,
                         [],
                         [id, cat, distance, lat, lng]),
    R=z_search:search({geo_nearby, Args}, Context),
    
    {Lat, Lng} = z_geo_search:get_query_center(Args, Context),
    {array, lists:sort(map_results(R, Lat, Lng, Context))}.


map_results(#search_result{result=Ids}, Lat, Lng, Context) ->
    [begin
         Props = export(Id, Context),
         D = z_geo_support:distance(Lat, Lng, proplists:get_value(location_lat, Props), proplists:get_value(location_lng, Props)),
         z_convert:to_json([{distance, D} | Props])
     end|| Id <- Ids].

export(Id, Context) ->
    ImageUrl = case z_media_tag:url(m_rsc:p(Id, depiction, Context), [{width, 400}], Context) of
                   {ok, U} -> [{image_url, U}];
                   _ -> []
               end,
    [{K, m_rsc:p(Id, K, Context)}
     || K <- [id, title, summary, location_lat, location_lng, location_zoom_level, created, modified, publication_start]]
        ++ ImageUrl.

