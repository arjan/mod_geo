
-module(service_geo_nearby).

-export([process_get/2]).
-include_lib("zotonic.hrl").

process_get(_, Context) ->
    Params = lists:foldl(fun(P, Acc) ->
                                 case z_context:get_q(atom_to_list(P), Context) of
                                     undefined -> Acc;
                                     V -> [{P, V}|Acc] end
                         end,
                         [],
                         [id, cat, distance, lat, lng]),
    R=z_search:search({geo_nearby, Params}, Context),
    {array, map_results(R, Context)}.


map_results(#search_result{result=Ids}, Context) ->
    [z_convert:to_json(export(Id, Context)) || Id <- Ids].

export(Id, Context) ->
    ImageUrl = case z_media_tag:url(m_rsc:p(Id, depiction, Context), [{width, 400}], Context) of
                   {ok, U} -> [{image_url, U}];
                   X
                   -> lager:warning("X: ~p", [X]),[]
               end,
    [{K, m_rsc:p(Id, K, Context)}
     || K <- [id, title, summary, location_lat, location_lng, location_zoom_level, created, modified, publication_start]]
        ++ ImageUrl.

