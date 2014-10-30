mod_geo
=======

Support for displaying maps and adding geographical locations to
:term:`resources <resource>` in the admin.

Uses Google Maps for display of maps in the admin.

This module is a fork of
(mod_geomap)[https://github.com/zotonic/mod_geomap], but has been
simplified: Google Maps is used in the admin interface, and the pivot
columns are no longer quadtile encoded but just 2 floating-point
columns in the rsc table, for portability and ease of querying.


Configuration
-------------

For Google Maps to work, add a config key called `mod_geo.api_key`
which must contain a valid Google Maps API key.


Search query: geo_nearby
------------------------

The module exposes a new search query type called `geo_nearby`, which is used like this:

    {% with m.search[{geo_nearby id=1306 distance=10}] as results %}

Required parameters are `id` or (`lat`+`lng`), and `distance` (which
specifies the search radius in kilometers).

The results are ordered, the nearest location is given first. (When
the `id` parameter is given, the first result is thus the id itself).

Optional parameters are `cat`, which can be a list of categories to
which to restrict the resulting resources to.


Service: /api/geo/nearby
-------------------------

Retrieve a list of resources with (basic) information about them, all
of which are in the vicinity of the given resource or lat/lng pair.

Internally uses the `geo_nearby` search mechanism, and has the same parameters.

It returns a list of JSON objects with for each resource the following
resource properties: id, title, summary, location_lat, location_lng,
location_zoom_level, created, modified, publication_start, image_url.
