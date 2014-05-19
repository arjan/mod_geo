{# Experimental, needs better parametrization (like resource id and div size) #}
<div id="{{ element_id|default:#map }}" style="width: 700px; height: 480px;"></div>

<script type="text/javascript" src="https://maps.googleapis.com/maps/api/js?key=API_KEY&sensor=false"></script>

{% javascript %}
    function initializeAdminGeomap() {

        var mapOptions = {
          center: new google.maps.LatLng(-34.397, 150.644),
          zoom: 8
        };
        var map = new google.maps.Map(document.getElementById('{{ element_id|default:#map }}'), mapOptions);

    

    {% if longitude|is_defined and latitude|is_defined %}
        var map_location = new OpenLayers
                            .LonLat({{longitude}}, {{latitude}})
                            .transform(
                                new OpenLayers.Projection("EPSG:4326"),
                                map.getProjectionObject());
        map.setCenter(map_location, 15);
        marker_icon.setOpacity(0.8);
        markers.addMarker(new OpenLayers.Marker(map_location, marker_icon));
    {% else %}
        var map_location = new OpenLayers.LonLat(0, 0);
        map.setCenter(map_location, 2);
    {% endif %}
    }
    google.maps.event.addDomListener(window, 'load', initializeAdminGeomap);
{% endjavascript %}
