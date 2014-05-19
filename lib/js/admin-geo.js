(function() {

    // disable the 'entered address' button when there is no address on the edit page 
    if ($('#address_country').length == 0) {
        $('#location_address').addClass('disabled');
    }

    // Initialize the map, with a timeout function so that the interface works before the map is loaded
    //OpenLayers.ImgPath = '/lib/images/';

    var map;
    var map_location;
    var marker;
    var currentPosition;
    
    function initializeAdminGeomap() {

        var rscLat = parseFloat($("#geomap-config").attr("data-latitude"));
        var rscLng = parseFloat($("#geomap-config").attr("data-longitude"));
        var elementId = $("#geomap-config").attr("data-element");

        if (rscLat || rscLng) {
            currentPosition = new google.maps.LatLng(rscLat, rscLng);
        }
        
        // Init map and center the map on the current position
        var mapOptions = {
            center: new google.maps.LatLng(rscLat, rscLng),
            zoom: 9
        };
        map = new google.maps.Map(document.getElementById(elementId), mapOptions);

        // The position marker
        marker = new google.maps.Marker({
            draggable: true,
            animation: google.maps.Animation.DROP
        });
        
        if (currentPosition) {
            marker.setPosition(currentPosition);
            marker.setMap(map);
        }

        google.maps.event.addListener(marker, "dragend", function(e) { 
            map_mark_location(e.latLng.lng(), e.latLng.lat(), true);
        }); 
        
        google.maps.event.addListener(map, 'click', function(e) {
            map_mark_location(e.latLng.lng(), e.latLng.lat(), true);
        });

    }

    google.maps.event.addDomListener(window, 'load', initializeAdminGeomap);

    $('#location_address').click(function(ev) {
        if ($('#address_country').length > 0) {
            var args = {
                street: $('#address_street_1').val(),
                city: $('#address_city').val(),
                postcode: $('#address_postcode').val(),
                state: $('#address_state').val(),
                country: $('#address_country').val(),
                z_delegate: 'mod_geomap'
            };
            z_notify("address_lookup", args);
            $(this).addClass('disabled');
        }
        ev.preventDefault(); 
    });
    
    $('#location_clear').click(function(ev) { 
        $('#location_lat').val('');
        $('#location_lng').val('');
        markers.clearMarkers();
        ev.preventDefault();
    });
    $('#location_reset').click(function(ev) { 
        $('#location_lat').val('{{ m.rsc[id].location_lat }}');
        $('#location_lng').val('{{ m.rsc[id].location_lng }}');
        var latitude = parseFloat('{{ m.rsc[id].computed_location_lat }}');
        var longitude = parseFloat('{{ m.rsc[id].computed_location_lng }}');
        if (latitude != NaN && longitude != NaN) {
            map_mark_location(longitude, latitude);
        }
        ev.preventDefault();
    });

    window.map_mark_location_error = function() {
        z_growl_add("{_ Could not find the location. _}");
        $('#location_address').removeClass('disabled');
    };

    window.map_mark_location = function(longitude, latitude, is_click) {

        marker.setMap(map);
        marker.setPosition(new google.maps.LatLng(latitude, longitude));
        $('#location_lat').val(latitude.toString());
        $('#location_lng').val(longitude.toString());
        if (!is_click) {
            //map.setCenter(map_location, 15);
            z_growl_add("{_ Location has been set. _}");
            $('#location_address').removeClass('disabled');
        }
    };
    
})();
