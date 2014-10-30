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
    
    window.setupAdminGeoMap = function(rscLat, rscLng, elementId, rscZoomLevel) {

        rscLat = parseFloat(rscLat);
        rscLng = parseFloat(rscLng);
        rscZoomLevel = parseInt(rscZoomLevel, 10);
        
        if (rscLat || rscLng) {
            currentPosition = new google.maps.LatLng(rscLat, rscLng);
        }

        var center = currentPosition ? currentPosition : new google.maps.LatLng(52.3730438, 4.837568000);
        
        // Init map and center the map on the current position
        var mapOptions = {
            center: center,
            zoom: rscZoomLevel > 0 ? rscZoomLevel : 12
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

        google.maps.event.addListener(map, 'zoom_changed', function(e) {
            $('#location_zoom_level').val(map.getZoom().toString());
        });

        $('#location_address').click(function(ev) {
            if ($('#address_country').length > 0) {
                var args = {
                    street: $('#address_street_1').val(),
                    city: $('#address_city').val(),
                    postcode: $('#address_postcode').val(),
                    state: $('#address_state').val(),
                    country: $('#address_country').val(),
                    z_delegate: 'mod_geo'
                };
                z_notify("address_lookup", args);
                $(this).addClass('disabled');
            }
            ev.preventDefault(); 
        });
        
        $('#location_clear').click(function(ev) { 
            $('#location_lat').val('');
            $('#location_lng').val('');
            marker.setMap(null);
            ev.preventDefault();
        });
                
    }

    window.map_mark_location_error = function() {
        z_growl_add("{_ Could not find the location. _}");
        $('#location_address').removeClass('disabled');
    };

    window.map_mark_location = function(longitude, latitude, is_click) {
        var loc = new google.maps.LatLng(latitude, longitude);
        marker.setMap(map);
        marker.setPosition(loc);
        $('#location_lat').val(latitude.toString());
        $('#location_lng').val(longitude.toString());
        if (!is_click) {
            map.setCenter(loc);
            z_growl_add("Location has been set.");
            $('#location_address').removeClass('disabled');
        }
    };
    
})();
