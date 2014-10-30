{# This template is lazy loaded from the _geomap_admin_location.tpl #}

<div id="{{ #geomap }}" class="admin-geomap" style="height: 480px;"></div>
<style>label.inline { display: inline; padding: 0 10px; }</style>

<div style="margin-top: 10px">
    <div class="controls form-group row">

        <div class="col-md-5">
            <button class="btn btn-primary" id="location_address"><i class="glyphicon glyphicon-screenshot"></i> {_ Set to entered address _}</button>
        </div>

	    <label for="location_lat" class="col-md-1 control-label">{_ Latitude _}</label>
        <div class="col-md-2">
	        <input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="form-control" />
        </div>

	    <label for="location_lng" class="col-md-1 control-label">{_ Longitude _}</label>
        <div class="col-md-2">
	        <input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="form-control" />
        </div>

        <div class="col-md-1">
	        <button class="btn btn-default pull-right" id="location_clear"><i class="glyphicon glyphicon-remove"></i> {_ Clear _}</button>
        </div>
        
    </div>

</div>

<span id="geomap-config" data-latitude="{{ id.location_lat }}" data-longitude="{{ id.location_lng }}" data-element="{{ #geomap }}"></span>

<input id="location_zoom_level" type="hidden" name="location_zoom_level" value="{{ m.rsc[id].location_zoom_level }}" />


{% javascript %}
    setupAdminGeoMap('{{ id.location_lat }}', '{{ id.location_lng }}', '{{ #geomap }}', '{{ id.location_zoom_level }}');
{% endjavascript %}
