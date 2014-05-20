{# This template is lazy loaded from the _geomap_admin_location.tpl #}

<div id="{{ #geomap }}" class="admin-geomap" style="height: 480px;"></div>
<style>label.inline { display: inline; padding: 0 10px; }</style>

<div style="margin-top: 10px">
    <div class="pull-right">
	    <label for="location_lat" class="inline">{_ Latitude _}</label>
	    <input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="input-medium" />

	    <label for="location_lng" class="inline">{_ Longitude _}</label>
	    <input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="input-medium" />
    </div>

	<button class="btn" id="location_address"><i class="icon-screenshot"></i> {_ Set to entered address _}</button>
	<button class="btn" id="location_clear">{_ Clear _}</button>
</div>

<span id="geomap-config" data-latitude="{{ id.location_lat }}" data-longitude="{{ id.location_lng }}" data-element="{{ #geomap }}"></span>

<input id="location_zoom_level" type="hidden" name="location_zoom_level" value="{{ m.rsc[id].location_zoom_level }}" />


{% javascript %}
    setupAdminGeoMap('{{ id.location_lat }}', '{{ id.location_lng }}', '{{ #geomap }}', '{{ id.location_zoom_level }}');
{% endjavascript %}
