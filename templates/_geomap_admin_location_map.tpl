{# This template is lazy loaded from the _geomap_admin_location.tpl #}

{% with m.rsc[id].computed_location_lat as latitude %}
{% with m.rsc[id].computed_location_lng as longitude %}
<div class="control-group">
	<label for="location_lat" class="control-label">{_ Latitude _}</label>
	<div class="controls">
		<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="input-medium" />
        <span class="help-inline">{_ indexed _}: {{ latitude }}</span>
	</div>
</div>

<div class="control-group">
	<label for="location_lng" class="control-label">{_ Longitude _}</label>
	<div class="controls">
		<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="input-medium" />
		<span class="help-inline">{_ indexed _}: {{ longitude }}</span>
	</div>
</div>

<div class="well">
	<button class="btn" id="location_me"><i class="icon-screenshot"></i> {_ Set to current location _}</button>
	<button class="btn" id="location_address"><i class="icon-screenshot"></i> {_ Set to entered address _}</button>
	<button class="btn" id="location_clear">{_ Clear _}</button>
	<button class="btn" id="location_reset">{_ Reset _}</button>
</div>

<div id="{{ #geomap }}" class="admin-geomap" style="height: 480px;"></div>

<p class="help-inline">{_ Please click on the map to select the location. _}</p>

<span id="geomap-config" data-latitude="{{ id.location_lat }}" data-longitude="{{ id.location_lng }}" data-element="{{ #geomap }}"></span>


{% endwith %}
{% endwith %}
