{# This template is lazy loaded from the _geomap_admin_location.tpl #}

<div id="{{ #geomap }}" class="admin-geomap" style="height: 300px;"></div>
<style>label.inline { display: inline; padding: 0 10px; }</style>

<div style="margin-top: 10px">
    <div class="controls form-group row">
        <input id="location_lat" type="hidden" name="location_lat" value="{{ m.config.mod_geo.location_lat.value|escape }}" class="form-control" />
        {% wire id="location_lat" type="blur" action={config_toggle module="mod_geo" key="location_lat" on="change"} %}
        <input id="location_lng" type="hidden" name="location_lng" value="{{ m.config.mod_geo.location_lng.value|escape }}" class="form-control" />
        {% wire id="location_lng" type="blur" action={config_toggle module="mod_geo" key="location_lng" on="change"} %}
    </div>
</div>

<span id="geomap-config" data-latitude="{{ m.config.mod_geo.location_lat.value|escape }}" data-longitude="{{ m.config.mod_geo.location_lng.value|escape }}" data-element="{{ #geomap }}"></span>

<input id="location_zoom_level" type="hidden" name="location_zoom_level" value="{{ m.config.mod_geo.location_zoom_level.value|escape }}" />
{% wire id="location_zoom_level" type="blur" action={config_toggle module="mod_geo" key="location_zoom_level" on="change"} %}

{% javascript %}
    setupAdminGeoMap('', '', '{{ #geomap }}', '', '{{ m.config.mod_geo.location_lat.value|escape }}', '{{ m.config.mod_geo.location_lng.value|escape }}', '{{ m.config.mod_geo.location_zoom_level.value|escape }}');
{% endjavascript %}
