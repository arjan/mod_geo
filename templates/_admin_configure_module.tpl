<div class="modal-body">

    <div class="control-group">
        <label class="control-label" for="apikey">{_ Google maps API key _}</label>
        <div class="controls">
            <input type="text" id="apikey" name="api_key" value="{{ m.config.mod_geo.api_key.value|escape }}" class="form-control do_autofocus" />
            {% wire id="apikey" type="blur" action={config_toggle module="mod_geo" key="api_key" on="keyup"} %}
        </div>
    </div>
    {% if m.config.mod_geo.api_key.value %}
        {% include "_admin_configure_map_field.tpl" %}
    {% else %}
        {% wire id="apikey" type="blur" action={update target="showmap" template="_admin_configure_map_field.tpl" } %}
        <div id="showmap"></div>
    {% endif %}
</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

