<div class="modal-body">

    <div class="control-group">
        <label class="control-label" for="apikey">{_ Google maps API key _}</label>
        <div class="controls">
            <input type="text" id="apikey" name="api_key" value="{{ m.config.mod_geo.api_key.value|escape }}" class="form-control do_autofocus" />
            {% wire id="apikey" type="blur" action={config_toggle module="mod_geo" key="api_key" on="keyup"} %}
        </div>
    </div>
    <div class="control-group">
        <label class="control-label" for="startingpoint">{_ Google maps starting point _}</label>
        <div class="controls">
            {% if m.config.mod_geo.api_key.value %}
                <div id="{{ #lazy }}">
                    {% lazy action={update target=#lazy id=id template="_admin_configure_map.tpl"}%}
                </div>
            {% else %}
                {_ First add the Google maps API key _}
            {% endif %}
        </div>
    </div>

</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>

