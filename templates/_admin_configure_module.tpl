<div class="modal-body">

    <div class="control-group">
        <label class="control-label" for="apikey">{_ Google maps API key _}</label>
        <div class="controls">
            <input type="text" id="apikey" name="api_key" value="{{ m.config.mod_geo.api_key.value|escape }}" class="span4 do_autofocus" />
            {% wire id="apikey" type="blur" action={config_toggle module="mod_geo" key="api_key" on="keyup"} %}
        </div>
    </div>

</div>

<div class="modal-footer">
    {% button class="btn" text=_"Close" action={dialog_close} tag="a" %}
</div>

