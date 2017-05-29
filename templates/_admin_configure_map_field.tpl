<div class="control-group">
    <label class="control-label" for="startingpoint">{_ Google maps starting point _}</label>
    <div class="controls">
        {% if m.config.mod_geo.api_key.value %}
        <div id="{{ #lazy }}">
            {% lazy action={update target=#lazy id=id template="_admin_configure_map.tpl"}%}
        </div>
        {% endif %}
    </div>
</div>
