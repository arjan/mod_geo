<div class="col-md-12">
	<div class="checkbox">
        <label>
            <input value="1" type="checkbox"
               name="feature_show_geodata"
               {% if id.feature_show_geodata|if_undefined:`true` %}checked{% endif %}
               />
           {_ Show geo data on edit page _}
       </label>
    </div>
</div>
