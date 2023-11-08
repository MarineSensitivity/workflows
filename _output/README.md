scripts for testing data analytics and visualization as well as production workflows 

## html

These web pages (\*.html) are typically rendered from Quarto markdown (\*.qmd):

<!-- Jekyll rendering -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}

## source

See [github.com/MarineSensitivity/workflows](https://github.com/MarineSensitivity/workflows)
