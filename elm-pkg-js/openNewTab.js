exports.init = async function init(app)
{
    app.ports.martinsstewart_elm_open_new_tab_to_js.subscribe(a => window.open(a, "_blank"));
}