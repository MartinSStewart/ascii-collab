exports.init = async function init(app)
{
    app.ports.martinsstewart_elm_device_pixel_ratio_to_js.subscribe(a => app.ports.martinsstewart_elm_device_pixel_ratio_from_js.send(window.devicePixelRatio));
}