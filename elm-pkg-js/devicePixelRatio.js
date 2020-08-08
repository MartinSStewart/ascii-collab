exports.init = async function init(app)
{
    app.ports.devicePixelRatioPortToJS.subscribe(a => app.ports.devicePixelRatioPortFromJS.send(window.devicePixelRatio));
}