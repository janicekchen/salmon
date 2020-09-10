// https://observablehq.com/@janicekchen/versor-zooming@335
export default function define(runtime, observer) {
  const main = runtime.module();
  const fileAttachments = new Map([["land-110m.json",new URL("./files/eec657afeffb70691657f56f78ce546cc20861c628c4272d902fb7ff94d07a73737fd5356d255cef2a092de8322c56bbbc4f0f6a3c0c12864101f37ec6da9321",import.meta.url)],["land-50m.json",new URL("./files/efcaaf9f0b260e09b6afeaee6dbc1b91ad45f3328561cd67eb16a1754096c1095f70d284acdc4b004910e89265b60eba2706334e0dc84ded38fd9209083d4cef",import.meta.url)]]);
  main.builtin("FileAttachment", runtime.fileAttachments(name => fileAttachments.get(name)));
  main.variable(observer()).define(["md"], function(md){return(
md`# Versor Zooming

A zoom behavior that makes a projection follow zoom, drag and rotate gestures on multitouch devices. See also [Versor Dragging](https://observablehq.com/@d3/versor-dragging).`
)});
  main.variable(observer("chart")).define("chart", ["DOM","width","height","d3","projection","sphere","zoom","land110","land50"], function(DOM,width,height,d3,projection,sphere,zoom,land110,land50)
{
  const context = DOM.context2d(width, height);
  const path = d3.geoPath(projection, context);

  function render(land) {
    context.clearRect(0, 0, width, height);
    context.beginPath(), path(sphere), context.fillStyle = "#fff", context.fill();
    context.beginPath(), path(land), context.fillStyle = "#000", context.fill();
    context.beginPath(), path(sphere), context.stroke();
  }

  return d3.select(context.canvas)
    .call(zoom(projection)
        .on("zoom.render", () => render(land110))
        .on("end.render", () => render(land50)))
    .call(() => render(land50))
    .node();
}
);
  main.variable(observer("zoom")).define("zoom", ["d3","versor"], function(d3,versor){return(
function zoom(projection, {
  // Capture the projection’s original scale, before any zooming.
  scale = projection._scale === undefined
    ? (projection._scale = projection.scale()) 
    : projection._scale,
  scaleExtent = [0.8, 8]
} = {}) {
  let v0, q0, r0, a0, tl;

  const zoom = d3.zoom()
      .scaleExtent(scaleExtent.map(x => x * scale))
      .on("zoom", zoomed);

  function point(event, that) {
    const t = d3.pointers(event, that);

    if (t.length !== tl) {
      tl = t.length;
      if (tl > 1) a0 = Math.atan2(t[1][1] - t[0][1], t[1][0] - t[0][0]);
      zoomstarted.call(that, event);
    }

    return tl > 1
      ? [
          d3.mean(t, p => p[0]),
          d3.mean(t, p => p[1]),
          Math.atan2(t[1][1] - t[0][1], t[1][0] - t[0][0])
        ]
      : t[0];
  }

  function zoomstarted(event) {
    v0 = versor.cartesian(projection.invert(point(event, this)));
    q0 = versor((r0 = projection.rotate()));
  }

  function zoomed(event) {
    projection.scale(event.transform.k);
    const pt = point(event, this);
    const v1 = versor.cartesian(projection.rotate(r0).invert(pt));
    const delta = versor.delta(v0, v1);
    let q1 = versor.multiply(q0, delta);

    // For multitouch, compose with a rotation around the axis.
    if (pt[2]) {
      const d = (pt[2] - a0) / 2;
      const s = -Math.sin(d);
      const c = Math.sign(Math.cos(d));
      q1 = versor.multiply([Math.sqrt(1 - s * s), 0, 0, c * s], q1);
    }

    projection.rotate(versor.rotation(q1));
    
    // In vicinity of the antipode (unstable) of q0, restart.
    if (delta[0] < 0.7) zoomstarted.call(this, event);
  }

   return Object.assign(selection => selection
      .property("__zoom", d3.zoomIdentity.scale(projection.scale()))
      .call(zoom), {
    on(type, ...options) {
      return options.length
          ? (zoom.on(type, ...options), this)
          : zoom.on(type);
    }
  });
}
)});
  main.variable(observer("projection")).define("projection", ["d3"], function(d3){return(
d3.geoOrthographic().precision(0.1)
)});
  main.variable(observer("height")).define("height", ["d3","projection","width","sphere"], function(d3,projection,width,sphere)
{
  const [[x0, y0], [x1, y1]] = d3.geoPath(projection.fitWidth(width, sphere)).bounds(sphere);
  const dy = Math.ceil(y1 - y0), l = Math.min(Math.ceil(x1 - x0), dy);
  projection.scale(projection.scale() * (l - 1) / l).precision(0.2);
  return dy;
}
);
  main.variable(observer("sphere")).define("sphere", function(){return(
{type: "Sphere"}
)});
  main.variable(observer("land50")).define("land50", ["FileAttachment","topojson"], function(FileAttachment,topojson){return(
FileAttachment("land-50m.json").json().then(world => topojson.feature(world, world.objects.land))
)});
  main.variable(observer("land110")).define("land110", ["FileAttachment","topojson"], function(FileAttachment,topojson){return(
FileAttachment("land-110m.json").json().then(world => topojson.feature(world, world.objects.land))
)});
  main.variable(observer("versor")).define("versor", ["require"], function(require){return(
require("versor@0.0.4")
)});
  main.variable(observer("topojson")).define("topojson", ["require"], function(require){return(
require("topojson-client@3")
)});
  main.variable(observer("d3")).define("d3", ["require"], function(require){return(
require("d3@6")
)});
  return main;
}
