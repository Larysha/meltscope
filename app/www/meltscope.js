// MeltScope browser side: draws figures sent from R with Plotly.js, reports plate-map clicks
// back to R, and makes results tables sortable by clicking a column header.

Shiny.addCustomMessageHandler("ms-plot", function (msg) {
  const el = document.getElementById(msg.id);
  if (!el) return;
  if (msg.message !== undefined) {
    // No figure: show the reason (e.g. "Upload a file...") instead
    Plotly.purge(el);
    el._msClick = false;
    el.textContent = "";
    if (msg.message) {
      const note = document.createElement("div");
      note.className = "ms-plot-msg";
      note.textContent = msg.message;
      el.appendChild(note);
    }
    return;
  }
  const note = el.querySelector(".ms-plot-msg");
  if (note) note.remove();
  el._msFit = msg.fitWells || null;
  el._msWellSize = null;
  Plotly.react(el, msg.data, msg.layout, Object.assign({ responsive: true }, msg.config))
    .then(function () { fitWells(el); });
  if (!el._msObserver && window.ResizeObserver) {
    // Redraw when the plot's box changes size (window, sidebar toggle, layout reflow)
    el._msObserver = new ResizeObserver(function () {
      if (el._fullLayout) Plotly.Plots.resize(el).then(function () { fitWells(el); });
    });
    el._msObserver.observe(el);
  }
  if (msg.clickInput && !el._msClick) {
    el._msClick = true;
    el.on("plotly_click", function (ev) {
      const point = ev.points[0];
      if (point && point.customdata !== undefined) {
        Shiny.setInputValue(msg.clickInput, point.customdata, { priority: "event" });
      }
    });
  }
});

// Plate map: size the well markers so the grid fills the plot without overlapping
function fitWells(el) {
  const fit = el._msFit, area = el._fullLayout && el._fullLayout._size;
  if (!fit || !area) return;
  const size = Math.max(4, Math.floor(0.82 * Math.min(area.w / fit.cols, area.h / fit.rows)));
  if (size === el._msWellSize) return;
  el._msWellSize = size;
  Plotly.restyle(el, { "marker.size": size });
}

document.addEventListener("click", function (ev) {
  const th = ev.target.closest("table.ms-sortable th");
  if (!th) return;
  const table = th.closest("table");
  const tbody = table.tBodies[0];
  const col = Array.from(th.parentNode.children).indexOf(th);
  const asc = th.dataset.sort !== "asc";
  table.querySelectorAll("th").forEach(function (h) { delete h.dataset.sort; });
  th.dataset.sort = asc ? "asc" : "desc";
  const text = function (row) { return row.children[col].textContent; };
  const rows = Array.from(tbody.rows).sort(function (a, b) {
    const x = text(a), y = text(b), nx = parseFloat(x), ny = parseFloat(y);
    const c = !isNaN(nx) && !isNaN(ny) ? nx - ny : x.localeCompare(y, undefined, { numeric: true });
    return asc ? c : -c;
  });
  rows.forEach(function (r) { tbody.appendChild(r); });
});
