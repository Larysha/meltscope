// MeltScope browser side: draws figures sent from R with Plotly.js, handles plate-map selection,
// and makes results tables sortable by clicking a column header.

Shiny.addCustomMessageHandler("ms-plot", function (msg) {
  const el = document.getElementById(msg.id);
  if (!el) return;
  if (msg.message !== undefined) {
    // No figure: show the reason (e.g. "Upload a file...") instead
    Plotly.purge(el);
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
  Plotly.react(el, msg.data, msg.layout, Object.assign({ responsive: true }, msg.config));
  if (!el._msObserver && window.ResizeObserver) {
    // Redraw when the plot's box changes size (window, sidebar toggle, layout reflow)
    el._msObserver = new ResizeObserver(function () {
      if (el._fullLayout) Plotly.Plots.resize(el);
    });
    el._msObserver.observe(el);
  }
});

// Close any open settings menu (after a file is loaded)
Shiny.addCustomMessageHandler("ms-close-menus", function (msg) {
  document.querySelectorAll('.ms-top [data-bs-toggle="dropdown"]').forEach(function (b) {
    bootstrap.Dropdown.getOrCreateInstance(b).hide();
  });
});

// ---- Plate map selection -------------------------------------------------------------
// The plate is an HTML grid (R: output$plate). Each cell has data-r / data-c; 0 means a header,
// i.e. the whole row / column (corner = everything). Pressing on a cell and dragging selects the
// rectangle between the two cells; Shift / Cmd / Ctrl adds to the current selection (a plain click
// on an already selected well with a modifier removes it). The selection lives here, so dragging
// is instant, and goes to R as input$plate_sel when the pointer is released.
(function () {
  let sel = new Set(), base = null, start = null, last = null;

  const grid = function () { return document.querySelector("#plate .ms-plate"); };
  const cellAt = function (x, y) {
    const el = document.elementFromPoint(x, y);
    return el && el.closest("#plate .ms-plate [data-r]");
  };
  const span = function (v, n) { return v === 0 ? [1, n] : [v, v]; };

  function paint() {
    const g = grid();
    if (g) g.querySelectorAll(".ms-well[data-well]").forEach(function (e) {
      e.classList.toggle("sel", sel.has(e.dataset.well));
    });
  }
  function send() { Shiny.setInputValue("plate_sel", Array.from(sel)); }

  function update() {
    const wells = grid().querySelectorAll(".ms-well[data-well]");
    let nr = 0, nc = 0;
    grid().querySelectorAll("[data-r]").forEach(function (e) {
      nr = Math.max(nr, +e.dataset.r); nc = Math.max(nc, +e.dataset.c);
    });
    const a = [span(+start.dataset.r, nr), span(+start.dataset.c, nc)];
    const b = [span(+last.dataset.r, nr), span(+last.dataset.c, nc)];
    const r0 = Math.min(a[0][0], b[0][0]), r1 = Math.max(a[0][1], b[0][1]);
    const c0 = Math.min(a[1][0], b[1][0]), c1 = Math.max(a[1][1], b[1][1]);
    sel = new Set(base);
    wells.forEach(function (e) {
      const r = +e.dataset.r, c = +e.dataset.c;
      if (r >= r0 && r <= r1 && c >= c0 && c <= c1) sel.add(e.dataset.well);
    });
    paint();
  }

  document.addEventListener("pointerdown", function (ev) {
    if (ev.button !== 0) return;
    const cell = ev.target.closest("#plate .ms-plate [data-r]");
    if (!cell) return;
    ev.preventDefault();
    const add = ev.shiftKey || ev.metaKey || ev.ctrlKey;
    base = add ? new Set(sel) : new Set();
    start = last = cell;
    update();
  });
  document.addEventListener("pointermove", function (ev) {
    if (!start) return;
    const cell = cellAt(ev.clientX, ev.clientY);
    if (cell && cell !== last) { last = cell; update(); }
  });
  function finish(ev) {
    if (!start) return;
    const w = start.dataset.well;
    if (start === last && w && base.has(w) && (ev.shiftKey || ev.metaKey || ev.ctrlKey)) { sel.delete(w); paint(); }
    start = last = null;
    send();
  }
  document.addEventListener("pointerup", finish);
  document.addEventListener("pointercancel", finish);
  document.addEventListener("keydown", function (ev) {
    if (ev.key === "Escape" && sel.size) window.msClearSelection();
  });

  window.msClearSelection = function () { sel = new Set(); paint(); send(); };
  Shiny.addCustomMessageHandler("ms-clear-selection", function (msg) { window.msClearSelection(); });
  // The grid is re-rendered whenever calls change: put the selection outline back
  document.addEventListener("DOMContentLoaded", function () {
    const p = document.getElementById("plate");
    if (p) new MutationObserver(paint).observe(p, { childList: true });
  });
})();

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
