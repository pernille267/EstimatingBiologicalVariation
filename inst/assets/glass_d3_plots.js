// ==========================================================================
// GLASS D3 PLOTS v1.0 — Biological Variation Chart Engine
// Supports: Subject-Specific CVI, Trace Plots, Posterior Density,
//           Prior Density, CVI vs Concentration, CVI vs RCV
// Features: Zoom, Pan, Tooltips, Download (.png / .tif), DPI choice
// ==========================================================================

(function () {
  "use strict";

  /* ---- Shiny Integration ---- */
  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("update_glass_d3_plot", function (msg) {
      GlassD3Plot.render(msg.id, msg.payload);
    });
  });
  $(document).on("shiny:disconnected", function () {
    GlassD3Plot.destroyAll();
  });

  const instances = new Map();

  const GlassD3Plot = {
    render(containerId, payload) {
      let inst = instances.get(containerId);
      const container = document.getElementById(containerId);
      if (!container) return;
      if (inst && container.querySelectorAll("svg.gd3-svg").length === 0) {
        GlassD3Plot.destroy(containerId);
        inst = null;
      }
      if (!inst) {
        inst = createPlotInstance(containerId);
        instances.set(containerId, inst);
      }
      inst.update(payload);
    },
    destroy(containerId) {
      const inst = instances.get(containerId);
      if (inst) { inst.destroy(); instances.delete(containerId); }
    },
    destroyAll() {
      for (const [id] of instances) GlassD3Plot.destroy(id);
    },
  };
  window.GlassD3Plot = GlassD3Plot;

  /* ---- Constants & Helpers ---- */
  const COLORS = {
    purple: "#605CA8",
    purpleDark: "#4B4885",
    purpleLight: "#7470B8",
    green: "#28A745",
    greenDark: "#1a702e",
    magenta: "#A7288A",
    orange: "#FF9800",
    red: "#DC1932",
    blue: "#1994DC",
    gray: "#999",
    grayLight: "#ccc",
    grayDark: "#333",
    white: "#fff",
    black: "#000",
  };

  const CHAIN_COLORS = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"];

  function safeId(str) { return String(str).replace(/[^a-zA-Z0-9_-]/g, "_"); }
  function fmt(v, d) { return Number(v).toFixed(d == null ? 2 : d); }
  function pct(v, d) { return fmt(v, d == null ? 1 : d) + " %"; }

  function extentOrDefault(values, fallback) {
    const clean = values.filter(v => v != null && isFinite(v));
    if (clean.length === 0) return fallback || [0, 1];
    const e = d3.extent(clean);
    if (e[0] === e[1]) return [e[0] - 1, e[1] + 1];
    return e;
  }

  function niceExtent(values, pad, fallback) {
    const e = extentOrDefault(values, fallback);
    const r = e[1] - e[0];
    const p = pad == null ? 0.05 : pad;
    return [e[0] - r * p, e[1] + r * p];
  }

  function quantile(arr, q) {
    const sorted = arr.slice().sort((a, b) => a - b);
    const pos = (sorted.length - 1) * q;
    const lo = Math.floor(pos);
    const hi = Math.ceil(pos);
    if (lo === hi) return sorted[lo];
    return sorted[lo] + (sorted[hi] - sorted[lo]) * (pos - lo);
  }

  /* ---- Download Utilities ---- */

  function svgToCanvas(svgNode, width, height, dpi, bgColor) {
    return new Promise((resolve, reject) => {
      const scaleFactor = dpi / 96;
      const canvas = document.createElement("canvas");
      canvas.width = width * scaleFactor;
      canvas.height = height * scaleFactor;
      const ctx = canvas.getContext("2d");
      ctx.scale(scaleFactor, scaleFactor);
      if (bgColor) {
        ctx.fillStyle = bgColor;
        ctx.fillRect(0, 0, width, height);
      }
      const serializer = new XMLSerializer();
      const svgString = serializer.serializeToString(svgNode);
      const svgBlob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
      const url = URL.createObjectURL(svgBlob);
      const img = new Image();
      img.onload = function () {
        ctx.drawImage(img, 0, 0, width, height);
        URL.revokeObjectURL(url);
        resolve(canvas);
      };
      img.onerror = reject;
      img.src = url;
    });
  }

  async function downloadPng(svgNode, width, height, dpi, filename) {
    const canvas = await svgToCanvas(svgNode, width, height, dpi, "#ffffff");
    const dataUrl = canvas.toDataURL("image/png");
    const a = document.createElement("a");
    a.href = dataUrl;
    a.download = (filename || "chart") + ".png";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
  }

  async function downloadTiff(svgNode, width, height, dpi, filename) {
    // TIFF export using UTIF.js if available, otherwise fallback to PNG
    const canvas = await svgToCanvas(svgNode, width, height, dpi, "#ffffff");
    const ctx = canvas.getContext("2d");
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);

    if (typeof UTIF !== "undefined") {
      // Use UTIF.js for real TIFF encoding
      const rgba = imageData.data;
      const ifd = {
        t256: [canvas.width],
        t257: [canvas.height],
        t258: [8, 8, 8, 8],
        t259: [1],
        t262: [2],
        t273: [1000],
        t277: [4],
        t278: [canvas.height],
        t279: [canvas.width * canvas.height * 4],
        t282: [[dpi, 1]],
        t283: [[dpi, 1]],
        t284: [1],
        t296: [2],
        t339: [1, 1, 1, 1],
      };
      const tiffData = UTIF.encodeImage(rgba, canvas.width, canvas.height, ifd);
      const blob = new Blob([tiffData], { type: "image/tiff" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = (filename || "chart") + ".tif";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } else {
      // Fallback: download as PNG but with .tif extension (with warning)
      console.warn("UTIF.js not available. Downloading as PNG with .tif extension.");
      const dataUrl = canvas.toDataURL("image/png");
      const a = document.createElement("a");
      a.href = dataUrl;
      a.download = (filename || "chart") + ".tif";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
    }
  }

  /* ---- Build Download Toolbar ---- */
  function buildToolbar(container, svgNode, getSvgDimensions, defaultFilename) {
    const existing = container.querySelector(".gd3-toolbar");
    if (existing) existing.remove();

    const toolbar = document.createElement("div");
    toolbar.className = "gd3-toolbar";

    // DPI selector (no label — DPI suffix in items)
    const dpiSelect = document.createElement("select");
    dpiSelect.className = "gd3-toolbar-select";
    [96, 150, 300, 600, 900, 1200].forEach(v => {
      const opt = document.createElement("option");
      opt.value = v;
      opt.textContent = v + " dpi";
      if (v === 300) opt.selected = true;
      dpiSelect.appendChild(opt);
    });
    toolbar.appendChild(dpiSelect);

    // PNG button (split capsule style matching glassButton)
    const pngBtn = document.createElement("button");
    pngBtn.className = "gd3-toolbar-btn";
    pngBtn.innerHTML = '<span class="gd3-toolbar-btn-icon"><svg viewBox="0 0 16 16" width="13" height="13"><path d="M2 12h12v2H2zM8 2v7m0 0l-3-3m3 3l3-3" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg></span><span class="gd3-toolbar-btn-label">.PNG</span>';
    pngBtn.onclick = function () {
      const dims = getSvgDimensions();
      downloadPng(svgNode, dims.width, dims.height, parseInt(dpiSelect.value), defaultFilename);
    };
    toolbar.appendChild(pngBtn);

    // TIF button (split capsule style)
    const tifBtn = document.createElement("button");
    tifBtn.className = "gd3-toolbar-btn";
    tifBtn.innerHTML = '<span class="gd3-toolbar-btn-icon"><svg viewBox="0 0 16 16" width="13" height="13"><path d="M2 12h12v2H2zM8 2v7m0 0l-3-3m3 3l3-3" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg></span><span class="gd3-toolbar-btn-label">.TIF</span>';
    tifBtn.onclick = function () {
      const dims = getSvgDimensions();
      downloadTiff(svgNode, dims.width, dims.height, parseInt(dpiSelect.value), defaultFilename);
    };
    toolbar.appendChild(tifBtn);

    // Scroll toggle button (vertical scroll on/off)
    const scrollBtn = document.createElement("button");
    scrollBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-scroll";
    const vScrollEnabled = !container.classList.contains("no-vscroll");
    if (vScrollEnabled) scrollBtn.classList.add("active");
    scrollBtn.innerHTML = '<span class="gd3-toolbar-btn-icon"><svg viewBox="0 0 16 16" width="13" height="13"><path d="M8 2v12M8 2l-3 3M8 2l3 3M8 14l-3-3M8 14l3-3" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg></span><span class="gd3-toolbar-btn-label">V-Scroll</span>';
    scrollBtn.onclick = function () {
      container.classList.toggle("no-vscroll");
      scrollBtn.classList.toggle("active");
    };
    toolbar.appendChild(scrollBtn);

    // Reset zoom button (white variant, pushed right)
    const resetBtn = document.createElement("button");
    resetBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-reset";
    resetBtn.innerHTML = '<span class="gd3-toolbar-btn-icon"><svg viewBox="0 0 16 16" width="13" height="13"><path d="M2 8a6 6 0 0111.3-2.8M14 2v3.2h-3.2M14 8a6 6 0 01-11.3 2.8M2 14v-3.2h3.2" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg></span><span class="gd3-toolbar-btn-label">Reset</span>';
    toolbar.appendChild(resetBtn);

    container.insertBefore(toolbar, container.firstChild);

    // Default to no vertical scroll (height matches parent)
    if (!container.classList.contains("no-vscroll")) {
      container.classList.add("no-vscroll");
      scrollBtn.classList.remove("active");
    }

    return { dpiSelect, resetBtn };
  }

  /* ================================================================== */
  /* ---- PLOT INSTANCE FACTORY ---- */
  /* ================================================================== */
  function createPlotInstance(containerId) {
    const container = document.getElementById(containerId);
    if (!container) return { update() {}, destroy() {} };

    const root = d3.select(container);
    container.tabIndex = 0;
    const wheelHandler = function (e) {
      if (e.ctrlKey) { e.preventDefault(); e.stopPropagation(); }
    };
    container.addEventListener("wheel", wheelHandler, { passive: false });

    // Base SVG  
    const svg = root.append("svg").attr("class", "gd3-svg")
      .style("overflow", "visible").attr("xmlns", "http://www.w3.org/2000/svg");

    // Tooltip
    const tooltipId = `gd3-tip-${safeId(containerId)}`;
    d3.select(`#${CSS.escape(tooltipId)}`).remove();
    const tooltip = d3.select("body").append("div").attr("id", tooltipId)
      .attr("class", "gd3-tooltip").style("opacity", 0);

    // Preserve the CSS-specified height before no-vscroll overrides it
    const specifiedHeight = parseInt(container.style.height) || 500;

    // State
    const state = { lastPayload: null, resizeObs: null, zoomBehavior: null, zoomTransform: d3.zoomIdentity };

    // Resize
    state.resizeObs = new ResizeObserver(() => {
      if (state.lastPayload) update(state.lastPayload);
    });
    state.resizeObs.observe(container);

    function destroy() {
      container.removeEventListener("wheel", wheelHandler, { passive: false });
      if (state.resizeObs) state.resizeObs.disconnect();
      tooltip.remove();
      root.selectAll("*").remove();
    }

    /* ---- Tooltip helpers ---- */
    function placeTooltip(cx, cy) {
      const n = tooltip.node();
      if (!n) return;
      const tw = n.offsetWidth || 200;
      const th = n.offsetHeight || 40;
      let x = cx + 14, y = cy - 10;
      if (x + tw > window.innerWidth - 8) x = cx - tw - 14;
      if (y + th > window.innerHeight - 8) y = cy - th - 14;
      if (x < 8) x = 8; if (y < 8) y = 8;
      tooltip.style("left", x + "px").style("top", y + "px");
    }
    function showTip(ev, html) {
      tooltip.html(html).style("opacity", 1);
      placeTooltip(ev.clientX, ev.clientY);
    }
    function moveTip(ev) { placeTooltip(ev.clientX, ev.clientY); }
    function hideTip() { tooltip.style("opacity", 0); }

    /* ================================================================== */
    /* ---- UPDATE (dispatches to correct plot type) ---- */
    /* ================================================================== */
    function update(payload) {
      state.lastPayload = payload;
      if (!payload || !payload.plot_type) return;
      svg.selectAll("*").remove();
      // Reset SVG dimensions so it doesn't inflate the container and
      // cause a feedback loop with the ResizeObserver
      svg.attr("width", null).attr("height", null).attr("viewBox", null);

      const plotType = payload.plot_type;
      const data = payload.data || {};
      const params = payload.params || {};

      switch (plotType) {
        case "subject_cvi":
          renderSubjectCVI(svg, container, data, params);
          break;
        case "cvi_vs_concentration":
          renderCVIvsConcentration(svg, container, data, params);
          break;
        case "cvi_vs_rcv":
          renderCVIvsRCV(svg, container, data, params);
          break;
        case "trace":
          renderTracePlots(svg, container, data, params);
          break;
        case "posterior":
          renderPosteriorDensity(svg, container, data, params);
          break;
        case "prior":
          renderPriorDensity(svg, container, data, params);
          break;
        case "exploration_scatter":
          renderExplorationScatter(svg, container, data, params);
          break;
        case "descriptive_dotplot":
          renderDescriptiveDotplot(svg, container, data, params);
          break;
        case "anova_components":
          renderAnovaComponents(svg, container, data, params);
          break;
        default:
          svg.append("text").attr("x", 20).attr("y", 30).text("Unknown plot type: " + plotType);
      }
    }

    /* ================================================================== */
    /* 1. SUBJECT-SPECIFIC CVI (dot-and-whisker, horizontal)              */
    /* ================================================================== */
    function renderSubjectCVI(svg, container, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;

      const popBand = data.pop_band || {};
      const title = params.title || "Subject-Specific CVs with 95% Credible Intervals";
      const subtitle = params.subtitle || "";

      const n = subjects.length;
      const rowH = Math.max(20, Math.min(36, 600 / n));
      const margin = { top: 70, right: 30, bottom: 60, left: 120 };
      const plotW = Math.max(500, (container.clientWidth || 700) - margin.left - margin.right);
      const plotH = n * rowH;
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      // Defs for clip
      const clipId = "clip-scvi-" + safeId(containerId);
      const defs = svg.append("defs");
      defs.append("clipPath").attr("id", clipId).append("rect")
        .attr("width", plotW).attr("height", plotH);

      // Title
      svg.append("text").attr("class", "gd3-title")
        .attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) {
        svg.append("text").attr("class", "gd3-subtitle")
          .attr("x", totalW / 2).attr("y", 44).text(subtitle);
      }

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const allX = subjects.flatMap(d => [d.lwr, d.upr, d.median]);
      if (popBand.lwr != null) allX.push(popBand.lwr, popBand.upr, popBand.median);
      const xDom = niceExtent(allX, 0.08, [0, 20]);

      const x = d3.scaleLinear().domain(xDom).range([0, plotW]);
      const y = d3.scaleBand().domain(subjects.map(d => d.label)).range([plotH, 0]).padding(0.25);

      // Pop band (green shading)
      if (popBand.lwr != null) {
        g.append("rect").attr("class", "gd3-pop-band")
          .attr("x", x(popBand.lwr)).attr("width", x(popBand.upr) - x(popBand.lwr))
          .attr("y", 0).attr("height", plotH);
        [popBand.lwr, popBand.median, popBand.upr].forEach(v => {
          g.append("line").attr("class", "gd3-pop-line")
            .attr("x1", x(v)).attr("x2", x(v)).attr("y1", 0).attr("y2", plotH);
        });
      }

      // Clip group for zoomed content
      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Error bars + points
      const subjectG = plotArea.selectAll("g.gd3-subject")
        .data(subjects).join("g").attr("class", "gd3-subject");

      subjectG.append("line").attr("class", "gd3-errbar")
        .attr("x1", d => x(d.lwr)).attr("x2", d => x(d.upr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2);

      // Whisker caps
      const capH = Math.min(y.bandwidth() * 0.3, 6);
      subjectG.append("line").attr("class", "gd3-errbar-cap")
        .attr("x1", d => x(d.lwr)).attr("x2", d => x(d.lwr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2 - capH)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2 + capH);
      subjectG.append("line").attr("class", "gd3-errbar-cap")
        .attr("x1", d => x(d.upr)).attr("x2", d => x(d.upr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2 - capH)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2 + capH);

      subjectG.append("circle").attr("class", "gd3-point")
        .attr("cx", d => x(d.median)).attr("cy", d => y(d.label) + y.bandwidth() / 2)
        .attr("r", 4.5)
        .attr("fill", d => d.color || COLORS.magenta)
        .attr("stroke", COLORS.black).attr("stroke-width", 1);

      // Tooltip on points
      subjectG.select("circle")
        .on("pointerenter", function (ev, d) {
          d3.select(this).attr("r", 6.5).attr("stroke-width", 2);
          const html = `<strong>${d.label}</strong><br>
            <span class="gd3-tip-muted">Median CV<sub>P(i)</sub>:</span> ${pct(d.median)}<br>
            <span class="gd3-tip-muted">95% CrI:</span> ${pct(d.lwr)} – ${pct(d.upr)}` +
            (d.mean != null ? `<br><span class="gd3-tip-muted">Mean:</span> ${pct(d.mean)}` : "");
          showTip(ev, html);
        })
        .on("pointermove", moveTip)
        .on("pointerleave", function () {
          d3.select(this).attr("r", 4.5).attr("stroke-width", 1); hideTip();
        });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(8).tickFormat(d => pct(d)));
      const yAxisG = g.append("g").attr("class", "gd3-axis gd3-y-axis").call(d3.axisLeft(y));

      // Axis labels
      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45)
        .text("Within-Individual CV (CVₚ₍ᵢ₎, %)");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -100).text("Subject");

      // Zoom
      const zoom = d3.zoom()
        .scaleExtent([1, 20])
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const newX = event.transform.rescaleX(x);
          xAxisG.call(d3.axisBottom(newX).ticks(8).tickFormat(d => pct(d)));
          // Update pop band
          if (popBand.lwr != null) {
            g.select(".gd3-pop-band")
              .attr("x", newX(popBand.lwr))
              .attr("width", newX(popBand.upr) - newX(popBand.lwr));
            g.selectAll(".gd3-pop-line").each(function (_, i) {
              const v = [popBand.lwr, popBand.median, popBand.upr][i];
              d3.select(this).attr("x1", newX(v)).attr("x2", newX(v));
            });
          }
          subjectG.select(".gd3-errbar").attr("x1", d => newX(d.lwr)).attr("x2", d => newX(d.upr));
          subjectG.selectAll(".gd3-errbar-cap").each(function (d) {
            const el = d3.select(this);
            const xv = el.attr("x1") === el.attr("x2") ? parseFloat(el.attr("x1")) : null;
            // Determine if this is the lower or upper cap by its original data position
          });
          // Simpler approach: re-draw caps
          subjectG.selectAll(".gd3-errbar-cap").remove();
          subjectG.append("line").attr("class", "gd3-errbar-cap")
            .attr("x1", d => newX(d.lwr)).attr("x2", d => newX(d.lwr))
            .attr("y1", d => y(d.label) + y.bandwidth() / 2 - capH)
            .attr("y2", d => y(d.label) + y.bandwidth() / 2 + capH);
          subjectG.append("line").attr("class", "gd3-errbar-cap")
            .attr("x1", d => newX(d.upr)).attr("x2", d => newX(d.upr))
            .attr("y1", d => y(d.label) + y.bandwidth() / 2 - capH)
            .attr("y2", d => y(d.label) + y.bandwidth() / 2 + capH);
          subjectG.select("circle").attr("cx", d => newX(d.median));
        });

      zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");
      g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom)
        .on("dblclick.zoom", function () {
          d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
        });

      // Toolbar
      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "subject_cvi_plot");
      resetBtn.onclick = function () {
        g.select(".gd3-zoom-rect").transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      };
    }

    /* ================================================================== */
    /* 2. CVI vs CONCENTRATION                                            */
    /* ================================================================== */
    function renderCVIvsConcentration(svg, container, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;
      const popBand = data.pop_band || {};
      const title = params.title || "Subject-Specific CVs vs. Concentration";
      const subtitle = params.subtitle || "";

      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const plotW = Math.max(500, (container.clientWidth || 700) - margin.left - margin.right);
      const plotH = Math.max(350, specifiedHeight - margin.top - margin.bottom);
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      const clipId = "clip-cvicon-" + safeId(containerId);
      svg.append("defs").append("clipPath").attr("id", clipId).append("rect")
        .attr("width", plotW).attr("height", plotH);

      // Title
      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const xVals = subjects.map(d => d.concentration);
      const yVals = subjects.flatMap(d => [d.lwr, d.upr, d.median]);
      if (popBand.lwr != null) yVals.push(popBand.lwr, popBand.upr);

      const x = d3.scaleLinear().domain(niceExtent(xVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      // Pop band (horizontal)
      if (popBand.lwr != null) {
        g.append("rect").attr("class", "gd3-pop-band")
          .attr("x", 0).attr("width", plotW)
          .attr("y", y(popBand.upr)).attr("height", y(popBand.lwr) - y(popBand.upr));
        [popBand.lwr, popBand.median, popBand.upr].forEach(v => {
          g.append("line").attr("class", "gd3-pop-line")
            .attr("x1", 0).attr("x2", plotW).attr("y1", y(v)).attr("y2", y(v));
        });
      }

      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Error bars (vertical)
      const ptG = plotArea.selectAll("g.gd3-pt").data(subjects).join("g").attr("class", "gd3-pt");
      ptG.append("line").attr("class", "gd3-errbar")
        .attr("x1", d => x(d.concentration)).attr("x2", d => x(d.concentration))
        .attr("y1", d => y(d.lwr)).attr("y2", d => y(d.upr));

      ptG.append("circle").attr("class", "gd3-point")
        .attr("cx", d => x(d.concentration)).attr("cy", d => y(d.median))
        .attr("r", 4.5).attr("fill", d => d.color || COLORS.magenta)
        .attr("stroke", COLORS.black).attr("stroke-width", 1);

      ptG.select("circle")
        .on("pointerenter", function (ev, d) {
          d3.select(this).attr("r", 6.5).attr("stroke-width", 2);
          showTip(ev, `<strong>${d.label}</strong><br>
            <span class="gd3-tip-muted">Concentration:</span> ${fmt(d.concentration)}<br>
            <span class="gd3-tip-muted">CV<sub>P(i)</sub>:</span> ${pct(d.median)}<br>
            <span class="gd3-tip-muted">95% CrI:</span> ${pct(d.lwr)} – ${pct(d.upr)}`);
        })
        .on("pointermove", moveTip)
        .on("pointerleave", function () {
          d3.select(this).attr("r", 4.5).attr("stroke-width", 1); hideTip();
        });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(8));
      const yAxisG = g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(8).tickFormat(d => pct(d)));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Concentration");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("Within-Individual CV (CVₚ₍ᵢ₎, %)");

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(8));
          yAxisG.call(d3.axisLeft(ny).ticks(8).tickFormat(d => pct(d)));
          if (popBand.lwr != null) {
            g.select(".gd3-pop-band")
              .attr("y", ny(popBand.upr))
              .attr("height", ny(popBand.lwr) - ny(popBand.upr));
            g.selectAll(".gd3-pop-line").each(function (_, i) {
              const v = [popBand.lwr, popBand.median, popBand.upr][i];
              d3.select(this).attr("y1", ny(v)).attr("y2", ny(v));
            });
          }
          ptG.select(".gd3-errbar")
            .attr("x1", d => nx(d.concentration)).attr("x2", d => nx(d.concentration))
            .attr("y1", d => ny(d.lwr)).attr("y2", d => ny(d.upr));
          ptG.select("circle")
            .attr("cx", d => nx(d.concentration)).attr("cy", d => ny(d.median));
        });
      zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");

      g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom)
        .on("dblclick.zoom", function () {
          d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
        });

      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "cvi_vs_concentration");
      resetBtn.onclick = () => g.select(".gd3-zoom-rect").transition().duration(500).call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 3. CVI vs RCV                                                      */
    /* ================================================================== */
    function renderCVIvsRCV(svg, container, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;
      const popRCV = data.pop_rcv || {};
      const title = params.title || "Subject-Specific RCVs with Uncertainty";
      const subtitle = params.subtitle || "Incorporated from CV Credible Intervals";

      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const plotW = Math.max(500, (container.clientWidth || 700) - margin.left - margin.right);
      const plotH = Math.max(350, specifiedHeight - margin.top - margin.bottom);
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const clipId = "clip-rcv-" + safeId(containerId);
      svg.append("defs").append("clipPath").attr("id", clipId).append("rect")
        .attr("width", plotW).attr("height", plotH);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const xVals = subjects.map(d => d.cv_median);
      const yVals = subjects.flatMap(d => [d.upper_rcv_lwr, d.upper_rcv_upr, d.lower_rcv_lwr, d.lower_rcv_upr]);
      const x = d3.scaleLinear().domain(niceExtent(xVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Sort by cv_median for ribbons
      const sorted = subjects.slice().sort((a, b) => a.cv_median - b.cv_median);

      // Upper RCV ribbon
      const areaUpper = d3.area()
        .x(d => x(d.cv_median))
        .y0(d => y(d.upper_rcv_lwr))
        .y1(d => y(d.upper_rcv_upr));
      plotArea.append("path").datum(sorted).attr("class", "gd3-ribbon")
        .attr("d", areaUpper).attr("fill", COLORS.green).attr("fill-opacity", 0.35);

      // Lower RCV ribbon
      const areaLower = d3.area()
        .x(d => x(d.cv_median))
        .y0(d => y(d.lower_rcv_lwr))
        .y1(d => y(d.lower_rcv_upr));
      plotArea.append("path").datum(sorted).attr("class", "gd3-ribbon")
        .attr("d", areaLower).attr("fill", COLORS.green).attr("fill-opacity", 0.35);

      // Median RCV lines
      const lineUpper = d3.line().x(d => x(d.cv_median)).y(d => y(d.upper_rcv_median));
      const lineLower = d3.line().x(d => x(d.cv_median)).y(d => y(d.lower_rcv_median));
      plotArea.append("path").datum(sorted).attr("d", lineUpper)
        .attr("fill", "none").attr("stroke", COLORS.black).attr("stroke-width", 1.5);
      plotArea.append("path").datum(sorted).attr("d", lineLower)
        .attr("fill", "none").attr("stroke", COLORS.black).attr("stroke-width", 1.5);

      // Population RCV lines
      if (popRCV.upper != null) {
        g.append("line").attr("class", "gd3-ref-line")
          .attr("x1", 0).attr("x2", plotW).attr("y1", y(popRCV.upper)).attr("y2", y(popRCV.upper));
        g.append("line").attr("class", "gd3-ref-line")
          .attr("x1", 0).attr("x2", plotW).attr("y1", y(popRCV.lower)).attr("y2", y(popRCV.lower));
      }

      // Points
      const ptG = plotArea.selectAll("g.gd3-rcv-pt").data(sorted).join("g").attr("class", "gd3-rcv-pt");
      ptG.append("circle").attr("cx", d => x(d.cv_median)).attr("cy", d => y(d.upper_rcv_median))
        .attr("r", 4).attr("fill", COLORS.purple).attr("stroke", COLORS.black).attr("stroke-width", 1);
      ptG.append("circle").attr("cx", d => x(d.cv_median)).attr("cy", d => y(d.lower_rcv_median))
        .attr("r", 4).attr("fill", COLORS.purple).attr("stroke", COLORS.black).attr("stroke-width", 1);

      // Tooltip on points
      ptG.selectAll("circle")
        .on("pointerenter", function (ev, d) {
          d3.select(this).attr("r", 6).attr("stroke-width", 2);
          showTip(ev, `<strong>${d.label}</strong><br>
            <span class="gd3-tip-muted">CV<sub>P(i)</sub>:</span> ${pct(d.cv_median)}<br>
            <span class="gd3-tip-muted">Upper RCV:</span> ${pct(d.upper_rcv_median)}<br>
            <span class="gd3-tip-muted">Lower RCV:</span> ${pct(d.lower_rcv_median)}`);
        })
        .on("pointermove", moveTip)
        .on("pointerleave", function () { d3.select(this).attr("r", 4).attr("stroke-width", 1); hideTip(); });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(10).tickFormat(d => pct(d)));
      const yAxisG = g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(10).tickFormat(d => pct(d)));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Within-Individual CV (CVₚ₍ᵢ₎, %)");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("RCV (%)");

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(10).tickFormat(d => pct(d)));
          yAxisG.call(d3.axisLeft(ny).ticks(10).tickFormat(d => pct(d)));
          const au = d3.area().x(d => nx(d.cv_median)).y0(d => ny(d.upper_rcv_lwr)).y1(d => ny(d.upper_rcv_upr));
          const al = d3.area().x(d => nx(d.cv_median)).y0(d => ny(d.lower_rcv_lwr)).y1(d => ny(d.lower_rcv_upr));
          plotArea.selectAll(".gd3-ribbon").each(function (_, i) {
            d3.select(this).attr("d", i === 0 ? au(sorted) : al(sorted));
          });
          const lu = d3.line().x(d => nx(d.cv_median)).y(d => ny(d.upper_rcv_median));
          const ll = d3.line().x(d => nx(d.cv_median)).y(d => ny(d.lower_rcv_median));
          plotArea.selectAll("path:not(.gd3-ribbon)").each(function (_, i) {
            d3.select(this).attr("d", i === 0 ? lu(sorted) : ll(sorted));
          });
          ptG.selectAll("circle").each(function (d) {
            const el = d3.select(this);
            el.attr("cx", nx(d.cv_median));
            // Determine if upper or lower by checking index within parent
            const isFirst = this === el.node().parentNode.firstElementChild;
            el.attr("cy", isFirst ? ny(d.upper_rcv_median) : ny(d.lower_rcv_median));
          });
          if (popRCV.upper != null) {
            g.selectAll(".gd3-ref-line").each(function (_, i) {
              const v = i === 0 ? popRCV.upper : popRCV.lower;
              d3.select(this).attr("y1", ny(v)).attr("y2", ny(v));
            });
          }
        });
      zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");

      g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom)
        .on("dblclick.zoom", function () {
          d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
        });

      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "cvi_vs_rcv");
      resetBtn.onclick = () => g.select(".gd3-zoom-rect").transition().duration(500).call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 4. TRACE PLOTS (faceted small multiples)                           */
    /* ================================================================== */
    function renderTracePlots(svg, container, data, params) {
      const panels = data.panels || [];
      if (panels.length === 0) return;
      const title = params.title || "Trace Plots";
      const subtitle = params.subtitle || "";

      const nCols = params.ncols || 3;
      const nRows = Math.ceil(panels.length / nCols);
      const gap = 50;
      const outerMargin = { top: 70, right: 60, bottom: 20, left: 10 };
      const panelMargin = { top: 30, right: 10, bottom: 25, left: 55 };

      const availW = Math.max(600, container.clientWidth || 800) - outerMargin.left - outerMargin.right;
      const panelW = (availW - gap * (nCols - 1)) / nCols;
      const panelH = Math.max(140, Math.min(200, panelW * 0.65));

      const totalW = outerMargin.left + nCols * panelW + (nCols - 1) * gap + outerMargin.right;
      const totalH = outerMargin.top + nRows * panelH + (nRows - 1) * gap + outerMargin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      // Collect unique chains for legend
      const allChains = [...new Set(panels.flatMap(p => (p.traces || []).map(t => t.chain)))].sort();

      // Draw legend on right side
      const legendX = totalW - outerMargin.right + 10;
      const legendG = svg.append("g").attr("transform", `translate(${legendX}, ${outerMargin.top})`);
      legendG.append("text").attr("class", "gd3-legend-title").attr("y", 0).text("Chain");
      allChains.forEach((ch, i) => {
        legendG.append("rect").attr("x", 0).attr("y", 12 + i * 18).attr("width", 12).attr("height", 4)
          .attr("fill", CHAIN_COLORS[i % CHAIN_COLORS.length]);
        legendG.append("text").attr("class", "gd3-legend-text").attr("x", 16).attr("y", 17 + i * 18)
          .text("Chain " + ch);
      });

      panels.forEach((panel, idx) => {
        const col = idx % nCols;
        const row = Math.floor(idx / nCols);
        const px = outerMargin.left + col * (panelW + gap);
        const py = outerMargin.top + row * (panelH + gap);
        const innerW = panelW - panelMargin.left - panelMargin.right;
        const innerH = panelH - panelMargin.top - panelMargin.bottom;

        const panelG = svg.append("g").attr("transform", `translate(${px},${py})`);

        // Strip title
        const stripH = 22;
        panelG.append("rect").attr("class", "gd3-strip-bg")
          .attr("x", 0).attr("y", 0).attr("width", panelW).attr("height", stripH).attr("rx", 4);
        panelG.append("text").attr("class", "gd3-strip-text")
          .attr("x", panelW / 2).attr("y", stripH / 2 + 1).text(panel.label || panel.parameter);

        const ig = panelG.append("g").attr("transform", `translate(${panelMargin.left},${panelMargin.top})`);

        // Clip
        const clipId = "clip-trace-" + safeId(containerId) + "-" + idx;
        ig.append("defs").append("clipPath").attr("id", clipId).append("rect")
          .attr("width", innerW).attr("height", innerH);

        const traces = panel.traces || [];
        const allIter = traces.map(d => d.iteration);
        const allVal = traces.map(d => d.value);
        const iterExtent = d3.extent(allIter);
        const valExtent = niceExtent(allVal, 0.04, [0, 1]);

        const x = d3.scaleLinear().domain(iterExtent).range([0, innerW]);
        const y = d3.scaleLinear().domain(valExtent).range([innerH, 0]);

        // Group traces by chain
        const byChain = d3.group(traces, d => d.chain);
        const plotArea = ig.append("g").attr("clip-path", `url(#${clipId})`);

        byChain.forEach((chainData, chainId) => {
          const sorted = chainData.slice().sort((a, b) => a.iteration - b.iteration);
          const ci = allChains.indexOf(chainId);
          const color = CHAIN_COLORS[ci % CHAIN_COLORS.length];

          // Raw trace (transparent)
          const line = d3.line().x(d => x(d.iteration)).y(d => y(d.value));
          plotArea.append("path").datum(sorted)
            .attr("fill", "none").attr("stroke", color).attr("stroke-width", 0.8)
            .attr("stroke-opacity", 0.2).attr("d", line);

          // Smoothed line (bin average approach for performance)
          const nBins = Math.min(80, Math.ceil(sorted.length / 20));
          if (nBins > 2) {
            const binSize = Math.ceil(sorted.length / nBins);
            const smoothed = [];
            for (let b = 0; b < nBins; b++) {
              const start = b * binSize;
              const end = Math.min(start + binSize, sorted.length);
              const slice = sorted.slice(start, end);
              if (slice.length === 0) continue;
              smoothed.push({
                iteration: d3.mean(slice, d => d.iteration),
                value: d3.mean(slice, d => d.value),
              });
            }
            const smoothLine = d3.line().curve(d3.curveBasis)
              .x(d => x(d.iteration)).y(d => y(d.value));
            plotArea.append("path").datum(smoothed)
              .attr("fill", "none").attr("stroke", color).attr("stroke-width", 1.5)
              .attr("d", smoothLine);
          }
        });

        // Axes
        const iterShift = iterExtent[0];
        const xAxisG = ig.append("g").attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${innerH})`);
        const yAxisG = ig.append("g").attr("class", "gd3-axis gd3-y-axis");

        function drawAxes(xS, yS) {
          xAxisG.call(d3.axisBottom(xS).ticks(4).tickFormat(d => {
            return ((d - iterShift) / 1e3).toFixed(1) + "K";
          }));
          const yTicks = yS.ticks(5);
          const yr = yTicks.length > 1 ? yTicks[yTicks.length - 1] - yTicks[0] : 1;
          yAxisG.call(d3.axisLeft(yS).ticks(5).tickFormat(d => {
            const isCV = (panel.is_cv === true);
            const suffix = isCV ? " %" : "";
            if (yr < 1.5) return d.toFixed(2) + suffix;
            if (yr < 15) return d.toFixed(1) + suffix;
            return Math.round(d) + suffix;
          }));
        }
        drawAxes(x, y);

        // Tooltip on hover
        const hoverLine = ig.append("line")
          .attr("y1", 0).attr("y2", innerH)
          .attr("stroke", COLORS.grayLight).attr("stroke-width", 1).attr("stroke-dasharray", "3,3")
          .style("opacity", 0).style("pointer-events", "none");

        ig.append("rect").attr("class", "gd3-hover-rect")
          .attr("width", innerW).attr("height", innerH)
          .attr("fill", "transparent").style("cursor", "crosshair")
          .on("pointerenter", function () { hoverLine.style("opacity", 1); })
          .on("pointermove", function (ev) {
            const [mx] = d3.pointer(ev);
            hoverLine.attr("x1", mx).attr("x2", mx).style("opacity", 1);
            const iter = x.invert(mx);
            let html = `<span class="gd3-tip-muted">Iteration:</span> ${Math.round(iter)}<br>`;
            byChain.forEach((cd, ch) => {
              const closest = cd.reduce((best, d) =>
                Math.abs(d.iteration - iter) < Math.abs(best.iteration - iter) ? d : best, cd[0]);
              const ci = allChains.indexOf(ch);
              html += `<span style="color:${CHAIN_COLORS[ci % CHAIN_COLORS.length]}">Chain ${ch}:</span> ${fmt(closest.value, 3)}<br>`;
            });
            showTip(ev, html);
          })
          .on("pointerleave", function () { hoverLine.style("opacity", 0); hideTip(); });

        // Zoom (Ctrl+wheel)
        const zoom = d3.zoom().scaleExtent([1, 30])
          .translateExtent([[0, 0], [innerW, innerH]])
          .extent([[0, 0], [innerW, innerH]])
          .on("zoom", function (event) {
            const nx = event.transform.rescaleX(x);
            const ny = event.transform.rescaleY(y);
            drawAxes(nx, ny);
            plotArea.selectAll("path").each(function () {
              const path = d3.select(this);
              const pathData = path.datum();
              if (!pathData || !Array.isArray(pathData)) return;
              if (path.attr("stroke-opacity")) {
                // Raw trace
                const line = d3.line().x(d => nx(d.iteration)).y(d => ny(d.value));
                path.attr("d", line);
              } else {
                // Smoothed
                const line = d3.line().curve(d3.curveBasis).x(d => nx(d.iteration)).y(d => ny(d.value));
                path.attr("d", line);
              }
            });
          });
        zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");
        ig.select(".gd3-hover-rect").call(zoom)
          .on("dblclick.zoom", function () {
            d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
          });
      });

      // Toolbar
      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "trace_plots");
      resetBtn.onclick = () => {
        // Reset all zooms — re-render
        if (state.lastPayload) update(state.lastPayload);
      };
    }

    /* ================================================================== */
    /* 5. POSTERIOR DENSITY (faceted small multiples)                      */
    /* ================================================================== */
    function renderPosteriorDensity(svg, container, data, params) {
      const panels = data.panels || [];
      if (panels.length === 0) return;
      const title = params.title || "Posterior Density Plots";
      const subtitle = params.subtitle || "";
      const includeHistogram = params.include_histogram || false;

      const nCols = params.ncols || 3;
      const nRows = Math.ceil(panels.length / nCols);
      const gap = 50;
      const outerMargin = { top: 70, right: 20, bottom: 20, left: 10 };
      const panelMargin = { top: 30, right: 10, bottom: 30, left: 55 };

      const availW = Math.max(600, container.clientWidth || 800) - outerMargin.left - outerMargin.right;
      const panelW = (availW - gap * (nCols - 1)) / nCols;
      const panelH = Math.max(140, Math.min(200, panelW * 0.65));

      const totalW = outerMargin.left + nCols * panelW + (nCols - 1) * gap + outerMargin.right;
      const totalH = outerMargin.top + nRows * panelH + (nRows - 1) * gap + outerMargin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      panels.forEach((panel, idx) => {
        const col = idx % nCols;
        const row = Math.floor(idx / nCols);
        const px = outerMargin.left + col * (panelW + gap);
        const py = outerMargin.top + row * (panelH + gap);
        const innerW = panelW - panelMargin.left - panelMargin.right;
        const innerH = panelH - panelMargin.top - panelMargin.bottom;

        const panelG = svg.append("g").attr("transform", `translate(${px},${py})`);

        // Strip
        const stripH = 22;
        panelG.append("rect").attr("class", "gd3-strip-bg")
          .attr("x", 0).attr("y", 0).attr("width", panelW).attr("height", stripH).attr("rx", 4);
        panelG.append("text").attr("class", "gd3-strip-text")
          .attr("x", panelW / 2).attr("y", stripH / 2 + 1).text(panel.label || panel.parameter);

        const ig = panelG.append("g").attr("transform", `translate(${panelMargin.left},${panelMargin.top})`);

        const clipId = "clip-post-" + safeId(containerId) + "-" + idx;
        ig.append("defs").append("clipPath").attr("id", clipId).append("rect")
          .attr("width", innerW).attr("height", innerH);

        const values = panel.values || [];
        if (values.length === 0) return;

        // Compute KDE with adaptive Silverman bandwidth
        const valExtent = niceExtent(values, 0.02, [0, 1]);
        const x = d3.scaleLinear().domain(valExtent).range([0, innerW]);

        const bw = silvermanBandwidth(values);
        const kde = kernelDensityEstimator(kernelEpanechnikov(bw), x.ticks(200));
        const density = kde(values);

        const yMax = d3.max(density, d => d[1]) * 1.05;
        const y = d3.scaleLinear().domain([0, yMax]).range([innerH, 0]);

        const plotArea = ig.append("g").attr("clip-path", `url(#${clipId})`);

        // Optional histogram
        if (includeHistogram) {
          const bins = d3.bin().domain(x.domain()).thresholds(x.ticks(60))(values);
          const binMaxY = d3.max(bins, b => b.length / values.length / (b.x1 - b.x0)) * 1.05;
          const yHist = d3.scaleLinear().domain([0, binMaxY]).range([innerH, 0]);
          plotArea.selectAll("rect.gd3-hist-bar").data(bins).join("rect")
            .attr("class", "gd3-hist-bar")
            .attr("x", d => x(d.x0) + 1).attr("width", d => Math.max(0, x(d.x1) - x(d.x0) - 1))
            .attr("y", d => yHist(d.length / values.length / (d.x1 - d.x0)))
            .attr("height", d => innerH - yHist(d.length / values.length / (d.x1 - d.x0)));
        }

        // Density curve
        const area = d3.area()
          .x(d => x(d[0]))
          .y0(innerH)
          .y1(d => y(d[1]))
          .curve(d3.curveBasis);

        if (!includeHistogram) {
          plotArea.append("path").datum(density)
            .attr("class", "gd3-density-fill").attr("d", area);
        }

        const line = d3.line().x(d => x(d[0])).y(d => y(d[1])).curve(d3.curveBasis);
        plotArea.append("path").datum(density)
          .attr("class", "gd3-density-line").attr("d", line);

        // Mean line
        const mean = d3.mean(values);
        plotArea.append("line").attr("class", "gd3-mean-line")
          .attr("x1", x(mean)).attr("x2", x(mean)).attr("y1", 0).attr("y2", innerH);

        // 95% CrI lines
        const cri025 = quantile(values, 0.025);
        const cri975 = quantile(values, 0.975);
        [cri025, cri975].forEach(v => {
          plotArea.append("line").attr("class", "gd3-cri-line")
            .attr("x1", x(v)).attr("x2", x(v)).attr("y1", 0).attr("y2", innerH);
        });

        // Axes
        const isCV = panel.is_cv === true;
        const suffix = isCV ? " %" : "";

        const xAxisG = ig.append("g").attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${innerH})`);
        const valRange = valExtent[1] - valExtent[0];
        xAxisG.call(d3.axisBottom(x).ticks(5).tickFormat(d => {
          if (valRange < 1.5) return d.toFixed(2) + suffix;
          if (valRange < 15) return d.toFixed(1) + suffix;
          return Math.round(d) + suffix;
        }));

        // Hover
        const hoverLine = ig.append("line")
          .attr("y1", 0).attr("y2", innerH)
          .attr("stroke", COLORS.grayLight).attr("stroke-width", 1).attr("stroke-dasharray", "3,3")
          .style("opacity", 0).style("pointer-events", "none");

        ig.append("rect").attr("class", "gd3-hover-rect")
          .attr("width", innerW).attr("height", innerH)
          .attr("fill", "transparent").style("cursor", "crosshair")
          .on("pointerenter", function () { hoverLine.style("opacity", 1); })
          .on("pointermove", function (ev) {
            const [mx] = d3.pointer(ev);
            hoverLine.attr("x1", mx).attr("x2", mx).style("opacity", 1);
            const xVal = x.invert(mx);
            const closestDens = density.reduce((best, d) =>
              Math.abs(d[0] - xVal) < Math.abs(best[0] - xVal) ? d : best, density[0]);
            showTip(ev, `<strong>${panel.label || panel.parameter}</strong><br>
              <span class="gd3-tip-muted">Value:</span> ${fmt(xVal, 3)}${suffix}<br>
              <span class="gd3-tip-muted">Density:</span> ${fmt(closestDens[1], 4)}<br>
              <span class="gd3-tip-muted">Mean:</span> ${fmt(mean, 3)}${suffix}<br>
              <span class="gd3-tip-muted">95% CrI:</span> ${fmt(cri025, 3)} – ${fmt(cri975, 3)}${suffix}`);
          })
          .on("pointerleave", function () { hoverLine.style("opacity", 0); hideTip(); });

        // Zoom
        const zoom = d3.zoom().scaleExtent([1, 30])
          .translateExtent([[0, 0], [innerW, innerH]])
          .extent([[0, 0], [innerW, innerH]])
          .on("zoom", function (event) {
            const nx = event.transform.rescaleX(x);
            xAxisG.call(d3.axisBottom(nx).ticks(5).tickFormat(d => {
              const r = nx.domain()[1] - nx.domain()[0];
              if (r < 1.5) return d.toFixed(2) + suffix;
              if (r < 15) return d.toFixed(1) + suffix;
              return Math.round(d) + suffix;
            }));
            const newArea = d3.area().x(d => nx(d[0])).y0(innerH).y1(d => y(d[1])).curve(d3.curveBasis);
            const newLine = d3.line().x(d => nx(d[0])).y(d => y(d[1])).curve(d3.curveBasis);
            plotArea.select(".gd3-density-fill").attr("d", newArea);
            plotArea.select(".gd3-density-line").attr("d", newLine);
            plotArea.select(".gd3-mean-line").attr("x1", nx(mean)).attr("x2", nx(mean));
            plotArea.selectAll(".gd3-cri-line").each(function (_, i) {
              const v = i === 0 ? cri025 : cri975;
              d3.select(this).attr("x1", nx(v)).attr("x2", nx(v));
            });
            if (includeHistogram) {
              const bins = d3.bin().domain(nx.domain()).thresholds(nx.ticks(60))(values);
              const binMaxY = d3.max(bins, b => b.length ? b.length / values.length / (b.x1 - b.x0) : 0) * 1.05 || 1;
              const yH = d3.scaleLinear().domain([0, binMaxY]).range([innerH, 0]);
              plotArea.selectAll(".gd3-hist-bar").data(bins)
                .join("rect").attr("class", "gd3-hist-bar")
                .attr("x", d => nx(d.x0) + 1).attr("width", d => Math.max(0, nx(d.x1) - nx(d.x0) - 1))
                .attr("y", d => yH(d.length / values.length / (d.x1 - d.x0)))
                .attr("height", d => innerH - yH(d.length / values.length / (d.x1 - d.x0)));
            }
          });
        zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");
        ig.select(".gd3-hover-rect").call(zoom)
          .on("dblclick.zoom", function () {
            d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
          });
      });

      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "posterior_density");
      resetBtn.onclick = () => { if (state.lastPayload) update(state.lastPayload); };
    }

    /* ================================================================== */
    /* 6. PRIOR DENSITY (3x3 grid)                                        */
    /* ================================================================== */
    function renderPriorDensity(svg, container, data, params) {
      // Reuse posterior density renderer with prior-specific defaults
      const priorPanels = data.panels || [];
      if (priorPanels.length === 0) return;

      const title = params.title || "Prior Density Plots";
      const subtitle = params.subtitle || "";

      // Prior densities share the layout logic with posterior — delegate
      renderPosteriorDensity(svg, container,
        { panels: priorPanels },
        {
          title: title,
          subtitle: subtitle,
          ncols: params.ncols || 3,
          include_histogram: false,
        }
      );
    }

    /* ---- KDE helpers ---- */
    function kernelDensityEstimator(kernel, X) {
      return function (V) {
        return X.map(function (x) {
          return [x, d3.mean(V, function (v) { return kernel(x - v); })];
        });
      };
    }

    function kernelEpanechnikov(k) {
      return function (v) {
        v = v / k;
        return Math.abs(v) <= 1 ? 0.75 * (1 - v * v) / k : 0;
      };
    }

    /**
     * Silverman's rule-of-thumb bandwidth for KDE.
     * h = 0.9 * min(sd, IQR/1.34) * n^(-1/5)
     * Falls back to range/10 if both sd and IQR are zero.
     */
    function silvermanBandwidth(values) {
      const n = values.length;
      if (n < 2) return 1;
      const sorted = values.slice().sort((a, b) => a - b);
      const mean = d3.mean(sorted);
      const variance = d3.mean(sorted, v => (v - mean) ** 2);
      const sd = Math.sqrt(variance);
      const q25 = sorted[Math.floor(n * 0.25)];
      const q75 = sorted[Math.floor(n * 0.75)];
      const iqr = q75 - q25;
      const spread = iqr > 0 ? Math.min(sd, iqr / 1.34) : sd;
      if (spread === 0) {
        const range = sorted[n - 1] - sorted[0];
        return range > 0 ? range / 10 : 1;
      }
      return 2 * 0.9 * spread * Math.pow(n, -0.2);
    }

    /* ================================================================== */
    /* 7. EXPLORATION SCATTER (click-to-exclude/include)                   */
    /* ================================================================== */
    function renderExplorationScatter(svg, container, data, params) {
      const points = data.points || [];
      if (points.length === 0) return;

      const grandMean = data.grand_mean;
      const title = params.title || "Data Points";
      const subtitle = params.subtitle || "";
      const nsId = containerId + "_click";
      const viewMode = params.view_mode || "combined"; // "combined" or "faceted"

      if (viewMode === "faceted") {
        renderExplorationScatterFaceted(svg, container, data, params);
        return;
      }

      // ---- COMBINED VIEW ----
      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const plotW = Math.max(500, (container.clientWidth || 700) - margin.left - margin.right);
      const plotH = Math.max(220, Math.min(320, specifiedHeight - margin.top - margin.bottom));
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title")
        .attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) {
        svg.append("text").attr("class", "gd3-subtitle")
          .attr("x", totalW / 2).attr("y", 44).text(subtitle);
      }

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const x = d3.scaleLinear().domain([0, points.length + 1]).range([0, plotW]);
      const yVals = points.map(d => d.y);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      // Grand mean line
      if (grandMean != null) {
        g.append("line")
          .attr("x1", 0).attr("x2", plotW)
          .attr("y1", y(grandMean)).attr("y2", y(grandMean))
          .attr("stroke", COLORS.gray).attr("stroke-width", 1.5)
          .attr("stroke-dasharray", "6,4").attr("opacity", 0.6);
        g.append("text")
          .attr("x", plotW - 5).attr("y", y(grandMean) - 6)
          .attr("text-anchor", "end")
          .attr("fill", COLORS.gray).attr("font-size", "11px")
          .text("Grand Mean: " + fmt(grandMean, 2));
      }

      // Clip
      const clipId = "clip-escat-" + safeId(containerId);
      g.append("defs").append("clipPath").attr("id", clipId).append("rect")
        .attr("width", plotW).attr("height", plotH);
      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Points
      const ptG = plotArea.selectAll("g.gd3-scatter-pt")
        .data(points).join("g").attr("class", "gd3-scatter-pt")
        .style("cursor", "pointer");

      ptG.append("circle")
        .attr("cx", d => x(d.idx))
        .attr("cy", d => y(d.y))
        .attr("r", d => d.excluded ? 5.5 : 4.5)
        .attr("fill", d => d.excluded ? "transparent" : d.color)
        .attr("stroke", d => d.excluded ? COLORS.red : d.color)
        .attr("stroke-width", d => d.excluded ? 2 : 1)
        .attr("opacity", d => d.excluded ? 0.6 : 0.85);

      // X markers for excluded points
      ptG.each(function (d) {
        if (d.excluded) {
          const cx = x(d.idx), cy = y(d.y), sz = 4;
          const el = d3.select(this);
          el.append("line")
            .attr("x1", cx - sz).attr("y1", cy - sz)
            .attr("x2", cx + sz).attr("y2", cy + sz)
            .attr("stroke", COLORS.red).attr("stroke-width", 1.8)
            .style("pointer-events", "none");
          el.append("line")
            .attr("x1", cx - sz).attr("y1", cy + sz)
            .attr("x2", cx + sz).attr("y2", cy - sz)
            .attr("stroke", COLORS.red).attr("stroke-width", 1.8)
            .style("pointer-events", "none");
        }
      });

      // Click → send to Shiny
      ptG.on("click", function (ev, d) {
        ev.stopPropagation();
        if (typeof Shiny !== "undefined") {
          Shiny.setInputValue(nsId, {
            SubjectID: d.SubjectID,
            SampleID: d.SampleID,
            ReplicateID: d.ReplicateID,
            excluded: d.excluded,
            timestamp: Date.now()
          }, { priority: "event" });
        }
        // Quick visual pulse
        d3.select(this).select("circle")
          .transition().duration(120).attr("r", 9)
          .transition().duration(200).attr("r", d.excluded ? 5.5 : 4.5);
      });

      // Tooltip
      ptG.on("pointerenter", function (ev, d) {
        d3.select(this).select("circle").attr("r", 7).attr("stroke-width", 2.5);
        const status = d.excluded
          ? '<span style="color:#DC1932">\u2717 Excluded</span>'
          : '<span style="color:#28A745">\u2713 Included</span>';
        showTip(ev, `<strong>Subject ${d.SubjectID}</strong><br>
          <span class="gd3-tip-muted">Sample:</span> ${d.SampleID}<br>
          <span class="gd3-tip-muted">Replicate:</span> ${d.ReplicateID}<br>
          <span class="gd3-tip-muted">Value:</span> ${fmt(d.y, 4)}<br>
          ${status}<br>
          <em style="font-size:10px;color:#888">Click to ${d.excluded ? "restore" : "exclude"}</em>`);
      })
        .on("pointermove", moveTip)
        .on("pointerleave", function (ev, d) {
          d3.select(this).select("circle")
            .attr("r", d.excluded ? 5.5 : 4.5)
            .attr("stroke-width", d.excluded ? 2 : 1);
          hideTip();
        });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(10).tickFormat(d3.format("d")));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(8));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Observation Index");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("Measurement Value");

      // Subject legend (up to 30)
      const uSubjects = [...new Set(points.map(p => p.SubjectID))];
      if (uSubjects.length <= 30) {
        const legX = plotW - 6, legG = g.append("g").attr("transform", `translate(${legX},0)`);
        legG.append("text").attr("class", "gd3-legend-title").attr("y", -5).attr("text-anchor", "end").text("Subject");
        uSubjects.forEach((s, i) => {
          const c = points.find(p => p.SubjectID === s).color;
          legG.append("circle").attr("cx", -5).attr("cy", 10 + i * 15).attr("r", 3.5).attr("fill", c);
          legG.append("text").attr("class", "gd3-legend-text")
            .attr("x", -12).attr("y", 13 + i * 15).attr("text-anchor", "end").text(s);
        });
      }

      // Excluded count annotation
      const nExcl = points.filter(p => p.excluded).length;
      if (nExcl > 0) {
        g.append("text")
          .attr("x", 5).attr("y", -8)
          .attr("fill", COLORS.red).attr("font-size", "12px").attr("font-weight", "600")
          .text(nExcl + " point" + (nExcl > 1 ? "s" : "") + " excluded");
      }

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(10).tickFormat(d3.format("d")));
          g.select(".gd3-y-axis").call(d3.axisLeft(ny).ticks(8));
          ptG.select("circle")
            .attr("cx", d => nx(d.idx)).attr("cy", d => ny(d.y));
          ptG.selectAll("line").each(function () {
            const l = d3.select(this);
            const pd = d3.select(this.parentNode).datum();
            if (pd && pd.excluded) {
              const cx = nx(pd.idx), cy = ny(pd.y), sz = 4;
              // Determine which diagonal this line belongs to
              const x1Cur = parseFloat(l.attr("x1"));
              const y1Cur = parseFloat(l.attr("y1"));
              // Diagonal 1: top-left to bottom-right
              // Diagonal 2: bottom-left to top-right
              if (y1Cur <= parseFloat(l.attr("y2"))) {
                l.attr("x1", cx - sz).attr("y1", cy - sz)
                  .attr("x2", cx + sz).attr("y2", cy + sz);
              } else {
                l.attr("x1", cx - sz).attr("y1", cy + sz)
                  .attr("x2", cx + sz).attr("y2", cy - sz);
              }
            }
          });
          if (grandMean != null) {
            g.selectAll("line").filter(function () {
              return d3.select(this).attr("stroke-dasharray") === "6,4";
            }).attr("y1", ny(grandMean)).attr("y2", ny(grandMean));
          }
        });
      zoom.filter(ev => ev.ctrlKey || ev.type === "dblclick");

      g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom)
        .on("dblclick.zoom", function () {
          d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
        });

      const { resetBtn } = buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "exploration_scatter");
      resetBtn.onclick = () => g.select(".gd3-zoom-rect").transition().duration(500)
        .call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 7b. EXPLORATION SCATTER — FACETED VIEW                             */
    /* ================================================================== */
    function renderExplorationScatterFaceted(svg, container, data, params) {
      const allPoints = data.points || [];
      if (allPoints.length === 0) return;

      const grandMean = data.grand_mean;
      const title = params.title || "Data Points — Faceted by Subject";
      const subtitle = params.subtitle || "";
      const nsId = containerId + "_click";

      // Group points by subject
      const subjectMap = new Map();
      allPoints.forEach(p => {
        if (!subjectMap.has(p.SubjectID)) subjectMap.set(p.SubjectID, []);
        subjectMap.get(p.SubjectID).push(p);
      });
      const subjects = [...subjectMap.keys()];
      const nSubjects = subjects.length;

      // Layout: determine grid
      const containerW = container.clientWidth || 700;
      const cols = Math.min(nSubjects, containerW < 600 ? 2 : containerW < 900 ? 3 : containerW < 1200 ? 4 : 5);
      const rows = Math.ceil(nSubjects / cols);
      const cellPad = 12;
      const cellW = Math.floor((containerW - cellPad * (cols + 1)) / cols);
      const cellH = Math.max(140, Math.min(180, 160));
      const margin = { top: 56, right: 10, bottom: 10, left: 10 };
      const innerMargin = { top: 24, right: 12, bottom: 28, left: 38 };

      const totalW = containerW;
      const totalH = margin.top + rows * (cellH + cellPad) + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title")
        .attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) {
        svg.append("text").attr("class", "gd3-subtitle")
          .attr("x", totalW / 2).attr("y", 44).text(subtitle);
      }

      // Global y domain (shared across all facets for comparability)
      const allYVals = allPoints.map(d => d.y);
      const yDomain = niceExtent(allYVals, 0.08);

      // Excluded count annotation
      const nExcl = allPoints.filter(p => p.excluded).length;
      if (nExcl > 0) {
        svg.append("text")
          .attr("x", 15).attr("y", 22)
          .attr("fill", COLORS.red).attr("font-size", "12px").attr("font-weight", "600")
          .text(nExcl + " point" + (nExcl > 1 ? "s" : "") + " excluded");
      }

      subjects.forEach((subj, si) => {
        const col = si % cols;
        const row = Math.floor(si / cols);
        const cellX = cellPad + col * (cellW + cellPad);
        const cellY = margin.top + row * (cellH + cellPad);
        const pts = subjectMap.get(subj);
        const subjColor = pts[0].color;

        const facetG = svg.append("g")
          .attr("transform", `translate(${cellX},${cellY})`);

        // Facet background
        facetG.append("rect")
          .attr("width", cellW).attr("height", cellH)
          .attr("rx", 8).attr("ry", 8)
          .attr("fill", "rgba(96,92,168,0.03)")
          .attr("stroke", "rgba(96,92,168,0.12)")
          .attr("stroke-width", 1);

        // Facet title
        facetG.append("text")
          .attr("x", cellW / 2).attr("y", 16)
          .attr("text-anchor", "middle")
          .attr("fill", subjColor)
          .attr("font-size", "11px").attr("font-weight", "700")
          .text("Subject " + subj);

        const plotAreaW = cellW - innerMargin.left - innerMargin.right;
        const plotAreaH = cellH - innerMargin.top - innerMargin.bottom;

        const ig = facetG.append("g")
          .attr("transform", `translate(${innerMargin.left},${innerMargin.top})`);

        // Scales
        const xF = d3.scaleLinear().domain([0, pts.length + 1]).range([0, plotAreaW]);
        const yF = d3.scaleLinear().domain(yDomain).nice().range([plotAreaH, 0]);

        // Grand mean line
        if (grandMean != null) {
          ig.append("line")
            .attr("x1", 0).attr("x2", plotAreaW)
            .attr("y1", yF(grandMean)).attr("y2", yF(grandMean))
            .attr("stroke", COLORS.gray).attr("stroke-width", 1)
            .attr("stroke-dasharray", "4,3").attr("opacity", 0.4);
        }

        // Points
        pts.forEach((d, pi) => {
          const ptG = ig.append("g").attr("class", "gd3-scatter-pt").style("cursor", "pointer");
          const cx = xF(pi + 1), cy = yF(d.y);

          ptG.append("circle")
            .attr("cx", cx).attr("cy", cy)
            .attr("r", d.excluded ? 5 : 4)
            .attr("fill", d.excluded ? "transparent" : d.color)
            .attr("stroke", d.excluded ? COLORS.red : d.color)
            .attr("stroke-width", d.excluded ? 2 : 1)
            .attr("opacity", d.excluded ? 0.6 : 0.85);

          if (d.excluded) {
            const sz = 3.5;
            ptG.append("line")
              .attr("x1", cx - sz).attr("y1", cy - sz)
              .attr("x2", cx + sz).attr("y2", cy + sz)
              .attr("stroke", COLORS.red).attr("stroke-width", 1.5)
              .style("pointer-events", "none");
            ptG.append("line")
              .attr("x1", cx - sz).attr("y1", cy + sz)
              .attr("x2", cx + sz).attr("y2", cy - sz)
              .attr("stroke", COLORS.red).attr("stroke-width", 1.5)
              .style("pointer-events", "none");
          }

          // Click → send to Shiny
          ptG.on("click", function (ev) {
            ev.stopPropagation();
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(nsId, {
                SubjectID: d.SubjectID,
                SampleID: d.SampleID,
                ReplicateID: d.ReplicateID,
                excluded: d.excluded,
                timestamp: Date.now()
              }, { priority: "event" });
            }
            d3.select(this).select("circle")
              .transition().duration(120).attr("r", 8)
              .transition().duration(200).attr("r", d.excluded ? 5 : 4);
          });

          // Tooltip
          ptG.on("pointerenter", function (ev) {
            d3.select(this).select("circle").attr("r", 6.5).attr("stroke-width", 2.5);
            const status = d.excluded
              ? '<span style="color:#DC1932">\u2717 Excluded</span>'
              : '<span style="color:#28A745">\u2713 Included</span>';
            showTip(ev, `<strong>Subject ${d.SubjectID}</strong><br>
              <span class="gd3-tip-muted">Sample:</span> ${d.SampleID}<br>
              <span class="gd3-tip-muted">Replicate:</span> ${d.ReplicateID}<br>
              <span class="gd3-tip-muted">Value:</span> ${fmt(d.y, 4)}<br>
              ${status}<br>
              <em style="font-size:10px;color:#888">Click to ${d.excluded ? "restore" : "exclude"}</em>`);
          })
            .on("pointermove", moveTip)
            .on("pointerleave", function () {
              d3.select(this).select("circle")
                .attr("r", d.excluded ? 5 : 4)
                .attr("stroke-width", d.excluded ? 2 : 1);
              hideTip();
            });
        });

        // Axes (minimal)
        ig.append("g").attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${plotAreaH})`)
          .call(d3.axisBottom(xF).ticks(Math.min(5, pts.length)).tickFormat(d3.format("d")))
          .selectAll("text").attr("font-size", "9px");
        ig.append("g").attr("class", "gd3-axis gd3-y-axis")
          .call(d3.axisLeft(yF).ticks(4))
          .selectAll("text").attr("font-size", "9px");
      });

      buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "exploration_scatter_faceted");
    }

    /* ================================================================== */
    /* 8. DESCRIPTIVE DOTPLOT (strip chart by subject)                     */
    /* ================================================================== */
    function renderDescriptiveDotplot(svg, container, data, params) {
      const points = data.points || [];
      const subjectMeans = data.subject_means || [];
      const grandMean = data.grand_mean;
      const title = params.title || "Data Distribution by Subject";
      const subtitle = params.subtitle || "";

      if (points.length === 0) return;

      const subjects = [...new Set(points.map(p => p.SubjectID))];
      const n = subjects.length;

      const margin = { top: 70, right: 30, bottom: 60, left: 100 };
      const rowH = Math.max(28, Math.min(50, 500 / n));
      const plotW = Math.max(500, (container.clientWidth || 700) - margin.left - margin.right);
      const plotH = n * rowH;
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const yVals = points.map(d => d.y);
      const x = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleBand().domain(subjects).range([0, plotH]).padding(0.2);

      // Alternating row stripes
      subjects.forEach((s, i) => {
        if (i % 2 === 1) {
          g.append("rect")
            .attr("x", 0).attr("y", y(s))
            .attr("width", plotW).attr("height", y.bandwidth())
            .attr("fill", "rgba(96,92,168,0.03)");
        }
      });

      // Grand mean line
      if (grandMean != null) {
        g.append("line")
          .attr("x1", x(grandMean)).attr("x2", x(grandMean))
          .attr("y1", 0).attr("y2", plotH)
          .attr("stroke", COLORS.purple).attr("stroke-width", 2)
          .attr("stroke-dasharray", "6,3").attr("opacity", 0.5);
        g.append("text")
          .attr("x", x(grandMean)).attr("y", -8)
          .attr("text-anchor", "middle")
          .attr("fill", COLORS.purple).attr("font-size", "11px")
          .text("Grand Mean: " + fmt(grandMean, 2));
      }

      // Build subject mean map
      const meanMap = {};
      subjectMeans.forEach(d => { meanMap[d.SubjectID] = d; });

      // Subject mean diamonds + SD whiskers
      subjects.forEach(subj => {
        const sm = meanMap[subj];
        if (!sm) return;
        const cy = y(subj) + y.bandwidth() / 2;
        const cx = x(sm.mean);
        const dSize = 6;

        // SD whisker
        if (sm.sd != null && sm.sd > 0 && isFinite(sm.sd)) {
          g.append("line")
            .attr("x1", x(sm.mean - sm.sd)).attr("x2", x(sm.mean + sm.sd))
            .attr("y1", cy).attr("y2", cy)
            .attr("stroke", COLORS.grayDark).attr("stroke-width", 1)
            .attr("stroke-dasharray", "3,2").attr("opacity", 0.4);
        }

        // Diamond
        g.append("polygon")
          .attr("points",
            `${cx},${cy - dSize} ${cx + dSize},${cy} ${cx},${cy + dSize} ${cx - dSize},${cy}`)
          .attr("fill", sm.color || COLORS.magenta)
          .attr("stroke", COLORS.black).attr("stroke-width", 0.8)
          .attr("opacity", 0.85);
      });

      // Individual data points (jittered)
      const jitter = y.bandwidth() * 0.3;
      // Deterministic jitter using simple hash
      points.forEach((d, i) => {
        const jitterY = ((i * 7 + 3) % 100) / 100 * jitter - jitter / 2;
        const cy = y(d.SubjectID) + y.bandwidth() / 2 + jitterY;
        g.append("circle")
          .attr("cx", x(d.y)).attr("cy", cy)
          .attr("r", 3)
          .attr("fill", d.color || COLORS.magenta)
          .attr("stroke", "white").attr("stroke-width", 0.5)
          .attr("opacity", 0.55)
          .on("pointerenter", function (ev) {
            d3.select(this).attr("r", 5).attr("opacity", 1);
            showTip(ev, `<strong>Subject ${d.SubjectID}</strong><br>
              <span class="gd3-tip-muted">Value:</span> ${fmt(d.y, 4)}`);
          })
          .on("pointermove", moveTip)
          .on("pointerleave", function () {
            d3.select(this).attr("r", 3).attr("opacity", 0.55);
            hideTip();
          });
      });

      // Axes
      g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(8));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Measurement Value");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -80).text("Subject");

      buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "descriptive_dotplot");
    }

    /* ================================================================== */
    /* 9. ANOVA COMPONENTS (horizontal bar chart)                         */
    /* ================================================================== */
    function renderAnovaComponents(svg, container, data, params) {
      const components = data.components || [];
      if (components.length === 0) return;

      const title = params.title || "ANOVA-based Variance Components";
      const subtitle = params.subtitle || "";

      const margin = { top: 70, right: 90, bottom: 60, left: 230 };
      const plotW = Math.max(350, (container.clientWidth || 700) - margin.left - margin.right);
      const barH = 48;
      const gap = 22;
      const plotH = components.length * (barH + gap) - gap;
      const totalW = margin.left + plotW + margin.right;
      const totalH = margin.top + plotH + margin.bottom;

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Determine x-scale domain from values and CI upper bounds
      const allVals = components.flatMap(d => {
        const vals = [d.value];
        if (d.upper != null && isFinite(d.upper)) vals.push(d.upper);
        return vals;
      });
      const maxVal = d3.max(allVals) * 1.25 || 1;
      const x = d3.scaleLinear().domain([0, maxVal]).range([0, plotW]);
      const y = d3.scaleBand()
        .domain(components.map(d => d.label))
        .range([0, plotH]).padding(0.3);

      const barG = g.selectAll("g.gd3-bar-group")
        .data(components).join("g").attr("class", "gd3-bar-group");

      // Background bars
      barG.append("rect")
        .attr("x", 0).attr("y", d => y(d.label))
        .attr("width", plotW).attr("height", y.bandwidth())
        .attr("fill", "rgba(240,240,240,0.7)").attr("rx", 6);

      // Value bars with gradient-like colour
      barG.append("rect")
        .attr("x", 0).attr("y", d => y(d.label))
        .attr("width", d => Math.max(4, x(d.value)))
        .attr("height", y.bandwidth())
        .attr("fill", d => d.color).attr("rx", 6)
        .attr("opacity", 0.85);

      // CI error bars (horizontal whiskers)
      barG.each(function (d) {
        const hasCI = d.lower != null && d.upper != null &&
                      isFinite(d.lower) && isFinite(d.upper) &&
                      !isNaN(d.lower) && !isNaN(d.upper);
        if (!hasCI) return;

        const barCenter = y(d.label) + y.bandwidth() / 2;
        const xLower = x(d.lower);
        const xUpper = x(d.upper);
        const capH = 10;
        const sel = d3.select(this);

        // Horizontal line
        sel.append("line")
          .attr("x1", xLower).attr("x2", xUpper)
          .attr("y1", barCenter).attr("y2", barCenter)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);

        // Left cap
        sel.append("line")
          .attr("x1", xLower).attr("x2", xLower)
          .attr("y1", barCenter - capH / 2).attr("y2", barCenter + capH / 2)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);

        // Right cap
        sel.append("line")
          .attr("x1", xUpper).attr("x2", xUpper)
          .attr("y1", barCenter - capH / 2).attr("y2", barCenter + capH / 2)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);
      });

      // Value labels (shifted right to accommodate potential CI upper bound)
      barG.append("text")
        .attr("x", d => {
          const hasCI = d.upper != null && isFinite(d.upper) && !isNaN(d.upper);
          const rightEdge = hasCI ? Math.max(x(d.value), x(d.upper)) : x(d.value);
          return Math.max(4, rightEdge) + 10;
        })
        .attr("y", d => y(d.label) + y.bandwidth() / 2)
        .attr("dy", "0.35em")
        .attr("fill", COLORS.grayDark)
        .attr("font-size", "14px").attr("font-weight", "700")
        .text(d => fmt(d.value, 2) + " %");

      // Tooltip
      barG.on("pointerenter", function (ev, d) {
        d3.select(this).selectAll("rect").filter((_, i) => i === 1).attr("opacity", 1);
        const hasCI = d.lower != null && d.upper != null &&
                      isFinite(d.lower) && isFinite(d.upper);
        let tipHtml = `<strong>${d.label}</strong><br>
          <span class="gd3-tip-muted">CV:</span> ${fmt(d.value, 2)} %`;
        if (hasCI) {
          tipHtml += `<br><span class="gd3-tip-muted">95% CI:</span> [${fmt(d.lower, 2)}, ${fmt(d.upper, 2)}] %`;
        }
        showTip(ev, tipHtml);
      })
        .on("pointermove", moveTip)
        .on("pointerleave", function () {
          d3.select(this).selectAll("rect").filter((_, i) => i === 1).attr("opacity", 0.85);
          hideTip();
        });

      // Axes
      g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(6).tickFormat(d => fmt(d, 1) + " %"));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45)
        .text("Coefficient of Variation (%)");

      buildToolbar(container, svg.node(),
        () => ({ width: totalW, height: totalH }), "anova_components");
    }

    return { update, destroy };
  }
})();
