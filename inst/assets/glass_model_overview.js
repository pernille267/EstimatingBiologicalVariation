// ==========================================================================
//  GLASS MODEL OVERVIEW GRID — Interactivity
//  Handles entrance animations and panel interaction for the 2×2
//  Bayesian model overview layout.
// ==========================================================================

(function () {
  "use strict";

  // ── Constants ──────────────────────────────────────────────────────────
  const GRID_SELECTOR = ".model-overview-grid";
  const PANEL_SELECTOR = ".overview-panel";
  const STAGGER_MS = 80; // delay between panels
  const ENTRANCE_CLASS = "panel-visible";
  const OBSERVED_CLASS = "overview-observed";

  // ── Entrance animation via IntersectionObserver ────────────────────────
  // Panels stagger into view the first time the grid scrolls into the
  // viewport (or is revealed by a tab switch).

  function animatePanels(grid) {
    if (grid.classList.contains(OBSERVED_CLASS)) return;
    grid.classList.add(OBSERVED_CLASS);

    var panels = grid.querySelectorAll(PANEL_SELECTOR);
    panels.forEach(function (panel, i) {
      setTimeout(function () {
        panel.classList.add(ENTRANCE_CLASS);
      }, i * STAGGER_MS);
    });
  }

  var _gridObserver = null;

  function observeGrids() {
    var grids = document.querySelectorAll(
      GRID_SELECTOR + ":not(." + OBSERVED_CLASS + ")"
    );
    if (!grids.length) return;

    // Use IntersectionObserver when available
    if ("IntersectionObserver" in window) {
      if (!_gridObserver) {
        _gridObserver = new IntersectionObserver(
          function (entries) {
            entries.forEach(function (entry) {
              if (entry.isIntersecting) {
                animatePanels(entry.target);
              }
            });
          },
          { threshold: 0.05 }
        );
      }

      grids.forEach(function (grid) {
        _gridObserver.observe(grid);
      });
    } else {
      // Fallback: show immediately
      grids.forEach(function (grid) {
        animatePanels(grid);
      });
    }
  }

  // ── Re-initialise on Shiny content changes ─────────────────────────────
  // When Shiny inserts new content (e.g. tab switch revealing the overview
  // for the first time), the grid may not yet be observed.

  function onShinyReady() {
    observeGrids();

    // Re-observe whenever Shiny replaces UI sections
    // Use namespace to prevent stacking on reconnect
    $(document).off("shiny:value.glassModelOverview")
      .on("shiny:value.glassModelOverview", function () {
        // Small delay to let the DOM settle after render
        setTimeout(observeGrids, 60);
      });
  }

  // ── Bootstrap ─────────────────────────────────────────────────────────
  if (window.Shiny) {
    // Shiny already loaded
    $(document).on("shiny:connected", onShinyReady);
  } else {
    // Wait for Shiny
    $(document).ready(function () {
      if (window.Shiny) {
        $(document).on("shiny:connected", onShinyReady);
      } else {
        observeGrids();
      }
    });
  }
})();
