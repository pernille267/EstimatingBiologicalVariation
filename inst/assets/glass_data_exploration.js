// ==========================================================================
// GLASS DATA EXPLORATION JS — Interactive Row Click Handler
// Handles click-to-exclude / click-to-restore on glassTable rows
// Supports "toggle" mode: excluded rows are highlighted in-place.
// ==========================================================================

(function () {
  "use strict";

  // Delegated click handler for clickable glass tables
  $(document).on("click", ".explore-clickable-table .glass-tr", function (e) {
    // Ignore clicks on header rows
    if ($(this).closest("thead").length) return;

    var $row = $(this);
    var $container = $row.closest(".explore-clickable-table");
    var nsId = $container.attr("data-ns");
    var tableAction = $container.attr("data-action");

    if (!nsId) return;

    // Use data-row-index for sort-safe identification; fall back to DOM index
    var rowIdx = parseInt($row.attr("data-row-index"), 10);
    if (isNaN(rowIdx) || rowIdx < 1) {
      var $allRows = $container.find("tbody .glass-tr");
      rowIdx = $allRows.index($row) + 1;
    }
    if (rowIdx < 1) return;

    // Determine action for toggle mode
    var action = tableAction;
    if (tableAction === "toggle") {
      action = $row.hasClass("explore-row-excluded") ? "restore" : "exclude";
    }

    // Visual feedback — flash the row
    $row.addClass("explore-row-flash");
    setTimeout(function () {
      $row.removeClass("explore-row-flash");
    }, 600);

    // Send to Shiny
    if (typeof Shiny !== "undefined") {
      Shiny.setInputValue(nsId, {
        row: rowIdx,
        action: action,
        timestamp: Date.now()
      }, { priority: "event" });
    }
  });

  // Add hover cursor indicator
  $(document).on("mouseenter", ".explore-clickable-table .glass-tr", function () {
    if ($(this).closest("thead").length) return;
    $(this).css("cursor", "pointer");
  });

})();
