$(document).on('shiny:connected', function() {
  initGlassDiagnostics();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassDiagnostics();
});

// Use event delegation so it works even if UI is re-rendered by Shiny
$(document).on('click', '.glass-diag-header', function(e) {
  var container = $(this).closest('.glass-diag-container');
  container.toggleClass('collapsed');
});

function initGlassDiagnostics() {
  // Any specific initialization if needed in future
}
