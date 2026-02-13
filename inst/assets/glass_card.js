// 1. Initialize
$(document).on('shiny:connected', function() {
  initGlassCards();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassCards();
});

function initGlassCards() {
  // Attach click listeners to all collapsible headers
  var headers = document.querySelectorAll('.glass-card.collapsible .glass-card-header:not(.initialized)');

  headers.forEach(function(header) {
    header.classList.add('initialized');

    header.addEventListener('click', function(e) {
      // Don't collapse if user clicked inside the toolbar
      if (e.target.closest('.glass-card-toolbar')) {
        return;
      }

      var card = header.closest('.glass-card');
      var id = card.id;

      // Ignore if disabled or not collapsible
      if (card.classList.contains('disabled')) return;
      if (!card.classList.contains('collapsible')) return;

      // Toggle Class
      card.classList.toggle('collapsed');

      // Inform Shiny of the state change (optional, but useful)
      var isCollapsed = card.classList.contains('collapsed');
      Shiny.setInputValue(id + '_collapsed', isCollapsed);
    });
  });
}

// 2. Update Handler
Shiny.addCustomMessageHandler("glass-card-update", function(message) {
  var id = message.id;
  var card = document.getElementById(id);

  if (!card) {
    console.warn("Glass Card Update Failed: Could not find card with ID " + id);
    return;
  }

  // A. Update Title
  if (message.hasOwnProperty('title')) {
    var titleEl = card.querySelector('.glass-card-title');
    if (titleEl) titleEl.innerText = message.title;
  }

  // B. Update Collapsed State
  if (message.hasOwnProperty('collapsed')) {
    // Only apply if card is actually collapsible
    if (card.classList.contains('collapsible')) {
      if (message.collapsed) {
        card.classList.add('collapsed');
      } else {
        card.classList.remove('collapsed');
      }
    }
  }

  // C. Update Disabled State
  if (message.hasOwnProperty('disabled')) {
    if (message.disabled) {
      card.classList.add('disabled');
    } else {
      card.classList.remove('disabled');
    }
  }
});
