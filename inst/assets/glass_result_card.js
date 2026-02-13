// 1. Initialize
$(document).on('shiny:connected', function() {
  initGlassResultCards();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassResultCards();
});

function initGlassResultCards() {
  // Find all result cards
  var cards = document.querySelectorAll('.glass-result-card:not(.initialized)');

  cards.forEach(function(card) {
    card.classList.add('initialized');

    // Stop propagation on toolbar clicks
    // This prevents clicks on "Calculate" from bubbling up if we ever add click listeners to the header
    var toolbar = card.querySelector('.glass-result-toolbar');
    if (toolbar) {
      toolbar.addEventListener('click', function(e) {
        e.stopPropagation();
      });
    }
  });
}

// 2. Update Handler (Placeholder for future features)
Shiny.addCustomMessageHandler("glass-result-card-update", function(message) {
  var id = message.id;
  var card = document.getElementById(id);

  if (!card) return;

  // Update Title
  if (message.hasOwnProperty('title')) {
    var titleEl = card.querySelector('.glass-result-title');
    if (titleEl) titleEl.innerText = message.title;
  }
});
