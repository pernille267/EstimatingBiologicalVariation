// 1. Initialize on load
$(document).on('shiny:connected', function() {
  initGlassTabs();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassTabs();
});


// 3. Custom Message Handler for Server-Side Switching
Shiny.addCustomMessageHandler('glass-tab-switch', function(message) {
  var tabsetId = message.inputId;
  var value = message.value;
  
  // Find the button with this value within the tabset nav
  var container = document.getElementById(tabsetId);
  if (container) {
    var btn = container.querySelector('.glass-tab-btn[data-value="' + value + '"]');
    if (btn) {
      // Trigger the standard click logic
      switchGlassTab(tabsetId, value, btn);
    }
  }
});


function initGlassTabs() {
  // Find all tabsets that haven't been initialized
  var tabsets = document.querySelectorAll('.glass-tabset-container:not(.initialized)');

  tabsets.forEach(function(container) {
    var id = container.id;
    // Find the initially active tab (if any)
    // We use querySelector to find the FIRST matching child, which safeguards against finding nested ones
    // provided the HTML structure is standard (nav is the first child).
    var nav = container.querySelector('.glass-tab-nav');
    if (nav) {
      var activeBtn = nav.querySelector('.glass-tab-btn.active');
      if (activeBtn) {
        var value = activeBtn.dataset.value;
        Shiny.setInputValue(id, value);
      }
    }
    container.classList.add('initialized');
  });
}

// 2. Click Handler
window.switchGlassTab = function(tabsetId, value, element) {
  var container = document.getElementById(tabsetId);
  if (!container) return;

  // --- A. Update Navigation Buttons ---
  // We first find the specific navigation bar for THIS tabset (the first one found)
  var navBar = container.querySelector('.glass-tab-nav');

  if (navBar) {
    // We convert the HTMLCollection (children) to an Array to iterate easily
    // Using .children ensures we ONLY target the buttons in this specific bar,
    // ignoring any buttons inside nested tabsets.
    var navBtns = Array.from(navBar.children);

    navBtns.forEach(function(btn) {
      // Ensure we only touch items that are actually glass tab buttons
      if (btn.classList.contains('glass-tab-btn')) {
        btn.classList.remove('active');
      }
    });
  }

  // Add active class to the clicked button
  element.classList.add('active');

  // --- B. Switch Content Panes ---
  // We look for the content wrapper inside this specific tabset
  var contentWrapper = container.querySelector('.glass-tab-content-wrapper');

  if (contentWrapper) {
    // Again, use .children to strictly target the panes belonging to this tabset
    // and avoid accidentally closing panes in nested tabsets.
    var panes = Array.from(contentWrapper.children);

    panes.forEach(function(pane) {
      // Only toggle if it is actually a pane
      if (pane.classList.contains('glass-tab-pane')) {
        pane.classList.remove('active');
        if (pane.dataset.value === value) {
          pane.classList.add('active');
        }
      }
    });
  }

  // --- C. Update Shiny Input ---
  Shiny.setInputValue(tabsetId, value);

  // Optional: Trigger a window resize event to force plots to redraw correctly
  setTimeout(function() {
    window.dispatchEvent(new Event('resize'));
  }, 50);
};
