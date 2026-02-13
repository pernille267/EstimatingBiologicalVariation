// ==========================================================================
// GLASS ROUTER ENGINE
// Replaces standard Shiny Tabs with custom DIV switching
// ==========================================================================

$(document).on('shiny:connected', function() {
  // 1. Initialize Router
  initGlassRouter();
  
  // 1b. Helper for opening URLs in new tabs (e.g. documentation)
  Shiny.addCustomMessageHandler('open_url_new_tab', function(url) {
    window.open(url, '_blank');
  });

  // 2. Register Custom Message Handler
  Shiny.addCustomMessageHandler('glass-sidebar-highlight', function(message) {
    // message: { tabName: "dins", enable: true }

    // 1. Finn selve menyknappen (Forelderen)
    var $navItem = $('.glass-nav-item[data-target="' + message.tabName + '"]');

    // 2. Finn selve varsel-ikonet INNI knappen (Barnet)
    // Dette er trinnet som manglet:
    var $notificationIcon = $navItem.find('.glass-nav-notification');

    if (message.enable) {
      // Nå legges klassen på riktig element:
      // .glass-nav-notification blir til .glass-nav-notification.has-notification
      $notificationIcon.addClass('has-notification');
    } else {
      $notificationIcon.removeClass('has-notification');
    }
  });

  // 3. Sync flyout state from server (e.g. wizard nav buttons)
  // Updates the card title AND sidebar sub-indicator to match a specific
  // flyout item, without requiring an actual flyout click.
  // message: { navTarget: "setup", flyoutLabel: "Upload Data", flyoutIcon: "upload" }
  Shiny.addCustomMessageHandler('glass-sync-flyout', function(message) {
    var $navItem = $('.glass-nav-item[data-target="' + message.navTarget + '"]');
    if ($navItem.length === 0) return;

    // A. Update card title
    var cardId = $navItem.data('card-id');
    if (cardId && message.flyoutLabel) {
      var baseTitle = $navItem.data('title');
      var card = document.getElementById(cardId);
      if (card) {
        var titleEl = card.querySelector('.glass-card-title');
        if (titleEl) titleEl.textContent = baseTitle + ' \u2014 ' + message.flyoutLabel;
      }
    }

    // B. Update sub-indicator icon
    if (message.flyoutIcon) {
      var $subIndicator = $navItem.find('.glass-sub-indicator');
      $subIndicator.empty();
      var $iconEl = $('<i class="fa-solid fa-' + message.flyoutIcon + '"></i>');
      $('<div class="glass-sub-icon-bucket"></div>')
        .append($iconEl)
        .appendTo($subIndicator);
      $navItem.addClass('has-sub-selection');
    }
  });
});

function initGlassRouter() {
  // 1. Set initial state based on "active" class in HTML
  var activeNav = $('.glass-nav-item.active');
  if (activeNav.length > 0) {
    var target = activeNav.data('target');
    switchPage(target);
  }
}

// Global Click Delegation for Navigation
$(document).on('click', '.glass-nav-item', function(e) {
  // Check if click originated from a flyout item - we probably want to handle that separately
  // or let the delegation bubble up but we need to know WHICH target to use.
  // Actually, standard nav item click logic uses $(this).data('target').
  // A flyout item click will bubble up to .glass-nav-item.
  // BUT the target on the flyout item is different from the nav item.
  
  // So we should separate the listeners.
});

// 1. Standard Nav Item Click (ignoring flyout clicks inside)
$(document).on('click', '.glass-nav-item', function(e) {
  // If we clicked inside a flyout menu, do NOT trigger the sidebar item default
  if ($(e.target).closest('.glass-nav-flyout').length > 0) return;

  var $el = $(this);
  var target = $el.data('target');

  activateSidebarItem($el, target);

  // Reset card title to base title (removes flyout suffix)
  var cardId = $el.data('card-id');
  if (cardId) {
    var baseTitle = $el.data('title');
    var card = document.getElementById(cardId);
    if (card) {
      var titleEl = card.querySelector('.glass-card-title');
      if (titleEl) titleEl.textContent = baseTitle;
    }
  }
});

// 2. Flyout Item Click
$(document).on('click', '.flyout-item', function(e) {
  e.stopPropagation(); // Stop bubbling to valid running standard nav item click

  var $flyoutItem = $(this);
  var target = $flyoutItem.data('target');
  
  // Find Parent Nav Item to set it as active visually
  var $parentNavItem = $flyoutItem.closest('.glass-nav-item');
  
  activateSidebarItem($parentNavItem, target);

  // --- TAB SWITCHING (for headless tabset integration) ---
  // If the flyout item carries tabset-id / tab-value data attributes,
  // switch the corresponding (potentially headless) glassTabsetPanel tab.
  var tabsetId = $flyoutItem.data('tabset-id');
  var tabValue = $flyoutItem.data('tab-value');
  if (tabsetId && tabValue) {
    // Small delay ensures the target page is visible before switching tabs
    setTimeout(function() {
      if (typeof switchGlassTab === 'function') {
        var container = document.getElementById(tabsetId);
        if (container) {
          // Buttons exist in DOM even when headless (just hidden via CSS)
          var btn = container.querySelector(
            '.glass-tab-nav .glass-tab-btn[data-value="' + tabValue + '"]'
          );
          if (btn) {
            switchGlassTab(tabsetId, tabValue, btn);
          }
        }
      }
    }, 50);
  }

  // --- CARD TITLE UPDATE ---
  // Update the main card title to "Base Title — Flyout Label".
  // If parent nav item has data-card-id, use that card.
  // Otherwise, find the first card on the target page.
  var cardId = $parentNavItem.data('card-id');
  var flyoutLabel = $flyoutItem.data('label');
  if (flyoutLabel) {
    var baseTitle = $parentNavItem.data('title');
    var titleEl = null;
    if (cardId) {
      var card = document.getElementById(cardId);
      if (card) titleEl = card.querySelector('.glass-card-title');
    } else {
      // Find the first card title on the target page
      var $page = $('.glass-page[data-value="' + target + '"]');
      if ($page.length) titleEl = $page.find('.glass-card-title').first()[0];
    }
    if (titleEl) titleEl.textContent = baseTitle + ' \u2014 ' + flyoutLabel;
  }

  // --- SUB SELECTION LOGIC ---
  // 1. Get Icon
  var $icon = $flyoutItem.find('i').clone();
  
  // 2. Inject into Sub Indicator
  var $subIndicator = $parentNavItem.find('.glass-sub-indicator');
  // Clear any existing content
  $subIndicator.empty();
  // Create bucket and add icon
  $('<div class="glass-sub-icon-bucket"></div>')
      .append($icon)
      .appendTo($subIndicator);
      
  // 3. Mark as having sub-selection
  $parentNavItem.addClass('has-sub-selection');
});

// 3. Smart Flyout Positioning (Expand Upwards if needed)
$(document).on('mouseenter', '.glass-nav-item', function() {
  var $flyout = $(this).find('.glass-nav-flyout');
  if ($flyout.length === 0) return;
  
  // Reset first to allow natural measurement
  $flyout.removeClass('drop-up');
  
  // Measure
  // We need the flyout to be "visible" to measure it, but the CSS transition might delay it.
  // However, `display` is not none, it's just opacity/visibility hidden. it should have layout.
  var rect = $flyout[0].getBoundingClientRect();
  var windowHeight = $(window).height();
  
  // If bottom goes past window (with 20px padding)
  if (rect.bottom > (windowHeight - 20)) {
    $flyout.addClass('drop-up');
  }
});

function activateSidebarItem($el, target) {
  // Remove Pulse Icon if there
  $el.find('.glass-nav-notification').removeClass('has-notification');

  // Visual Update (Sidebar)
  $('.glass-nav-item').removeClass('active').removeClass('has-sub-selection');
  $el.addClass('active');

  // Router Logic
  // Special handling for results sub-pages to route to the main results module
  if (target && target.indexOf('results_') === 0) {
    switchPage('results');
  } else {
    switchPage(target);
  }

  // Notify Shiny
  var sidebarId = $el.closest('.glass-sidebar').data('input-id');
  if (sidebarId) {
    Shiny.setInputValue(sidebarId, target);
  }
}

function switchPage(targetId) {
  // 1. Hide all pages
  $('.glass-page').removeClass('active');

  // 2. Show target page
  // We use [data-value] selector to match standard Shiny patterns or ID
  var $page = $('.glass-page[data-value="' + targetId + '"]');
  $page.addClass('active');

  // 3. Trigger resize event to fix Plots/DT tables rendering in hidden divs
  $(window).trigger('resize');
}
