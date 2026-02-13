// 1. Initialize on load
$(document).on('shiny:connected', function() {
  initGlassDropdowns();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassDropdowns();
});

function initGlassDropdowns() {
  var dropdowns = document.querySelectorAll('.glass-dropdown-container:not(.initialized)');
  dropdowns.forEach(function(container) {
    var id = container.id.replace('container-', '');
    var initialValue = container.dataset.selected;
    if (initialValue) Shiny.setInputValue(id, initialValue);
    container.classList.add('initialized');
  });
}

// 2. Open/Close Logic
window.toggleGlassMenu = function(id) {
  var btn = document.getElementById('btn-' + id);
  if (!btn) return;
  if (btn.classList.contains('disabled')) return;

  var container = document.getElementById('container-' + id);
  var menu = document.getElementById('menu-' + id);

  closeAllGlassMenus(id);

  var isOpening = !menu.classList.contains('show');

  if (isOpening) {
    // --- PORTAL MOVE ---
    if (menu.parentElement !== document.body) {
      document.body.appendChild(menu);
    }

    // --- POSITIONING ---
    var rect = container.getBoundingClientRect();
    var viewportHeight = window.innerHeight;
    var estimatedMenuHeight = 300; // Matches CSS max-height
    var spaceBelow = viewportHeight - rect.bottom;

    // Flip Logic: Open UP if tight on space below
    var openUpwards = (spaceBelow < (estimatedMenuHeight + 20)) && (rect.top > spaceBelow);

    menu.style.position = 'fixed';
    menu.style.left = rect.left + 'px';
    menu.style.width = rect.width + 'px';
    menu.style.zIndex = '99999';

    if (openUpwards) {
      menu.style.bottom = (viewportHeight - rect.top + 5) + 'px';
      menu.style.top = 'auto';
      menu.style.transformOrigin = "bottom center";
    } else {
      menu.style.top = (rect.bottom + 5) + 'px';
      menu.style.bottom = 'auto';
      menu.style.transformOrigin = "top center";
    }

    menu.classList.add('show');
    btn.classList.add('active');

    // Add listeners to close on outside interaction
    // Note: We use 'true' (capture) for scroll to catch page scrolling,
    // but we filter it inside the function now.
    window.addEventListener('scroll', window.closeGlassMenusOnScroll, true);
    window.addEventListener('resize', window.closeGlassMenusOnScroll);

  } else {
    menu.classList.remove('show');
    btn.classList.remove('active');
    window.removeEventListener('scroll', window.closeGlassMenusOnScroll, true);
    window.removeEventListener('resize', window.closeGlassMenusOnScroll);
  }
};

window.selectGlassOption = function(inputId, value, label, element) {
  Shiny.setInputValue(inputId, value);
  document.getElementById('label-' + inputId).innerText = label;

  var menu = document.getElementById('menu-' + inputId);
  var options = menu.getElementsByClassName('glass-option');
  for (var i = 0; i < options.length; i++) {
    options[i].classList.remove('selected');
  }
  element.classList.add('selected');

  window.toggleGlassMenu(inputId);
};

window.closeAllGlassMenus = function(exceptId) {
  var allMenus = document.getElementsByClassName('glass-dropdown-menu');
  var allBtns = document.getElementsByClassName('glass-dropdown-btn');

  for (var i = 0; i < allMenus.length; i++) {
    var menuId = allMenus[i].id.replace('menu-', '');
    if (exceptId && menuId === exceptId) continue;
    allMenus[i].classList.remove('show');
  }

  for (var i = 0; i < allBtns.length; i++) {
    var btnId = allBtns[i].id.replace('btn-', '');
    if (exceptId && btnId === exceptId) continue;
    allBtns[i].classList.remove('active');
  }

  if (!exceptId) {
    window.removeEventListener('scroll', window.closeGlassMenusOnScroll, true);
    window.removeEventListener('resize', window.closeGlassMenusOnScroll);
  }
};

// --- CRITICAL FIX: SMART SCROLL HANDLER ---
window.closeGlassMenusOnScroll = function(event) {
  // If the event exists and the target is the menu itself (or inside it), IGNORE.
  if (event && event.target && event.target.classList && event.target.classList.contains('glass-dropdown-menu')) {
    return;
  }
  // Otherwise (page scroll), close menus
  window.closeAllGlassMenus(null);
};

window.addEventListener('click', function(event) {
  // Close if clicking outside button AND outside menu (including scrollbar area)
  if (!event.target.matches('.glass-dropdown-btn') &&
      !event.target.closest('.glass-dropdown-btn') &&
      !event.target.closest('.glass-dropdown-menu')) {
    window.closeAllGlassMenus(null);
  }
});

// 4. Update Handler (Retry Logic)
Shiny.addCustomMessageHandler("glass-dropdown-update", function(message) {
  var id = message.id;
  var attempt = message.attempt || 1;

  var btn = document.getElementById('btn-' + id);
  if (!btn) {
    if (attempt < 20) {
      setTimeout(function() {
        message.attempt = attempt + 1;
        Shiny.handlers["glass-dropdown-update"](message);
      }, 50);
      return;
    } else {
      console.warn("Glass Dropdown Update Failed: Could not find ID " + id);
      return;
    }
  }

  var labelSpan = document.getElementById('label-' + id);
  var menu = document.getElementById('menu-' + id);
  var container = document.getElementById('container-' + id);

  if (message.hasOwnProperty('disabled')) {
    if (message.disabled) {
      btn.classList.add('disabled');
      container.classList.add('disabled');
      menu.classList.remove('show');
    } else {
      btn.classList.remove('disabled');
      container.classList.remove('disabled');
    }
  }

  // Handle color update
  if (message.hasOwnProperty('color')) {
    // Remove existing color classes
    container.classList.remove('green', 'purple');
    // Add new color class
    if (message.color === 'green' || message.color === 'purple') {
      container.classList.add(message.color);
    }
  }

  if (message.values && message.labels) {
    menu.innerHTML = "";
    for (var i = 0; i < message.values.length; i++) {
      var val = message.values[i];
      var lbl = message.labels[i];
      var div = document.createElement('div');
      div.className = "glass-option";
      div.setAttribute('data-value', val);
      (function(v, l, el) {
        el.onclick = function() { window.selectGlassOption(id, v, l, el); };
      })(val, lbl, div);
      div.innerText = lbl;
      menu.appendChild(div);
    }
  }

  var newSelectedVal = null;
  if (message.hasOwnProperty('selected')) {
    newSelectedVal = message.selected;
  } else if (message.values) {
    newSelectedVal = message.values[0];
  }

  if (newSelectedVal !== null) {
    var options = menu.getElementsByClassName('glass-option');
    var foundLabel = null;
    for (var i = 0; i < options.length; i++) {
      options[i].classList.remove('selected');
      if (options[i].getAttribute('data-value') === newSelectedVal) {
        options[i].classList.add('selected');
        foundLabel = options[i].innerText;
      }
    }
    if (foundLabel) {
      labelSpan.innerText = foundLabel;
      Shiny.setInputValue(id, newSelectedVal);
    }
  }
});
