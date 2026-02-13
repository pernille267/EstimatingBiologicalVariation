// 1. Initialize on load
$(document).on('shiny:connected', function() {
  initGlassRadios();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassRadios();
});

function initGlassRadios() {
  var groups = document.querySelectorAll('.glass-radio-group:not(.initialized)');
  groups.forEach(function(group) {
    var id = group.id.replace('group-', '');
    var initialValue = group.dataset.selected;

    // Register initial value with Shiny
    if (initialValue) {
      Shiny.setInputValue(id, initialValue);
    }
    group.classList.add('initialized');
  });
}

// 2. Click Handler (Attached globally for simplicity)
window.selectGlassRadio = function(groupId, value, element) {
  var group = document.getElementById('group-' + groupId);
  if (!group || group.classList.contains('disabled')) return;

  // A. Update Visuals
  var btns = group.querySelectorAll('.glass-radio-btn');
  btns.forEach(function(btn) {
    btn.classList.remove('selected');
  });
  element.classList.add('selected');

  // B. Send to Shiny
  Shiny.setInputValue(groupId, value);
};

// 3. Update Handler (Communication from R)
Shiny.addCustomMessageHandler("glass-radio-update", function(message) {
  var id = message.id;
  var group = document.getElementById('group-' + id);

  if (!group) {
    console.warn("Glass Radio Update Failed: Could not find group " + id);
    return;
  }

  // A. Handle Color Variant
  if (message.hasOwnProperty('color')) {
    // Remove existing color classes
    group.classList.remove('purple', 'green');
    // Add new color class (only if purple, green is default)
    if (message.color === 'purple') {
      group.classList.add('purple');
    }
  }

  // B. Handle Disabled
  if (message.hasOwnProperty('disabled')) {
    if (message.disabled) {
      group.classList.add('disabled');
    } else {
      group.classList.remove('disabled');
    }
  }

  // B. Handle Choices Update
  if (message.values && message.labels) {
    group.innerHTML = ""; // Clear existing

    for (var i = 0; i < message.values.length; i++) {
      var val = message.values[i];
      var lbl = message.labels[i];

      var btn = document.createElement('div');
      btn.className = "glass-radio-btn";
      if (val === message.selected) btn.classList.add('selected');

      // Closure for click handler
      (function(v, el) {
        el.onclick = function() { window.selectGlassRadio(id, v, el); };
      })(val, btn);

      btn.innerText = lbl;
      group.appendChild(btn);
    }
  }
  // C. Handle Selection Update (if choices didn't change but selection did)
  else if (message.hasOwnProperty('selected')) {
    var btns = group.querySelectorAll('.glass-radio-btn');
    btns.forEach(function(btn) {
      btn.classList.remove('selected');
      // Note: We need a way to check value. In generating HTML below,
      // we didn't add data-value explicitly to DOM, let's fix that in logic above.
      // But simpler: just re-render is often safest, but for selection only:
    });

    // Actually, let's ensure we attach data-value to the buttons so we can find them
    // I will update the R function to add data-value attribute.
    var targetBtn = group.querySelector('.glass-radio-btn[data-value="' + message.selected + '"]');
    if (targetBtn) {
      targetBtn.classList.add('selected');
      Shiny.setInputValue(id, message.selected);
    }
  }
});
