// ==========================================================================
// GLASS INPUT BINDING (Text & Numeric)
// ==========================================================================

var glassInputBinding = new Shiny.InputBinding();

$.extend(glassInputBinding, {

  find: function(scope) {
    return $(scope).find('.glass-input-binding');
  },

  initialize: function(el) {
    var $el = $(el);
    var $input = $el.find('input.glass-native-input');
    var $wrapper = $el.find('.glass-input-wrapper');
    var id = $el.attr('id');

    // --- State: Focus ---
    $input.on('focus', function() {
      $wrapper.addClass('focused');
    });

    $input.on('blur', function() {
      $wrapper.removeClass('focused');
    });

    // --- State: Input Change & Validation ---
    $input.on('input change', function() {
      var val = $input.val();

      // 1. Update Tooltip Text (Empty vs Filled)
      var tipEmpty = $wrapper.attr('data-tip-empty');
      var tipFilled = $wrapper.attr('data-tip-filled');
      if (val === "") {
        $wrapper.attr('data-tooltip', tipEmpty);
      } else {
        $wrapper.attr('data-tooltip', tipFilled);
      }

      // 2. Numeric Validation (Only if it's a numeric input)
      if ($el.hasClass('glass-numeric')) {
        validateNumeric($el, val);
      }
    });

    // Trigger initial validation/tooltip setup
    $input.trigger('input');
  },

  getValue: function(el) {
    var $input = $(el).find('input.glass-native-input');
    var val = $input.val();

    if ($(el).hasClass('glass-numeric')) {
      // Return null for empty string to mimic Shiny behavior
      if (val === "") return null;
      var num = parseFloat(val);
      return isNaN(num) ? null : num;
    }
    return val;
  },

  setValue: function(el, value) {
    var $input = $(el).find('input.glass-native-input');
    $input.val(value);
    // Trigger input to run validation logic
    $input.trigger('input');
  },

  subscribe: function(el, callback) {
    // Debounce slightly for text inputs to avoid spamming R
    var $input = $(el).find('input.glass-native-input');
    var delay = $(el).hasClass('glass-numeric') ? 800 : 250;

    var timer = null;
    $input.on('input.glassInputBinding', function(event) {
      if (timer) clearTimeout(timer);
      timer = setTimeout(function() {
        callback();
      }, delay);
    });

    // Immediate update on change (enter key / blur)
    $input.on('change.glassInputBinding', function(event) {
        if (timer) clearTimeout(timer);
        callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassInputBinding');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);
    var $input = $el.find('input.glass-native-input');
    var $wrapper = $el.find('.glass-input-wrapper');
    var $label = $el.find('.glass-input-label-text');
    var $icon = $el.find('.glass-input-label-icon');

    // Value
    if (data.hasOwnProperty('value')) {
      this.setValue(el, data.value);
    }

    // Label
    if (data.hasOwnProperty('label')) {
      $label.text(data.label);
    }

    // Icon (HTML)
    if (data.hasOwnProperty('label_icon')) {
      $icon.html(data.label_icon);
    }

    // Disabled state
    if (data.hasOwnProperty('disabled')) {
      if (data.disabled) {
        $el.addClass('disabled');
        $input.prop('disabled', true);
      } else {
        $el.removeClass('disabled');
        $input.prop('disabled', false);
      }
    }

    // Color update
    if (data.hasOwnProperty('color')) {
      // Remove existing color classes
      $wrapper.removeClass('green purple');
      // Add new color class
      if (data.color === 'green' || data.color === 'purple') {
        $wrapper.addClass(data.color);
      }
    }

    // Validation Range Update (Numeric Only)
    if (data.hasOwnProperty('accept')) {
        $el.attr('data-min', data.accept[0]);
        $el.attr('data-max', data.accept[1]);
        // Re-validate current value
        validateNumeric($el, $input.val());
    }
  }
});

Shiny.inputBindings.register(glassInputBinding, 'glass.inputBinding');


// --- Helper: Client-Side Numeric Validation ---
function validateNumeric($el, value) {
  var $wrapper = $el.find('.glass-input-wrapper');
  var $warningMsg = $el.find('.glass-input-warning-msg');

  // If empty, usually we don't show range error unless required,
  // but let's clear warning for empty.
  if (value === "" || value === null) {
    $wrapper.removeClass('warning');
    $warningMsg.removeClass('show');
    return;
  }

  var numVal = parseFloat(value);
  var min = parseFloat($el.attr('data-min'));
  var max = parseFloat($el.attr('data-max'));

  // Logic: Check bounds if they exist
  var invalid = false;
  if (!isNaN(min) && numVal < min) invalid = true;
  if (!isNaN(max) && numVal > max) invalid = true;

  if (invalid) {
    $wrapper.addClass('warning');
    $warningMsg.addClass('show');
  } else {
    $wrapper.removeClass('warning');
    $warningMsg.removeClass('show');
  }
}
