// --- Glass Notification JavaScript ---

var glassNotificationBinding = new Shiny.InputBinding();

$.extend(glassNotificationBinding, {

  find: function(scope) {
    return $(scope).find('.glass-notification');
  },

  initialize: function(el) {
    var $el = $(el);
    var inputId = $el.attr('id'); // Get the ID to send signals

    // Handle Close Button Click
    $el.find('.glass-notification-close').on('click', function(e) {
      e.stopPropagation();

      // 1. Visually hide it immediately (UX responsiveness)
      $el.addClass('hidden');

      // 2. Tell Server it was dismissed manually
      // We append "_dismissed" to the inputID so we can listen to input$id_dismissed
      Shiny.setInputValue(inputId + "_dismissed", true, {priority: "event"});

      // 3. Trigger change for standard binding
      $el.trigger('change');
    });
  },

  getValue: function(el) {
    return !$(el).hasClass('hidden');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);
    var $header = $el.find('.glass-notification-title-group');
    var $body = $el.find('.glass-notification-body');
    var $timerBar = $el.find('.glass-notification-timer-bar');

    // Reset hidden state if value or type is updated (Re-appear)
    // Only if we are not explicitly telling it to stay hidden?
    // Usually, if server sends update, we want to show it.
    if (data.hasOwnProperty('value') || data.hasOwnProperty('label')) {
       $el.removeClass('hidden');
    }

    if (data.hasOwnProperty('label')) {
      $el.find('.glass-notification-label-text').html(data.label);
    }

    if (data.hasOwnProperty('label_icon')) {
      $el.find('.glass-notification-icon-span').html(data.label_icon);
    }

    if (data.hasOwnProperty('value')) {
      $body.html(data.value);

      if (data.value === "") {
        // If value is empty, HIDE the notification (avoid showing empty box)
        $el.addClass('hidden');
      }
      else {
        // if value has content, SHOW the box
        $el.removeClass('hidden');
      }

    }

    if (data.hasOwnProperty('message_type')) {
      $el.removeClass('info warning error');
      $el.addClass(data.message_type);
    }

    // --- Manual visibility control from server ---
    if (data.hasOwnProperty('visible')) {
      if (data.visible === false) {
        $el.addClass('hidden');
      } else {
        $el.removeClass('hidden');
      }
    }

    // Timer Logic (Same as before)
    if ($el.data('timerId')) {
      clearTimeout($el.data('timerId'));
      $el.data('timerId', null);
    }
    $timerBar.css('width', '0%').css('transition', 'none');

    if (data.hasOwnProperty('timer') && data.timer > 0) {
      void $el.get(0).offsetWidth;
      $timerBar.css('transition', 'width ' + data.timer + 'ms linear');
      $timerBar.css('width', '100%');
      var timerId = setTimeout(function() {
        $el.addClass('hidden');
        Shiny.setInputValue($el.attr('id') + "_dismissed", true, {priority: "event"});
        $el.trigger('change');
      }, data.timer);
      $el.data('timerId', timerId);
    }
  },

  subscribe: function(el, callback) {
    $(el).on('change.glassNotificationBinding', function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassNotificationBinding');
  }
});

Shiny.inputBindings.register(glassNotificationBinding);
