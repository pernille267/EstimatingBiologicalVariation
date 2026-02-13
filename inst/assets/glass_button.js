var glassButtonBinding = new Shiny.InputBinding();

$.extend(glassButtonBinding, {

  find: function(scope) {
    // Only find regular glass buttons, not download buttons
    // Download buttons have their own handler (glassDownloadHandler)
    return $(scope).find('.glass-btn').not('.glass-download-btn');
  },

  initialize: function(el) {
    $(el).data('val', 0);
  },

  getValue: function(el) {
    return $(el).data('val');
  },

  subscribe: function(el, callback) {
    $(el).on('click.glassButtonBinding', function(e) {
      if ($(el).hasClass('disabled')) return;
      var val = $(el).data('val') || 0;
      $(el).data('val', val + 1);
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassButtonBinding');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);

    if (data.hasOwnProperty('label')) {
      $el.find('.glass-btn-label').text(data.label);
    }

    if (data.hasOwnProperty('icon')) {
      $el.find('.glass-btn-icon').html(data.icon);
    }

    if (data.hasOwnProperty('disabled')) {
      if (data.disabled) {
        $el.addClass('disabled');
      } else {
        $el.removeClass('disabled');
      }
    }

    if (data.hasOwnProperty('color')) {
      $el.removeClass('green white');
      if (data.color === 'green') {
        $el.addClass('green');
      } else if (data.color === 'white') {
        $el.addClass('white');
      }
    }

    // Urgent indicator is a sibling within the wrapper, immediately preceding the button
    if (data.hasOwnProperty('urgent')) {
      var $indicator = $el.parent().find('.urgent-indicator');
      if (data.urgent) {
        $indicator.addClass('visible');
      } else {
        $indicator.removeClass('visible');
      }
    }

    if (data.hasOwnProperty('urgent_text')) {
       var $indicatorText = $el.parent().find('.urgent-text');
       $indicatorText.text(data.urgent_text);
    }
  }
});

Shiny.inputBindings.register(glassButtonBinding, 'shiny.glassButton');


// =========================================================================
// GLASS DOWNLOAD HANDLER
// Customizable handler for download buttons that may or may not use
// Shiny's built-in downloadHandler logic (shiny-download-link class).
// =========================================================================

var glassDownloadHandler = new Shiny.InputBinding();

$.extend(glassDownloadHandler, {

  find: function(scope) {
    return $(scope).find('.glass-download-btn');
  },

  initialize: function(el) {
    $(el).data('val', 0);
  },

  getValue: function(el) {
    return $(el).data('val');
  },

  subscribe: function(el, callback) {
    $(el).on('click.glassDownloadHandler', function(e) {
      if ($(el).hasClass('disabled')) {
        e.preventDefault();
        e.stopPropagation();
        return;
      }
      var val = $(el).data('val') || 0;
      $(el).data('val', val + 1);
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassDownloadHandler');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);

    if (data.hasOwnProperty('label')) {
      $el.find('.glass-btn-label').text(data.label);
    }

    if (data.hasOwnProperty('icon')) {
      $el.find('.glass-btn-icon').html(data.icon);
    }

    if (data.hasOwnProperty('disabled')) {
      if (data.disabled) {
        $el.addClass('disabled');
        $el.attr('tabindex', '-1');
        $el.attr('aria-disabled', 'true');
      } else {
        $el.removeClass('disabled');
        $el.removeAttr('tabindex');
        $el.removeAttr('aria-disabled');
      }
    }

    if (data.hasOwnProperty('color')) {
      $el.removeClass('green white');
      if (data.color === 'green') {
        $el.addClass('green');
      } else if (data.color === 'white') {
        $el.addClass('white');
      }
    }

    if (data.hasOwnProperty('style')) {
      $el.attr('style', data.style);
    }

    if (data.hasOwnProperty('downloadUrl')) {
      var url = data.downloadUrl;
      var file = data.filename || '';

      var link = document.createElement('a');
      link.href = url;
      link.download = file;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    }

    $el.trigger('change');
  }
});

Shiny.inputBindings.register(glassDownloadHandler, 'shiny.glassDownloadHandler');
