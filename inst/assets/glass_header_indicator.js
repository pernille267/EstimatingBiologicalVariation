var glassHeaderIndicatorBinding = new Shiny.InputBinding();

$.extend(glassHeaderIndicatorBinding, {
  find: function(scope) {
    return $(scope).find('.glass-header-indicator');
  },

  initialize: function(el) {
    // No specific init
  },

  getValue: function(el) {
    return !$(el).hasClass('hidden');
  },

  receiveMessage: function(el, data) {
    var $el = $(el);

    // 1. Tooltip
    if (data.hasOwnProperty('tooltip_text')) {
      $el.find('.glass-indicator-tooltip').html(data.tooltip_text);
    }

    // 2. Visibility
    if (data.hasOwnProperty('visible')) {
      if (data.visible) {
        $el.removeClass('hidden');
      } else {
        $el.addClass('hidden');
      }
    }

    // 3. Status Coloring (Swapping gradients)
    if (data.hasOwnProperty('status')) {
      // Fjern gamle status-klasser
      $el.removeClass('success warning error');

      // Legg til ny (som trigger riktig gradient i CSS)
      if (data.status) {
        $el.addClass(data.status);
      }
    }

    // 4. Icon Update
    if (data.hasOwnProperty('icon_name')) {
      var $icon = $el.find('i').first();
      if ($icon.length > 0) {
        // Nullstill icon klasser
        var classes = $icon.attr('class').split(/\s+/);
        $.each(classes, function(index, item) {
             if (item.startsWith('fa-')) {
                 $icon.removeClass(item);
             }
        });
        $icon.addClass('fa-' + data.icon_name);
      }
    }
  }
});

Shiny.inputBindings.register(glassHeaderIndicatorBinding);
