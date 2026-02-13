// ==========================================================================
// GLASS BADGE JS - v1.0
// ==========================================================================

// Delegated events work on future DOM — no need to wait for shiny:connected.
// Using jQuery namespacing to ensure only one set of handlers exists.
(function() {
  $('body')
    .off('mouseenter.glassBadge', '.gb-tooltip-trigger-hover')
    .on('mouseenter.glassBadge', '.gb-tooltip-trigger-hover', function() {
      showTooltip($(this));
    });

  $('body')
    .off('mouseleave.glassBadge', '.gb-tooltip-trigger-hover')
    .on('mouseleave.glassBadge', '.gb-tooltip-trigger-hover', function() {
      hideTooltip($(this));
    });

  $('body')
    .off('click.glassBadge', '.gb-tooltip-trigger-click')
    .on('click.glassBadge', '.gb-tooltip-trigger-click', function(e) {
      e.stopPropagation(); // Prevent bubbling
      var badge = $(this);

      if (badge.hasClass('gb-tooltip-active')) {
        hideTooltip(badge);
      } else {
        // Close others first
        $('.gb-tooltip-active').each(function() { hideTooltip($(this)); });
        showTooltip(badge);
      }
    });

  // Close tooltips when clicking anywhere else
  $(document)
    .off('click.glassBadge')
    .on('click.glassBadge', function() {
      $('.gb-tooltip-active').each(function() { hideTooltip($(this)); });
    });
})();

function showTooltip(badge) {
  var text = badge.attr('data-tooltip');
  if (!text) return;

  // Check if tooltip element already exists inside badge
  if (badge.find('.gb-tooltip').length === 0) {
    var tip = $('<span class="gb-tooltip"></span>').text(text);
    badge.append(tip);
  }

  // Small delay for animation smoothness
  setTimeout(function() {
    badge.addClass('gb-tooltip-active');
  }, 10);
}

function hideTooltip(badge) {
  badge.removeClass('gb-tooltip-active');
  // Optional: Remove from DOM after transition to keep it clean
  setTimeout(function() {
    if(!badge.hasClass('gb-tooltip-active')) {
       badge.find('.gb-tooltip').remove();
    }
  }, 300);
}
