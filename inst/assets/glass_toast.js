// === Glass Toast Notification System ===
// A custom toast notification system independent of Shiny's showNotification

(function() {
  'use strict';

  // Store for active toasts
  var activeToasts = {};
  var toastCounter = 0;

  // Icons for each type
  var typeIcons = {
    message: '<i class="fa fa-info-circle"></i>',
    success: '<i class="fa fa-check-circle"></i>',
    warning: '<i class="fa fa-exclamation-triangle"></i>',
    error: '<i class="fa fa-times-circle"></i>'
  };

  // Ensure container exists
  function ensureContainer() {
    var container = document.getElementById('glass-toast-container');
    if (!container) {
      container = document.createElement('div');
      container.id = 'glass-toast-container';
      container.className = 'glass-toast-container';
      document.body.appendChild(container);
    }
    return container;
  }

  // Create a toast element
  function createToastElement(options) {
    var toast = document.createElement('div');
    toast.className = 'glass-toast type-' + (options.type || 'message');
    toast.setAttribute('data-toast-id', options.id);

    // Build HTML
    var html = '';

    // Icon
    html += '<div class="glass-toast-icon">';
    html += options.icon || typeIcons[options.type] || typeIcons.message;
    html += '</div>';

    // Content
    html += '<div class="glass-toast-content">';
    if (options.title) {
      html += '<div class="glass-toast-title">' + escapeHtml(options.title) + '</div>';
    }
    html += '<div class="glass-toast-message">' + options.message + '</div>';
    
    // Action button (optional) - must be an object with a label property
    if (options.actionButton && options.actionButton.label) {
      html += '<button class="glass-toast-action" data-action="true">' + 
              escapeHtml(options.actionButton.label) + '</button>';
    }
    html += '</div>';

    // Close button
    if (options.closeButton !== false) {
      html += '<button class="glass-toast-close" aria-label="Close">&times;</button>';
    }

    // Progress bar
    if (options.duration && options.duration > 0) {
      html += '<div class="glass-toast-progress"></div>';
    }

    toast.innerHTML = html;

    // Attach event listeners
    var closeBtn = toast.querySelector('.glass-toast-close');
    if (closeBtn) {
      closeBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        removeToast(options.id);
      });
    }

    var actionBtn = toast.querySelector('.glass-toast-action');
    if (actionBtn && options.actionButton && options.actionButton.callback) {
      actionBtn.addEventListener('click', function(e) {
        e.stopPropagation();
        if (options.actionButton.inputId) {
          Shiny.setInputValue(options.actionButton.inputId, {
            toastId: options.id,
            timestamp: Date.now()
          }, {priority: 'event'});
        }
        if (typeof options.actionButton.callback === 'function') {
          options.actionButton.callback();
        }
        if (options.actionButton.closeOnClick !== false) {
          removeToast(options.id);
        }
      });
    }

    return toast;
  }

  // Simple HTML escape
  function escapeHtml(text) {
    if (typeof text !== 'string') return text;
    var div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  // Show a toast
  function showToast(options) {
    var container = ensureContainer();

    // Generate ID if not provided
    if (!options.id) {
      options.id = 'glass-toast-' + (++toastCounter);
    }

    // Remove existing toast with same ID
    if (activeToasts[options.id]) {
      removeToast(options.id, true);
    }

    // Create element
    var toast = createToastElement(options);
    container.appendChild(toast);

    // Store reference
    activeToasts[options.id] = {
      element: toast,
      options: options,
      timerId: null
    };

    // Trigger animation
    requestAnimationFrame(function() {
      requestAnimationFrame(function() {
        toast.classList.add('visible');
      });
    });

    // Set up auto-dismiss
    if (options.duration && options.duration > 0) {
      var progress = toast.querySelector('.glass-toast-progress');
      if (progress) {
        progress.style.animationDuration = options.duration + 'ms';
        progress.classList.add('animating');
      }

      // Handle hover pause
      var remainingTime = options.duration;
      var startTime = Date.now();
      var timerId;

      function startTimer() {
        startTime = Date.now();
        timerId = setTimeout(function() {
          removeToast(options.id);
        }, remainingTime);
        activeToasts[options.id].timerId = timerId;
      }

      function pauseTimer() {
        if (timerId) {
          clearTimeout(timerId);
          remainingTime -= (Date.now() - startTime);
          if (remainingTime < 0) remainingTime = 0;
        }
      }

      toast.addEventListener('mouseenter', pauseTimer);
      toast.addEventListener('mouseleave', startTimer);

      startTimer();
    }

    return options.id;
  }

  // Remove a toast
  function removeToast(id, immediate) {
    var toastData = activeToasts[id];
    if (!toastData) return;

    var toast = toastData.element;

    // Clear timer
    if (toastData.timerId) {
      clearTimeout(toastData.timerId);
    }

    if (immediate) {
      toast.remove();
      delete activeToasts[id];
      return;
    }

    // Animate out
    toast.classList.remove('visible');
    toast.classList.add('removing');

    setTimeout(function() {
      toast.remove();
      delete activeToasts[id];
    }, 400);
  }

  // Remove all toasts
  function removeAllToasts() {
    Object.keys(activeToasts).forEach(function(id) {
      removeToast(id);
    });
  }

  // Expose to global scope
  window.GlassToast = {
    show: showToast,
    remove: removeToast,
    removeAll: removeAllToasts
  };

  // Shiny message handler
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('glassToast', function(message) {
      if (message.action === 'show') {
        showToast(message);
      } else if (message.action === 'remove') {
        removeToast(message.id);
      } else if (message.action === 'removeAll') {
        removeAllToasts();
      }
    });
  }

})();
