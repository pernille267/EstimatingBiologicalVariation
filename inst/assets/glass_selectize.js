// === Glass Selectize Input - Premium Multi-Select Component v14.0 ===
// Performance-optimised: batched DOM writes, event delegation,
// deferred dropdown rendering, debounced server updates.

(function() {
  'use strict';

  // ── Stores ──────────────────────────────────────────────
  var selectizeInstances = {};
  var pendingUpdates = {};          // queued msgs before init
  var debouncedUpdateTimers = {};   // per-instance debounce ids

  // ── Lookup helper (O(1) instead of Array.find) ─────────
  function buildOptionMap(options) {
    var map = {};
    for (var i = 0; i < options.length; i++) {
      map[options[i].value] = options[i].label;
    }
    return map;
  }

  // ── Initialisation hooks (single combined handler) ─────
  function initAllGlassSelectize() {
    var containers = document.querySelectorAll(
      '.glass-selectize-container:not(.initialized)'
    );
    for (var i = 0; i < containers.length; i++) {
      initGlassSelectize(containers[i]);
    }
  }

  $(document).on('shiny:connected shiny:sessioninitialized', initAllGlassSelectize);
  $(document).ready(function() { setTimeout(initAllGlassSelectize, 100); });

  // MutationObserver for dynamically inserted widgets
  $(document).ready(function() {
    var observer = new MutationObserver(function(mutations) {
      var needsScan = false;
      for (var m = 0; m < mutations.length; m++) {
        if (mutations[m].addedNodes.length) { needsScan = true; break; }
      }
      if (needsScan) initAllGlassSelectize();
    });
    observer.observe(document.body, { childList: true, subtree: true });
  });

  // ── Initialise a single selectize instance ─────────────
  function initGlassSelectize(container) {
    try {
      if (container.classList.contains('initialized')) return;
      container.classList.add('initialized');

      var id = container.id;
      var options = [];
      var selected = [];
      try { options  = JSON.parse(container.getAttribute('data-options')  || '[]'); } catch (e) { /* keep [] */ }
      try { selected = JSON.parse(container.getAttribute('data-selected') || '[]'); } catch (e) { /* keep [] */ }

      var multiple   = container.getAttribute('data-multiple') === 'true';
      var placeholder = container.getAttribute('data-placeholder') || 'Select...';
      var additionalPlaceholder = container.getAttribute('data-additional-placeholder') || 'Select another...';
      var maxItems   = container.getAttribute('data-max-items');

      // Build DOM
      var itemsDiv = document.createElement('div');
      itemsDiv.className = 'glass-selectize-items';

      var inputWrapper = document.createElement('div');
      inputWrapper.className = 'glass-selectize-input-wrapper';

      var input = document.createElement('input');
      input.className = 'glass-selectize-input';
      input.type = 'text';
      input.placeholder = placeholder;
      input.autocomplete = 'off';
      // Fix: give input an id and name so the browser doesn't
      // fire "form field missing id/name" accessibility warnings
      input.id   = id + '-input';
      input.name = id + '-input';

      var arrow = document.createElement('div');
      arrow.className = 'glass-selectize-arrow';
      arrow.innerHTML = '<i class="fa fa-chevron-down"></i>';

      var dropdown = document.createElement('div');
      dropdown.className = 'glass-selectize-dropdown';

      inputWrapper.appendChild(input);
      inputWrapper.appendChild(arrow);
      container.appendChild(itemsDiv);
      container.appendChild(inputWrapper);
      container.appendChild(dropdown);

      var instance = {
        container:  container,
        input:      input,
        dropdown:   dropdown,
        itemsDiv:   itemsDiv,
        options:    options,
        optionMap:  buildOptionMap(options),
        selected:   multiple ? selected : (selected.length ? [selected[0]] : []),
        multiple:   multiple,
        placeholder: placeholder,
        additionalPlaceholder: additionalPlaceholder,
        maxItems:   maxItems ? parseInt(maxItems, 10) : null,
        highlightedIndex: -1,
        searchTerm: '',
        dropdownDirty: true   // flag: options need rebuild
      };

      selectizeInstances[id] = instance;

      // Replay any queued server updates
      if (pendingUpdates[id]) {
        var queued = pendingUpdates[id];
        delete pendingUpdates[id];
        // Only the LAST queued message matters (later messages supersede earlier)
        var last = queued[queued.length - 1];
        mergeUpdateMessage(instance, last);
      }

      renderSelected(instance);
      // Don't render dropdown yet — it's invisible anyway.
      // It will be built lazily on first open.

      // Resize observer (debounced via rAF)
      var resizeRaf = 0;
      var resizeObserver = new ResizeObserver(function() {
        if (instance.selected.length > 0) {
          cancelAnimationFrame(resizeRaf);
          resizeRaf = requestAnimationFrame(function() {
            checkPlaceholderSpace(instance);
          });
        }
      });
      resizeObserver.observe(container);
      instance.resizeObserver = resizeObserver;

      // ── Event listeners (on the container — delegation) ──
      input.addEventListener('focus', function() {
        container.classList.add('focused');
        openDropdown(instance);
      });

      input.addEventListener('blur', function() {
        setTimeout(function() {
          if (!container.contains(document.activeElement)) {
            container.classList.remove('focused');
            closeDropdown(instance);
          }
        }, 200);
      });

      input.addEventListener('input', function(e) {
        instance.searchTerm = e.target.value;
        instance.dropdownDirty = true;
        renderOptions(instance);
        openDropdown(instance);
      });

      input.addEventListener('keydown', function(e) {
        handleKeyboard(e, instance);
      });

      arrow.addEventListener('click', function(e) {
        e.stopPropagation();
        if (dropdown.classList.contains('active')) {
          closeDropdown(instance);
          input.blur();
        } else {
          input.focus();
        }
      });

      // Delegated click for chips remove buttons
      itemsDiv.addEventListener('click', function(e) {
        var removeBtn = e.target.closest('.glass-selectize-chip-remove');
        if (removeBtn) {
          e.stopPropagation();
          var chip = removeBtn.closest('.glass-selectize-chip');
          if (chip) removeItem(instance, chip.getAttribute('data-value'));
          return;
        }
        // Click on items area focuses input
        e.preventDefault();
        input.focus();
      });

      // Delegated click/hover for dropdown options
      dropdown.addEventListener('mousedown', function(e) {
        var optEl = e.target.closest('.glass-selectize-option');
        if (optEl) {
          e.preventDefault();
          selectOption(instance, optEl.getAttribute('data-value'));
        }
      });
      dropdown.addEventListener('mouseover', function(e) {
        var optEl = e.target.closest('.glass-selectize-option');
        if (optEl) {
          var idx = parseInt(optEl.getAttribute('data-index'), 10);
          if (idx !== instance.highlightedIndex) {
            instance.highlightedIndex = idx;
            updateHighlight(instance);
          }
        }
      });

      container.addEventListener('click', function(e) {
        if (e.target === container ||
            e.target === inputWrapper ||
            e.target.classList.contains('glass-selectize-input-wrapper')) {
          e.preventDefault();
          input.focus();
        }
      });

      updateShinyValue(instance);

    } catch (err) {
      console.error('FATAL ERROR initializing glass selectize:', err);
      container.style.border = '2px solid red';
      container.textContent = 'Error: ' + err.message;
    }
  }

  // ── Render selected chips ──────────────────────────────
  function renderSelected(instance) {
    var frag = document.createDocumentFragment();

    // Remove any existing overflow tooltip
    var existingTooltip = instance.container.querySelector('.glass-selectize-overflow-tooltip');
    if (existingTooltip) existingTooltip.remove();
    instance.container.classList.remove('overflow-collapsed');

    for (var i = 0; i < instance.selected.length; i++) {
      var value = instance.selected[i];
      var label = instance.optionMap[value] || value;

      var chip = document.createElement('div');
      chip.className = 'glass-selectize-chip';
      chip.setAttribute('data-value', value);
      chip.innerHTML = label + '<div class="glass-selectize-chip-remove"><i class="fa fa-times"></i></div>';
      frag.appendChild(chip);
    }

    instance.itemsDiv.innerHTML = '';
    instance.itemsDiv.appendChild(frag);

    // Update placeholder
    if (instance.selected.length > 0) {
      instance.input.placeholder = instance.additionalPlaceholder;
      instance.input.classList.add('has-selections');
      // Defer layout measurement to a single rAF
      requestAnimationFrame(function() {
        checkPlaceholderSpace(instance);
        checkOverflow(instance);
      });
    } else {
      instance.input.placeholder = instance.placeholder;
      instance.input.classList.remove('has-selections');
      instance.input.classList.remove('hide-placeholder');
    }

    instance.input.value = '';
    instance.dropdownDirty = true;
  }

  // ── Overflow collapse ──────────────────────────────────
  function checkOverflow(instance) {
    if (instance.selected.length <= 1) return;

    var container  = instance.container;
    var itemsDiv   = instance.itemsDiv;
    var containerWidth = container.offsetWidth;
    var arrowWidth = 36;
    var inputMinWidth = 40;
    var padding = 30;
    var availableWidth = containerWidth - arrowWidth - inputMinWidth - padding;

    var chips = itemsDiv.querySelectorAll('.glass-selectize-chip');
    var totalChipWidth = 0;
    for (var c = 0; c < chips.length; c++) {
      totalChipWidth += chips[c].offsetWidth + 6;
    }

    if (totalChipWidth <= availableWidth) return;   // fits — nothing to do

    // Collapse: keep chips that fit, badge the rest
    container.classList.add('overflow-collapsed');
    var frag = document.createDocumentFragment();
    var usedWidth = 0;
    var badgeWidth = 80;
    var shownCount = 0;

    for (var i = 0; i < instance.selected.length; i++) {
      var value = instance.selected[i];
      var label = instance.optionMap[value] || value;

      var chip = document.createElement('div');
      chip.className = 'glass-selectize-chip';
      chip.setAttribute('data-value', value);
      chip.innerHTML = label + '<div class="glass-selectize-chip-remove"><i class="fa fa-times"></i></div>';
      frag.appendChild(chip);

      // Temporarily add to DOM to measure (batch later)
      itemsDiv.innerHTML = '';
      itemsDiv.appendChild(frag);
      usedWidth = 0;
      var tempChips = itemsDiv.querySelectorAll('.glass-selectize-chip');
      for (var t = 0; t < tempChips.length; t++) usedWidth += tempChips[t].offsetWidth + 6;

      shownCount = i + 1;
      if (usedWidth + badgeWidth > availableWidth && i < instance.selected.length - 1) break;
    }

    // Rebuild fragment with just the chips that fit
    itemsDiv.innerHTML = '';
    var finalFrag = document.createDocumentFragment();
    for (var j = 0; j < shownCount; j++) {
      var v = instance.selected[j];
      var l = instance.optionMap[v] || v;
      var ch = document.createElement('div');
      ch.className = 'glass-selectize-chip';
      ch.setAttribute('data-value', v);
      ch.innerHTML = l + '<div class="glass-selectize-chip-remove"><i class="fa fa-times"></i></div>';
      finalFrag.appendChild(ch);
    }

    var remaining = instance.selected.length - shownCount;
    if (remaining > 0) {
      var badge = document.createElement('div');
      badge.className = 'glass-selectize-overflow-badge';
      badge.textContent = '+' + remaining + ' more';
      finalFrag.appendChild(badge);

      // Tooltip with all selected items
      var tooltipDiv = document.createElement('div');
      tooltipDiv.className = 'glass-selectize-overflow-tooltip';
      for (var k = 0; k < instance.selected.length; k++) {
        var tv = instance.selected[k];
        var tl = instance.optionMap[tv] || tv;
        var tc = document.createElement('div');
        tc.className = 'glass-selectize-chip';
        tc.setAttribute('data-value', tv);
        tc.innerHTML = tl + '<div class="glass-selectize-chip-remove"><i class="fa fa-times"></i></div>';
        tooltipDiv.appendChild(tc);
      }

      // Delegated events on tooltip
      tooltipDiv.addEventListener('click', function(e) {
        var removeBtn = e.target.closest('.glass-selectize-chip-remove');
        if (removeBtn) {
          e.stopPropagation();
          var chipEl = removeBtn.closest('.glass-selectize-chip');
          if (chipEl) removeItem(instance, chipEl.getAttribute('data-value'));
        }
      });

      badge.addEventListener('mouseenter', function() {
        tooltipDiv.classList.add('visible');
      });
      badge.addEventListener('mouseleave', function() {
        setTimeout(function() {
          if (!tooltipDiv.matches(':hover')) tooltipDiv.classList.remove('visible');
        }, 150);
      });
      tooltipDiv.addEventListener('mouseleave', function() {
        tooltipDiv.classList.remove('visible');
      });

      instance.container.appendChild(tooltipDiv);
    }

    itemsDiv.appendChild(finalFrag);
  }

  // ── Placeholder space check ────────────────────────────
  function checkPlaceholderSpace(instance) {
    if (!instance.input.classList.contains('has-selections')) return;
    var containerWidth = instance.container.offsetWidth;
    var itemsWidth    = instance.itemsDiv.offsetWidth;
    var available     = containerWidth - itemsWidth - 56;   // arrow + padding
    if (available < 140) {
      instance.input.classList.add('hide-placeholder');
    } else {
      instance.input.classList.remove('hide-placeholder');
    }
  }

  // ── Render dropdown options (lazy — only when visible) ─
  function renderOptions(instance) {
    // Skip expensive DOM work if dropdown is closed and not dirty
    var isOpen = instance.dropdown.classList.contains('active');
    if (!isOpen && !instance.dropdownDirty) return;

    instance.dropdown.innerHTML = '';
    instance.highlightedIndex = -1;

    var searchTerm = (instance.searchTerm || '').toLowerCase();
    var filteredOptions = [];
    for (var i = 0; i < instance.options.length; i++) {
      var opt = instance.options[i];
      if (searchTerm && opt.label.toLowerCase().indexOf(searchTerm) === -1) continue;
      if (instance.multiple && instance.selected.indexOf(opt.value) !== -1) continue;
      filteredOptions.push(opt);
    }

    if (filteredOptions.length === 0) {
      var empty = document.createElement('div');
      empty.className = 'glass-selectize-empty';
      empty.textContent = searchTerm ? 'No matches found' : 'No options available';
      instance.dropdown.appendChild(empty);
      instance.dropdownDirty = false;
      return;
    }

    var frag = document.createDocumentFragment();
    for (var j = 0; j < filteredOptions.length; j++) {
      var option = filteredOptions[j];
      var el = document.createElement('div');
      el.className = 'glass-selectize-option';
      if (instance.selected.indexOf(option.value) !== -1) el.classList.add('selected');
      el.textContent = option.label;
      el.setAttribute('data-value', option.value);
      el.setAttribute('data-index', j);
      frag.appendChild(el);
    }
    instance.dropdown.appendChild(frag);
    instance.dropdownDirty = false;
  }

  // ── Selection ──────────────────────────────────────────
  function selectOption(instance, value) {
    if (!instance.multiple) {
      instance.selected = [value];
      closeDropdown(instance);
      instance.input.blur();
    } else {
      if (instance.maxItems && instance.selected.length >= instance.maxItems) return;
      if (instance.selected.indexOf(value) === -1) instance.selected.push(value);
      instance.searchTerm = '';
    }
    renderSelected(instance);
    if (instance.dropdown.classList.contains('active')) renderOptions(instance);
    updateShinyValue(instance);
  }

  function removeItem(instance, value) {
    instance.selected = instance.selected.filter(function(v) { return v !== value; });
    renderSelected(instance);
    if (instance.dropdown.classList.contains('active')) renderOptions(instance);
    updateShinyValue(instance);
  }

  // ── Dropdown open / close ──────────────────────────────
  function openDropdown(instance) {
    if (!instance.dropdown.classList.contains('active')) {
      instance.dropdownDirty = true;
      renderOptions(instance);
    }
    instance.dropdown.classList.add('active');
  }

  function closeDropdown(instance) {
    instance.dropdown.classList.remove('active');
    instance.searchTerm = '';
    // Don't rebuild DOM — just mark dirty for next open
    instance.dropdownDirty = true;
  }

  // ── Keyboard navigation ────────────────────────────────
  function handleKeyboard(e, instance) {
    var options = instance.dropdown.querySelectorAll('.glass-selectize-option:not(.selected)');

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        instance.highlightedIndex = Math.min(instance.highlightedIndex + 1, options.length - 1);
        updateHighlight(instance);
        break;
      case 'ArrowUp':
        e.preventDefault();
        instance.highlightedIndex = Math.max(instance.highlightedIndex - 1, 0);
        updateHighlight(instance);
        break;
      case 'Enter':
        e.preventDefault();
        if (instance.highlightedIndex >= 0 && options[instance.highlightedIndex]) {
          selectOption(instance, options[instance.highlightedIndex].getAttribute('data-value'));
        }
        break;
      case 'Escape':
        e.preventDefault();
        closeDropdown(instance);
        instance.input.blur();
        break;
      case 'Backspace':
        if (instance.multiple && !instance.input.value && instance.selected.length > 0) {
          removeItem(instance, instance.selected[instance.selected.length - 1]);
        }
        break;
    }
  }

  function updateHighlight(instance) {
    var options = instance.dropdown.querySelectorAll('.glass-selectize-option');
    for (var i = 0; i < options.length; i++) {
      if (i === instance.highlightedIndex) {
        options[i].classList.add('highlighted');
        options[i].scrollIntoView({ block: 'nearest', behavior: 'smooth' });
      } else {
        options[i].classList.remove('highlighted');
      }
    }
  }

  // ── Shiny integration ──────────────────────────────────
  function updateShinyValue(instance) {
    var value = instance.multiple ? instance.selected : (instance.selected[0] || null);
    Shiny.setInputValue(instance.container.id, value);
  }

  // Merge a server message into instance state (no render)
  function mergeUpdateMessage(instance, message) {
    if (message.options) {
      instance.options   = message.options;
      instance.optionMap = buildOptionMap(message.options);
    }
    if (message.selected !== undefined) {
      instance.selected = Array.isArray(message.selected)
        ? message.selected
        : [message.selected];
    }
    if (message.placeholder) {
      instance.input.placeholder = message.placeholder;
    }
  }

  // Apply an update and render
  function applySelectizeUpdate(instance, message) {
    mergeUpdateMessage(instance, message);
    renderSelected(instance);
    if (instance.dropdown.classList.contains('active')) renderOptions(instance);
    updateShinyValue(instance);
  }

  // Debounced handler: rapid-fire server updates are coalesced
  Shiny.addCustomMessageHandler('glass-selectize-update', function(message) {
    var id = message.id;
    var instance = selectizeInstances[id];

    if (!instance) {
      // Instance not yet initialised — queue (keep only latest)
      pendingUpdates[id] = [message];
      return;
    }

    // Merge state immediately (cheap), but debounce the expensive render
    mergeUpdateMessage(instance, message);
    if (debouncedUpdateTimers[id]) cancelAnimationFrame(debouncedUpdateTimers[id]);
    debouncedUpdateTimers[id] = requestAnimationFrame(function() {
      delete debouncedUpdateTimers[id];
      renderSelected(instance);
      if (instance.dropdown.classList.contains('active')) renderOptions(instance);
      updateShinyValue(instance);
    });
  });

  // OutputBinding (for renderUI-injected widgets)
  var glassSelectizeBinding = new Shiny.OutputBinding();

  glassSelectizeBinding.find = function(scope) {
    return $(scope).find('.glass-selectize-wrapper');
  };

  glassSelectizeBinding.renderValue = function(el, data) {
    var container = $(el).find('.glass-selectize-container')[0];
    if (container && !container.classList.contains('initialized')) {
      setTimeout(function() { initGlassSelectize(container); }, 100);
    }
  };

  Shiny.outputBindings.register(glassSelectizeBinding, 'glassSelectize');

})();
