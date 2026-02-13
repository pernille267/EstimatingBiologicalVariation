$(document).on('shiny:connected', function() {
  initGlassSliders();
});

$(document).on('shiny:sessioninitialized', function() {
  initGlassSliders();
});

function initGlassSliders() {
  var sliders = document.querySelectorAll('.glass-slider-container:not(.initialized)');
  sliders.forEach(function(slider) {
    // Clean up any previous listeners if slider was re-rendered with same DOM
    if (slider._sliderAbort) {
      slider._sliderAbort.abort();
      slider._sliderAbort = null;
    }
    setupGlassSlider(slider);
    slider.classList.add('initialized');
  });
}

function setupGlassSlider(slider) {
  var id = slider.id.replace('container-', '');
  var track = slider.querySelector('.glass-slider-track');
  var handle = slider.querySelector('.glass-slider-handle');
  var fill = slider.querySelector('.glass-slider-fill');
  var tooltip = slider.querySelector('.glass-slider-tooltip');

  // Parse data attributes
  var choices = JSON.parse(slider.dataset.choices);
  var unit = slider.dataset.unit || ""; // Get the unit string (e.g., " dpi")
  var count = choices.length;
  var stepSize = 100 / (count - 1);

  var isDragging = false;

  function updatePosition(index) {
    if (index < 0) index = 0;
    if (index >= count) index = count - 1;

    var percent = index * stepSize;

    handle.style.left = percent + '%';
    fill.style.width = percent + '%';

    // Append unit to the tooltip text
    tooltip.innerText = choices[index] + unit;

    // Note: We send ONLY the value to Shiny, not the unit appended string
    // This keeps your data clean (e.g., sending 100 instead of "100 dpi")
    Shiny.setInputValue(id, choices[index]);
  }

  function getIndexFromEvent(e) {
    var rect = track.getBoundingClientRect();
    var clientX = (e.touches && e.touches.length > 0) ? e.touches[0].clientX : e.clientX;
    var offsetX = clientX - rect.left;
    var width = rect.width;
    var percent = offsetX / width;
    var rawIndex = Math.round(percent * (count - 1));
    return Math.max(0, Math.min(rawIndex, count - 1));
  }

  // --- Event Listeners (with AbortController for cleanup) ---
  var controller = new AbortController();
  slider._sliderAbort = controller;

  handle.addEventListener('mousedown', function(e) {
    isDragging = true;
    handle.classList.add('active');
    e.preventDefault(); e.stopPropagation();
  }, { signal: controller.signal });

  handle.addEventListener('touchstart', function(e) {
    isDragging = true;
    handle.classList.add('active');
  }, { passive: false, signal: controller.signal });

  document.addEventListener('mousemove', function(e) {
    if (!isDragging) return;
    e.preventDefault();
    updatePosition(getIndexFromEvent(e));
  }, { signal: controller.signal });

  document.addEventListener('touchmove', function(e) {
    if (!isDragging) return;
    updatePosition(getIndexFromEvent(e));
  }, { passive: false, signal: controller.signal });

  document.addEventListener('mouseup', function() {
    if (isDragging) { isDragging = false; handle.classList.remove('active'); }
  }, { signal: controller.signal });

  document.addEventListener('touchend', function() {
    if (isDragging) { isDragging = false; handle.classList.remove('active'); }
  }, { signal: controller.signal });

  track.addEventListener('click', function(e) {
    updatePosition(getIndexFromEvent(e));
  }, { signal: controller.signal });

  var labels = slider.querySelectorAll('.glass-slider-label');
  labels.forEach(function(lbl) {
    lbl.addEventListener('click', function() {
      var idx = parseInt(lbl.dataset.index);
      updatePosition(idx);
    });
  });

  var initialIndex = parseInt(slider.dataset.selectedIndex);
  updatePosition(initialIndex);
}
