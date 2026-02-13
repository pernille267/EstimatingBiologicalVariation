/* ============================================================================
   Glass Analysis Progress — JavaScript Controller
   ============================================================================ */

$(document).ready(function () {

  // ---- State ----
  var _state = {
    startTime: null,
    timerInterval: null,
    currentValue: 0,
    targetValue: 0
  };

  // ---- Helpers ----

  function pad(n) {
    return n < 10 ? '0' + n : '' + n;
  }

  function formatDuration(ms) {
    if (ms < 0) ms = 0;
    var totalSec = Math.floor(ms / 1000);
    var h = Math.floor(totalSec / 3600);
    var m = Math.floor((totalSec % 3600) / 60);
    var s = totalSec % 60;
    if (h > 0) return pad(h) + ':' + pad(m) + ':' + pad(s);
    return pad(m) + ':' + pad(s);
  }

  function formatTime(date) {
    if (!date) return '--:--:--';
    return pad(date.getHours()) + ':' + pad(date.getMinutes()) + ':' + pad(date.getSeconds());
  }

  // Animated number lerp
  function animatePercentage($el, from, to, duration) {
    var startTs = performance.now();
    duration = duration || 400;
    function step(now) {
      var t = Math.min((now - startTs) / duration, 1);
      // ease-out cubic
      t = 1 - Math.pow(1 - t, 3);
      var val = Math.round(from + (to - from) * t);
      $el.text(val + '%');
      if (t < 1) requestAnimationFrame(step);
    }
    requestAnimationFrame(step);
  }

  // ---- Build DOM ----

  function buildProgressHTML(title) {
    return `
    <div id="glass-analysis-progress-overlay" class="glass-analysis-progress-overlay">
      <div class="glass-progress-card" id="glass-progress-card">

        <!-- Header -->
        <div class="glass-progress-header">
          <div class="glass-progress-header-icon">
            <svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
              <path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-1 17.93c-3.95-.49-7-3.85-7-7.93 0-.62.08-1.21.21-1.79L9 15v1c0 1.1.9 2 2 2v1.93zm6.9-2.54c-.26-.81-1-1.39-1.9-1.39h-1v-3c0-.55-.45-1-1-1H8v-2h2c.55 0 1-.45 1-1V7h2c1.1 0 2-.9 2-2v-.41c2.93 1.19 5 4.06 5 7.41 0 2.08-.8 3.97-2.1 5.39z"/>
            </svg>
          </div>
          <div>
            <div class="glass-progress-title" id="glass-progress-title">${title || 'Running Analysis'}</div>
            <div class="glass-progress-subtitle">Bayesian MCMC Sampling</div>
          </div>
        </div>

        <!-- Percentage + step label -->
        <div class="glass-progress-percentage-row">
          <span class="glass-progress-percentage" id="glass-progress-pct">0%</span>
          <span class="glass-progress-step-label" id="glass-progress-step">Initializing...</span>
        </div>

        <!-- Bar -->
        <div class="glass-progress-bar-track">
          <div class="glass-progress-bar-fill" id="glass-progress-bar" style="width: 0%;"></div>
        </div>

        <!-- Detail -->
        <div class="glass-progress-detail">
          <span class="glass-progress-detail-dot"></span>
          <span id="glass-progress-detail-text">Preparing...</span>
        </div>

        <!-- Time grid -->
        <div class="glass-progress-time-grid">
          <div class="glass-progress-time-box">
            <div class="glass-progress-time-label">Elapsed</div>
            <div class="glass-progress-time-value" id="glass-progress-elapsed">00:00</div>
          </div>
          <div class="glass-progress-time-box">
            <div class="glass-progress-time-label">Started</div>
            <div class="glass-progress-time-value" id="glass-progress-start">--:--:--</div>
          </div>
          <div class="glass-progress-time-box">
            <div class="glass-progress-time-label">ETA</div>
            <div class="glass-progress-time-value" id="glass-progress-eta">--:--:--</div>
          </div>
        </div>

      </div>
    </div>`;
  }

  // ---- Tick (runs every second) ----

  function tick() {
    if (!_state.startTime) return;
    var now = new Date();
    var elapsed = now - _state.startTime;

    // Elapsed
    $('#glass-progress-elapsed').text(formatDuration(elapsed));

    // ETA calculation — only after meaningful progress
    var pct = _state.targetValue;
    if (pct > 0 && pct < 100) {
      var estimatedTotal = elapsed / (pct / 100);
      var remaining = estimatedTotal - elapsed;
      var eta = new Date(now.getTime() + remaining);
      $('#glass-progress-eta').text(formatTime(eta));
    } else if (pct >= 100) {
      $('#glass-progress-eta').text(formatTime(now));
    }
  }

  // ---- Shiny handlers ----

  // OPEN
  Shiny.addCustomMessageHandler('glass-analysis-progress-open', function (msg) {
    // Remove any existing overlay
    $('#glass-analysis-progress-overlay').remove();

    // Reset state
    _state.startTime = new Date();
    _state.currentValue = 0;
    _state.targetValue = 0;
    if (_state.timerInterval) clearInterval(_state.timerInterval);

    // Insert
    $('body').append(buildProgressHTML(msg.title || 'Running Analysis'));

    // Set start time display
    $('#glass-progress-start').text(formatTime(_state.startTime));

    // Start timer
    _state.timerInterval = setInterval(tick, 1000);

    // Fade in
    requestAnimationFrame(function () {
      $('#glass-analysis-progress-overlay').addClass('visible');
    });
  });

  // UPDATE
  Shiny.addCustomMessageHandler('glass-analysis-progress-update', function (msg) {
    var pctRaw = msg.value; // 0-1 float
    if (pctRaw === undefined || pctRaw === null) return;

    var newPct = Math.min(Math.round(pctRaw * 100), 100);
    var oldPct = _state.currentValue;
    _state.targetValue = newPct;

    // Bar width
    $('#glass-progress-bar').css('width', newPct + '%');

    // Animated percentage number
    animatePercentage($('#glass-progress-pct'), oldPct, newPct, 450);
    _state.currentValue = newPct;

    // Detail text
    if (msg.detail) {
      $('#glass-progress-detail-text').text(msg.detail);
    }

    // Step label (top right)
    if (msg.step) {
      $('#glass-progress-step').text(msg.step);
    }

    // Trigger immediate tick for ETA recalc
    tick();

    // If 100%, mark completed
    if (newPct >= 100) {
      $('#glass-progress-card').addClass('completed');
      $('#glass-progress-detail-text').text(msg.detail || 'Complete!');
    }
  });

  // CLOSE
  Shiny.addCustomMessageHandler('glass-analysis-progress-close', function (msg) {
    if (_state.timerInterval) {
      clearInterval(_state.timerInterval);
      _state.timerInterval = null;
    }

    var $overlay = $('#glass-analysis-progress-overlay');
    if ($overlay.length) {
      // Brief delay so user can see 100%
      var delay = msg.delay !== undefined ? msg.delay : 800;
      setTimeout(function () {
        $overlay.removeClass('visible');
        setTimeout(function () { $overlay.remove(); }, 450);
      }, delay);
    }
  });

  // ERROR
  Shiny.addCustomMessageHandler('glass-analysis-progress-error', function (msg) {
    if (_state.timerInterval) {
      clearInterval(_state.timerInterval);
      _state.timerInterval = null;
    }

    $('#glass-progress-card').removeClass('completed').addClass('errored');
    $('#glass-progress-detail-text').text(msg.detail || 'An error occurred.');
    $('#glass-progress-step').text('Error');

    // Auto-close after a few seconds
    var delay = msg.delay !== undefined ? msg.delay : 4000;
    if (delay > 0) {
      setTimeout(function () {
        var $overlay = $('#glass-analysis-progress-overlay');
        $overlay.removeClass('visible');
        setTimeout(function () { $overlay.remove(); }, 450);
      }, delay);
    }
  });

});
