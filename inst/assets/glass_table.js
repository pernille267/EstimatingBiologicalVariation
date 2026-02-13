// ==========================================================================
// GLASS TABLE JS - v13.0 (Pagination & Global Delegation)
// ==========================================================================

(function() {
  console.log("[GlassTable] Script loaded v13.1");

  // 1. GLOBAL INITIALIZATION (Runs on new content)
  // Uses MutationObserver + Shiny events to reliably catch dynamically rendered tables.

  function attemptInit() {
    $('.glass-table-container:not(.paginated)').each(function() {
      initPagination(this);
    });
  }

  // Bind to Shiny lifecycle events for future renders
  $(document).on('shiny:connected shiny:sessioninitialized shiny:value', function() {
    setTimeout(attemptInit, 100);
  });

  // Immediate init on script load (covers dynamic htmlDependency loading via renderUI
  // where shiny:connected / shiny:value have already fired before this script arrives)
  if (document.readyState === 'complete' || document.readyState === 'interactive') {
    setTimeout(attemptInit, 50);
    setTimeout(attemptInit, 300);
  } else {
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(attemptInit, 50);
    });
  }

  // MutationObserver: catches tables injected after all events have passed
  var tableObserver = new MutationObserver(function(mutations) {
    var dominated = false;
    for (var i = 0; i < mutations.length; i++) {
      if (mutations[i].addedNodes.length > 0) { dominated = true; break; }
    }
    if (dominated) setTimeout(attemptInit, 80);
  });

  function startObserving() {
    if (document.body) {
      tableObserver.observe(document.body, { childList: true, subtree: true });
    } else {
      document.addEventListener('DOMContentLoaded', function() {
        tableObserver.observe(document.body, { childList: true, subtree: true });
      });
    }
  }
  startObserving();

  // 2. GLOBAL SORT LISTENER
  $(document).off('click', '.glass-th.sortable').on('click', '.glass-th.sortable', function(e) {
    var table = $(this).closest('table.glass-table')[0];
    if (table) sortTable(table, this);
  });

  // 3. GLOBAL PAGE BUTTON LISTENER
  $(document).off('click', '.glass-page-btn').on('click', '.glass-page-btn', function(e) {
    var btn = $(this);
    var container = btn.closest('.glass-table-container');
    var action = btn.data('action'); // 'prev', 'next', or page number

    if (!container.length || btn.is(':disabled') || btn.hasClass('active')) return;

    var currentPage = parseInt(container.attr('data-current-page')) || 1;
    var newPage = currentPage;

    if (action === 'prev') newPage--;
    else if (action === 'next') newPage++;
    else newPage = parseInt(action);

    updateTablePage(container[0], newPage);
  });

})();

// --- PAGINATION LOGIC ---

function initPagination(container) {
  var table = container.querySelector('table.glass-table');
  if (!table) return;
  var tbody = table.querySelector('tbody');
  if (!tbody) return;
  var rows = tbody.querySelectorAll('tr');
  if (!rows || rows.length === 0) return;
  var pageSize = parseInt(container.getAttribute('data-page-size')) || 25;

  // Mark as processed
  container.classList.add('paginated');
  container.setAttribute('data-current-page', '1');

  // If rows are few, hide the pagination footer entirely
  if (rows.length <= pageSize) {
    var footer = container.querySelector('.glass-pagination-container');
    if (footer) footer.style.display = 'none';
    return;
  }

  updateTablePage(container, 1);
}

function updateTablePage(container, pageNum) {
  var table = container.querySelector('table.glass-table');
  var tbody = table.querySelector('tbody');
  var rows = Array.from(tbody.querySelectorAll('tr')); // Get ALL rows (hidden or shown)
  var pageSize = parseInt(container.getAttribute('data-page-size')) || 25;
  var totalPages = Math.ceil(rows.length / pageSize);

  // 1. Clamp Page
  if (pageNum < 1) pageNum = 1;
  if (pageNum > totalPages) pageNum = totalPages;

  // 2. Save State
  container.setAttribute('data-current-page', pageNum);

  // 3. Show/Hide Rows
  var start = (pageNum - 1) * pageSize;
  var end = start + pageSize;

  rows.forEach(function(row, index) {
    if (index >= start && index < end) {
      row.style.display = '';
      // Fix Striping manually because CSS nth-child counts hidden rows
      // We calculate local index (0 to 24) to determine stripe
      var localIndex = index - start;
      row.style.backgroundColor = (localIndex % 2 !== 0) ? 'rgba(248, 247, 252, 0.6)' : 'rgba(255, 255, 255, 0.7)';
    } else {
      row.style.display = 'none';
    }
  });

  // 4. Rebuild Buttons
  renderPaginationControls(container, pageNum, totalPages);
}

function renderPaginationControls(container, current, total) {
  var footer = container.querySelector('.glass-pagination-container');
  if (!footer) return;

  footer.innerHTML = ''; // Clear existing

  // Prev Button
  var btnPrev = createButton('<i class="fas fa-chevron-left"></i>', 'prev', current === 1);
  footer.appendChild(btnPrev);

  // Logic for "Smart" Page Numbers (1, 2 ... 5 6 7 ... 10)
  var range = [];
  if (total <= 7) {
    for (var i = 1; i <= total; i++) range.push(i);
  } else {
    if (current <= 4) {
      range = [1, 2, 3, 4, 5, '...', total];
    } else if (current >= total - 3) {
      range = [1, '...', total - 4, total - 3, total - 2, total - 1, total];
    } else {
      range = [1, '...', current - 1, current, current + 1, '...', total];
    }
  }

  range.forEach(function(item) {
    if (item === '...') {
      var span = document.createElement('span');
      span.className = 'glass-page-dots';
      span.innerText = '...';
      footer.appendChild(span);
    } else {
      var btn = createButton(item, item, false);
      if (item === current) btn.classList.add('active');
      footer.appendChild(btn);
    }
  });

  // Next Button
  var btnNext = createButton('<i class="fas fa-chevron-right"></i>', 'next', current === total);
  footer.appendChild(btnNext);
}

function createButton(html, action, disabled) {
  var btn = document.createElement('button');
  btn.className = 'glass-page-btn';
  btn.innerHTML = html;
  btn.setAttribute('data-action', action);
  if (disabled) btn.disabled = true;
  return btn;
}

// --- SORTING LOGIC ---

function sortTable(table, header) {
  var tbody = table.querySelector('tbody');
  var colIndex = parseInt(header.getAttribute('data-col-index'));
  if (isNaN(colIndex)) return;

  // 1. Direction Logic
  var currentDir = header.getAttribute('data-sort-dir') || 'none';
  var newDir = (currentDir === 'asc') ? 'desc' : 'asc';

  // 2. Update Icons
  var allHeaders = table.querySelectorAll('.glass-th');
  allHeaders.forEach(function(th) {
    th.setAttribute('data-sort-dir', 'none');
    var icon = th.querySelector('.glass-sort-icon');
    if (icon) icon.className = 'fas fa-sort glass-sort-icon';
  });
  header.setAttribute('data-sort-dir', newDir);
  var activeIcon = header.querySelector('.glass-sort-icon');
  if (activeIcon) activeIcon.className = (newDir === 'asc') ? 'fas fa-sort-up glass-sort-icon' : 'fas fa-sort-down glass-sort-icon';

  // 3. Actual Sort
  var rows = Array.from(tbody.querySelectorAll('tr'));
  rows.sort(function(rowA, rowB) {
    var cellA = rowA.children[colIndex];
    var cellB = rowB.children[colIndex];
    if (!cellA || !cellB) return 0;

    var txtA = cellA.textContent.trim();
    var txtB = cellB.textContent.trim();

    var cleanA = txtA.replace(/[^-0-9.]/g, '');
    var cleanB = txtB.replace(/[^-0-9.]/g, '');
    var numA = parseFloat(cleanA);
    var numB = parseFloat(cleanB);
    var isDate = /^\d{4}-\d{2}-\d{2}/.test(txtA);

    var isNumA = !isNaN(numA) && isFinite(numA) && cleanA !== "" && !isDate;
    var isNumB = !isNaN(numB) && isFinite(numB) && cleanB !== "" && !isDate;

    if (isNumA && isNumB) {
      return (newDir === 'asc') ? numA - numB : numB - numA;
    } else {
      return (newDir === 'asc') ?
        txtA.localeCompare(txtB, undefined, {numeric: true, sensitivity: 'base'}) :
        txtB.localeCompare(txtA, undefined, {numeric: true, sensitivity: 'base'});
    }
  });

  // 4. Re-append sorted rows to DOM
  var fragment = document.createDocumentFragment();
  rows.forEach(function(row) { fragment.appendChild(row); });
  tbody.appendChild(fragment);

  // 5. IMPORTANT: RESET TO PAGE 1
  // We need to trigger the pagination redraw so the first 25 sorted rows show up
  var container = table.closest('.glass-table-container');
  updateTablePage(container, 1);
}
