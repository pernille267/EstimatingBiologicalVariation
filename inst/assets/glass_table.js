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

// --- TABLE DOWNLOAD LOGIC (CSV & XLSX) ---

(function() {

  // --- Get export data from embedded JSON script tag ---
  function getExportData(container) {
    var scriptEl = container.querySelector('script.glass-table-export-data');
    if (!scriptEl) return null;
    try {
      return JSON.parse(scriptEl.textContent);
    } catch (e) {
      console.error('[GlassTable] Failed to parse export data:', e);
      return null;
    }
  }

  // --- CSV Builder ---
  function buildCsv(data) {
    var lines = [];
    lines.push(data.columns.map(csvEscape).join(','));
    data.rows.forEach(function(row) {
      lines.push(row.map(function(val) {
        if (val === null || val === undefined) return '';
        return csvEscape(String(val));
      }).join(','));
    });
    return lines.join('\r\n');
  }

  function csvEscape(val) {
    if (val === null || val === undefined) return '';
    var s = String(val);
    if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 ||
        s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0) {
      return '"' + s.replace(/"/g, '""') + '"';
    }
    return s;
  }

  // --- XLSX Builder (minimal pure-JS, no external libraries) ---

  // Column index to Excel reference (0 -> A, 25 -> Z, 26 -> AA)
  function colRef(i) {
    var r = '';
    i++;
    while (i > 0) {
      i--;
      r = String.fromCharCode(65 + (i % 26)) + r;
      i = Math.floor(i / 26);
    }
    return r;
  }

  function escapeXml(s) {
    return String(s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }

  function buildXlsx(data) {
    // Shared strings pool
    var sharedStrings = [];
    var ssMap = {};
    function getSSIndex(str) {
      str = String(str);
      if (ssMap.hasOwnProperty(str)) return ssMap[str];
      var idx = sharedStrings.length;
      sharedStrings.push(str);
      ssMap[str] = idx;
      return idx;
    }

    // Index headers
    data.columns.forEach(function(h) { getSSIndex(h); });

    // Build worksheet XML
    var sp = [];
    sp.push('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    sp.push('<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">');
    sp.push('<sheetData>');

    // Header row (style s="1" = bold)
    sp.push('<row r="1">');
    data.columns.forEach(function(col, ci) {
      sp.push('<c r="' + colRef(ci) + '1" t="s" s="1"><v>' +
        getSSIndex(col) + '</v></c>');
    });
    sp.push('</row>');

    // Data rows
    data.rows.forEach(function(row, ri) {
      var rowNum = ri + 2;
      sp.push('<row r="' + rowNum + '">');
      row.forEach(function(val, ci) {
        var ref = colRef(ci) + rowNum;
        if (val === null || val === undefined) return;
        var type = data.types ? data.types[ci] : 'string';
        if (type === 'number' && !isNaN(Number(val))) {
          sp.push('<c r="' + ref + '"><v>' + val + '</v></c>');
        } else {
          sp.push('<c r="' + ref + '" t="s"><v>' +
            getSSIndex(String(val)) + '</v></c>');
        }
      });
      sp.push('</row>');
    });
    sp.push('</sheetData></worksheet>');
    var sheetXml = sp.join('');

    // Shared strings XML
    var ssp = [];
    ssp.push('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>');
    ssp.push('<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"' +
      ' count="' + sharedStrings.length + '" uniqueCount="' + sharedStrings.length + '">');
    sharedStrings.forEach(function(s) {
      ssp.push('<si><t>' + escapeXml(s) + '</t></si>');
    });
    ssp.push('</sst>');
    var ssXml = ssp.join('');

    // Content Types
    var contentTypes =
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
      '<Default Extension="xml" ContentType="application/xml"/>' +
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>' +
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' +
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>' +
      '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>' +
      '</Types>';

    // Root relationships
    var rels =
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>' +
      '</Relationships>';

    // Workbook
    var workbook =
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"' +
      ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">' +
      '<sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>';

    // Workbook relationships
    var wbRels =
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>' +
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>' +
      '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>' +
      '</Relationships>';

    // Styles (index 0 = normal, index 1 = bold header)
    var styles =
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">' +
      '<fonts count="2">' +
        '<font><sz val="11"/><name val="Calibri"/></font>' +
        '<font><b/><sz val="11"/><name val="Calibri"/></font>' +
      '</fonts>' +
      '<fills count="2">' +
        '<fill><patternFill patternType="none"/></fill>' +
        '<fill><patternFill patternType="gray125"/></fill>' +
      '</fills>' +
      '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>' +
      '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>' +
      '<cellXfs count="2">' +
        '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>' +
        '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>' +
      '</cellXfs>' +
      '</styleSheet>';

    // Bundle into ZIP (stored, no compression)
    var files = [
      { name: '[Content_Types].xml', content: contentTypes },
      { name: '_rels/.rels', content: rels },
      { name: 'xl/workbook.xml', content: workbook },
      { name: 'xl/_rels/workbook.xml.rels', content: wbRels },
      { name: 'xl/worksheets/sheet1.xml', content: sheetXml },
      { name: 'xl/styles.xml', content: styles },
      { name: 'xl/sharedStrings.xml', content: ssXml }
    ];

    return createZip(files);
  }

  // --- Minimal ZIP builder (stored, no compression) ---

  var crcTable = null;
  function getCrcTable() {
    if (crcTable) return crcTable;
    crcTable = new Uint32Array(256);
    for (var n = 0; n < 256; n++) {
      var c = n;
      for (var k = 0; k < 8; k++) {
        c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
      }
      crcTable[n] = c;
    }
    return crcTable;
  }

  function crc32(data) {
    var table = getCrcTable();
    var crc = 0xFFFFFFFF;
    for (var i = 0; i < data.length; i++) {
      crc = (crc >>> 8) ^ table[(crc ^ data[i]) & 0xFF];
    }
    return (crc ^ 0xFFFFFFFF) >>> 0;
  }

  function createZip(files) {
    var encoder = new TextEncoder();
    var centralDir = [];
    var localParts = [];
    var offset = 0;

    files.forEach(function(file) {
      var nameBytes = encoder.encode(file.name);
      var dataBytes = encoder.encode(file.content);
      var crc = crc32(dataBytes);

      // Local file header (30 bytes) + name + data
      var localBuf = new ArrayBuffer(30 + nameBytes.length + dataBytes.length);
      var lv = new DataView(localBuf);
      var lu = new Uint8Array(localBuf);
      lv.setUint32(0, 0x04034b50, true);
      lv.setUint16(4, 20, true);
      lv.setUint16(6, 0, true);
      lv.setUint16(8, 0, true);
      lv.setUint16(10, 0, true);
      lv.setUint16(12, 0, true);
      lv.setUint32(14, crc, true);
      lv.setUint32(18, dataBytes.length, true);
      lv.setUint32(22, dataBytes.length, true);
      lv.setUint16(26, nameBytes.length, true);
      lv.setUint16(28, 0, true);
      lu.set(nameBytes, 30);
      lu.set(dataBytes, 30 + nameBytes.length);
      localParts.push(lu);

      // Central directory header (46 bytes) + name
      var cdBuf = new ArrayBuffer(46 + nameBytes.length);
      var cv = new DataView(cdBuf);
      var cu = new Uint8Array(cdBuf);
      cv.setUint32(0, 0x02014b50, true);
      cv.setUint16(4, 20, true);
      cv.setUint16(6, 20, true);
      cv.setUint16(8, 0, true);
      cv.setUint16(10, 0, true);
      cv.setUint16(12, 0, true);
      cv.setUint16(14, 0, true);
      cv.setUint32(16, crc, true);
      cv.setUint32(20, dataBytes.length, true);
      cv.setUint32(24, dataBytes.length, true);
      cv.setUint16(28, nameBytes.length, true);
      cv.setUint16(30, 0, true);
      cv.setUint16(32, 0, true);
      cv.setUint16(34, 0, true);
      cv.setUint16(36, 0, true);
      cv.setUint32(38, 0, true);
      cv.setUint32(42, offset, true);
      cu.set(nameBytes, 46);
      centralDir.push(cu);

      offset += lu.length;
    });

    // End of central directory (22 bytes)
    var cdSize = centralDir.reduce(function(a, b) { return a + b.length; }, 0);
    var eocdBuf = new ArrayBuffer(22);
    var ev = new DataView(eocdBuf);
    ev.setUint32(0, 0x06054b50, true);
    ev.setUint16(4, 0, true);
    ev.setUint16(6, 0, true);
    ev.setUint16(8, files.length, true);
    ev.setUint16(10, files.length, true);
    ev.setUint32(12, cdSize, true);
    ev.setUint32(16, offset, true);
    ev.setUint16(20, 0, true);

    // Concatenate all parts
    var allParts = localParts.concat(centralDir, [new Uint8Array(eocdBuf)]);
    var totalLen = allParts.reduce(function(a, b) { return a + b.length; }, 0);
    var result = new Uint8Array(totalLen);
    var pos = 0;
    allParts.forEach(function(part) {
      result.set(part, pos);
      pos += part.length;
    });
    return result.buffer;
  }

  // --- Download trigger (Blob + anchor) ---
  function triggerDownload(content, filename, mimeType) {
    var blob;
    if (content instanceof ArrayBuffer) {
      blob = new Blob([content], { type: mimeType });
    } else {
      blob = new Blob([content], { type: mimeType + ';charset=utf-8' });
    }
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(function() { URL.revokeObjectURL(url); }, 100);
  }

  // --- Delegated click handler for download buttons ---
  $(document).off('click', '.glass-table-dl-btn').on('click', '.glass-table-dl-btn', function(e) {
    e.preventDefault();
    var btn = $(this);
    var container = btn.closest('.glass-table-container');
    if (!container.length) return;

    var data = getExportData(container[0]);
    if (!data) {
      console.error('[GlassTable] No export data found for download.');
      return;
    }

    var format = btn.data('format');
    var filename = data.filename || 'table_export';

    if (format === 'csv') {
      // BOM prefix ensures Excel handles UTF-8 correctly
      var csv = '\ufeff' + buildCsv(data);
      triggerDownload(csv, filename + '.csv', 'text/csv');
    } else if (format === 'xlsx') {
      var xlsx = buildXlsx(data);
      triggerDownload(xlsx, filename + '.xlsx',
        'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
    }
  });

})();

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
