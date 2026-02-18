// ==========================================================================
// GLASS D3 PLOTS v1.0 — Biological Variation Chart Engine
// Supports: Subject-Specific CVI, Trace Plots, Posterior Density,
//           Prior Density, CVI vs Concentration, CVI vs RCV
// Features: Zoom, Pan, Tooltips, Download (.png / .tif), DPI choice, Data Export (.csv / .xlsx)
// ==========================================================================

(function () {
  "use strict";

  /* ---- Shiny Integration ---- */
  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("update_glass_d3_plot", function (msg) {
      GlassD3Plot.render(msg.id, msg.payload);
    });
  });
  $(document).on("shiny:disconnected", function () {
    GlassD3Plot.destroyAll();
  });

  const instances = new Map();

  const GlassD3Plot = {
    render(containerId, payload) {
      let inst = instances.get(containerId);
      const containerElement = document.getElementById(containerId);
      if (!containerElement) return;
      if (inst && containerElement.querySelectorAll("svg.gd3-svg").length === 0) {
        GlassD3Plot.destroy(containerId);
        inst = null;
      }
      if (!inst) {
        inst = createPlotInstance(containerId);
        instances.set(containerId, inst);
      }
      inst.update(payload);
    },
    destroy(containerId) {
      const inst = instances.get(containerId);
      if (inst) { inst.destroy(); instances.delete(containerId); }
    },
    destroyAll() {
      for (const [id] of instances) GlassD3Plot.destroy(id);
    },
  };
  window.GlassD3Plot = GlassD3Plot;

  /* ---- Constants & Helpers ---- */
  const COLORS = {
    purple: "#605CA8",
    purpleDark: "#4B4885",
    purpleLight: "#7470B8",
    green: "#28A745",
    greenDark: "#1a702e",
    magenta: "#A7288A",
    orange: "#FF9800",
    red: "#DC1932",
    blue: "#1994DC",
    gray: "#999",
    grayLight: "#ccc",
    grayDark: "#333",
    white: "#fff",
    black: "#000",
  };

  const CHAIN_COLORS = [
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f"
  ];

  // Replace non-letters, non-digits, and non-underscore characters with underscores
  function safeId(inputString) {
    return String(inputString).replace(/[^a-zA-Z0-9_-]/g, "_");
  }
  // Format a number with fixed decimal places, defaulting to 2 if not specified
  function formatNumber(value, decimalPlaces) {
    return Number(value).toFixed(decimalPlaces == null ? 2 : decimalPlaces);
  }
  // Format a number as a percentage string with specified decimal places (default 1)
  function formatPercentage(value, decimalPlaces) {
    return formatNumber(value, decimalPlaces == null ? 1 : decimalPlaces) + " %";
  }
  // Determine data range (of valid values) or 
  // return a fallback range if no valid values are present
  function extentOrDefault(values, fallback) {
    const validValues = values.filter(
      value => value != null && isFinite(value)
    );
    if (validValues.length === 0) {
      return fallback || [0, 1];
    }
    const dataRange = d3.extent(validValues);
    if (dataRange[0] === dataRange[1]) {
      return [dataRange[0] - 1, dataRange[1] + 1];
    }
    return dataRange;
  }

  // Expand the data range by a percentage padding on either side, or 
  // return a fallback range if no valid values are present 
  function niceExtent(values, paddingMultiplier, fallback) {
    const baseRange = extentOrDefault(values, fallback);
    const [minBound, maxBound] = baseRange;
    const rangeSpan = maxBound - minBound;
    const paddingPercentage = paddingMultiplier == null ? 0.05 : paddingMultiplier;
    const lowerExpandedBound = minBound - rangeSpan * paddingPercentage;
    const upperExpandedBound = maxBound + rangeSpan * paddingPercentage;
    return [lowerExpandedBound, upperExpandedBound];
  }

  /**
   * Calculates a specific quantile for an array of numbers using linear interpolation.
   * Equivalent to R's quantile(x, probs = probability, type = 7).
   * @param {number[]} dataArray - The unsorted numerical input data.
   * @param {number} prob - A value between 0 and 1 (e.g., 0.95 for the 95th percentile).
   * @returns {number} The calculated quantile value.
   */
  function quantile(dataArray, prob) {
    const sortedData = dataArray.slice().sort((a, b) => a - b);
    const indexPosition = (sortedData.length - 1) * prob;
    const lowerIndex = Math.floor(indexPosition);
    const upperIndex = Math.ceil(indexPosition);
    // Position is a whole number, return the value at that index
    if (lowerIndex === upperIndex) {
      return sortedData[lowerIndex];
    }
    // Linear interpolation between the two bounding values
    const valueAtLowerIndex = sortedData[lowerIndex];
    const valueAtUpperIndex = sortedData[upperIndex];
    const interpolationFraction = indexPosition - lowerIndex;
    return valueAtLowerIndex + (valueAtUpperIndex - valueAtLowerIndex) * interpolationFraction;
  }

  /**
   * Centralised chart-dimension calculator.
   *
   * Standard (margin-based) usage:
   *   getDimensions(containerElement, margin, { plotH: n * rowH })
   *
   * Override any derived value via `overrides`:
   *   minPlotW  – minimum plotW (default 500)
   *   fallbackW – containerElement width fallback (default 700)
   *   plotW     – explicit plotW (skips the containerElement-based calc)
   *   plotH     – explicit plotH (required for most charts)
   *   totalW    – explicit totalW (skips margin.left + plotW + margin.right)
   *   totalH    – explicit totalH (skips margin.top  + plotH + margin.bottom)
   *
   * @param {HTMLElement} containerElement  DOM element whose clientWidth is measured.
   * @param {{ top:number, right:number, bottom:number, left:number }} margin
   * @param {object} [overrides]
   * @returns {{ plotW:number, plotH:number, totalW:number, totalH:number }}
   */
  function getDimensions(containerElement, margin, overrides) {
    const o     = overrides || {};
    const plotW  = o.plotW  != null ? o.plotW  : Math.max(o.minPlotW || 500, (containerElement.clientWidth || o.fallbackW || 700) - margin.left - margin.right);
    const plotH  = o.plotH  != null ? o.plotH  : 350;
    const totalW = o.totalW != null ? o.totalW : margin.left + plotW + margin.right;
    const totalH = o.totalH != null ? o.totalH : margin.top  + plotH + margin.bottom;
    return { plotW, plotH, totalW, totalH };
  }

  /* ---- Shared SVG Gradients & Defs ---- */

  /**
   * SharedGradients consolidates all reusable SVG <defs> (gradients,
   * clip-paths) so that every plot type uses identical visual styles
   * without duplicating the gradient markup.  Each call uses an
   * `idPrefix` to guarantee unique element IDs even when multiple
   * plots coexist on the same page.
   */
  const SharedGradients = {
    /**
     * Append a single <defs> block to `svg` containing all shared
     * gradients, namespaced by `idPrefix`.
     *
     * @param {d3.Selection} svg - The root SVG selection.
     * @param {string}       idPrefix - Unique prefix (e.g. safeId(containerId)).
     * @returns {{ glossyPoint: string, popBandH: string, popBandV: string, ribbonV: string }}
     *   An object whose values are the element IDs ready for `url(#…)`.
     */
    apply(svg, idPrefix) {
      let defs = svg.select("defs");
      if (defs.empty()) defs = svg.append("defs");

      const ids = {
        glossyPoint: idPrefix + "-glossy-pt",
        popBandH:    idPrefix + "-pop-band-h",
        popBandV:    idPrefix + "-pop-band-v",
        ribbonV:     idPrefix + "-ribbon-v",
      };

      // ── Glossy radial gradient for data points ──
      const glossGrad = defs.append("radialGradient")
        .attr("id", ids.glossyPoint)
        .attr("cx", "35%")
        .attr("cy", "30%")
        .attr("r", "65%")
        .attr("fx", "35%")
        .attr("fy", "30%");
      glossGrad.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", "#9A97D0");
      glossGrad.append("stop")
        .attr("offset", "45%")
        .attr("stop-color", "#605CA8");
      glossGrad.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", "#4B4885");

      // ── Population band gradient (horizontal, left→right) ──
      const popH = defs.append("linearGradient")
        .attr("id", ids.popBandH)
        .attr("x1", "0%")
        .attr("y1", "0%")
        .attr("x2", "100%")
        .attr("y2", "0%");
      popH.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.03);
      popH.append("stop")
        .attr("offset", "25%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.14);
      popH.append("stop")
        .attr("offset", "50%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.18);
      popH.append("stop")
        .attr("offset", "75%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.14);
      popH.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.03);

      // ── Population band gradient (vertical, top→bottom) ──
      const popV = defs.append("linearGradient")
        .attr("id", ids.popBandV)
        .attr("x1", "0%")
        .attr("y1", "0%")
        .attr("x2", "0%")
        .attr("y2", "100%");
      popV.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.03);
      popV.append("stop")
        .attr("offset", "25%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.14);
      popV.append("stop")
        .attr("offset", "50%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.18);
      popV.append("stop")
        .attr("offset", "75%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.14);
      popV.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.03);

      // ── Ribbon gradient (vertical, for RCV uncertainty bands) ──
      const ribbonV = defs.append("linearGradient")
        .attr("id", ids.ribbonV)
        .attr("x1", "0%")
        .attr("y1", "0%")
        .attr("x2", "0%")
        .attr("y2", "100%")
        .attr("gradientUnits", "objectBoundingBox");
      ribbonV.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.04);
      ribbonV.append("stop")
        .attr("offset", "30%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.22);
      ribbonV.append("stop")
        .attr("offset", "50%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.28);
      ribbonV.append("stop")
        .attr("offset", "70%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.22);
      ribbonV.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", "#28A745")
        .attr("stop-opacity", 0.04);

      return ids;
    },

    /**
     * Add a clip-path rectangle to the SVG's <defs>, creating <defs> if
     * it does not already exist.
     *
     * @param {d3.Selection} svg    - The root SVG selection.
     * @param {string}       clipId - Unique clip-path element ID.
     * @param {number}       width  - Clip rectangle width.
     * @param {number}       height - Clip rectangle height.
     * @returns {string} The same `clipId` for convenient chaining.
     */
    addClipPath(svg, clipId, width, height) {
      let defs = svg.select("defs");
      if (defs.empty()) {
        defs = svg.append("defs");
      }
      defs.append("clipPath")
        .attr("id", clipId)
        .append("rect")
        .attr("width", width)
        .attr("height", height);
      return clipId;
    },
  };

  /* ---- Download Utilities ---- */

  /**
   * Clone an SVG and injects all active CSS rules directly into it.
   * This ensures the exported image looks identical to the on-screen version.
   * 
   * @param {SVGElement} originalSvg - The source D3/SVG node to be cloned and styled.
   * @returns {SVGElement} A standalone clone of the SVG with embedded styles.
   */
  function prepareSvgForExport(originalSvg) {
    // Create a clone of the plot so we avoid messing with the live UI
    const svgClone = originalSvg.cloneNode(true);
    // Ensure the XML Namespace is present (required for standalone SVG files)
    if (!svgClone.getAttribute("xmlns")) {
      svgClone.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    }
    const matchedCssRules = [];
    try {
      // Iterate through all stylesheets and their rules to find those that apply to the original SVG.
      for (const sheet of document.styleSheets) {
        try {
          // Access the individual CSS rules (handles different browser naming)
          const sheetRules = sheet.cssRules || sheet.rules || [];
          for (const sheetRule of sheetRules) {
            if (sheetRule instanceof CSSStyleRule) {
              try {
                if (originalSvg.querySelector(sheetRule.selectorText)) {
                  matchedCssRules.push(sheetRule.cssText);
                }
              } catch (err) {
                /* selector parse error */
              }
            }
          }
        } catch (err) {
          /* cross-origin sheet – skip */
        }
      }
    } catch (err) {
      /* SecurityError or other issues accessing stylesheets – skip CSS injection */
    }
    if (matchedCssRules.length > 0) {
      const styleElement = document.createElementNS(
        "http://www.w3.org/2000/svg",
        "style"
      );
      styleElement.textContent = matchedCssRules.join("\n");
      // Place the <style> block before the first child (usually <defs> or <g>) to
      // ensure styles are applied correctly.
      svgClone.insertBefore(styleElement, svgClone.firstChild);
    }
    return svgClone;
  }

  /**
   * Renders an SVG element onto an HTML5 Canvas, allowing for high-DPI rasterization.
   * 
   * @param {SVGElement} svgElement - The source D3/SVG node. 
   * @param {number} width - The width of the resulting canvas (in pixels).
   * @param {number} height - The height of the resulting canvas (in pixels).
   * @param {number} targetDpi - Desired resolution (e.g., 300 or 600).
   * @param {string} backgroundColor - Fill color of the background (e.g., "#ffffff").
   * @returns {Promise<HTMLCanvasElement>} A promise that resolves to the painted canvas.
   */
  function svgToCanvas(svgElement, width, height, targetDpi, backgroundColor) {
    return new Promise((resolve, reject) => {
      const screenDpi = 96;
      const scaleFactor = targetDpi / screenDpi;
      // Prepare the canvas: Physical size = logical size × scale factor
      const canvas = document.createElement("canvas");
      canvas.width = width * scaleFactor;
      canvas.height = height * scaleFactor;

      // Get the 2D drawing context
      const canvasContext = canvas.getContext("2d");
      
      canvasContext.scale(scaleFactor, scaleFactor);
      if (backgroundColor) {
        canvasContext.fillStyle = backgroundColor;
        canvasContext.fillRect(0, 0, width, height);
      }

      // Inject CSS styles into a cloned SVG to ensure the exported image matches the on-screen version.
      const styledSvgClone = prepareSvgForExport(svgElement);

      // Turn SVG DOM into a string of XML code.
      const serializer = new XMLSerializer();
      const svgString = serializer.serializeToString(styledSvgClone);

      // Wrap the SVG string in a Blob and create a temporary URL for it, then
      // load that URL into an Image object.
      const svgBlob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
      const blobUrl = URL.createObjectURL(svgBlob);
      const virtualImage = new Image();
      virtualImage.onload = function () {
        canvasContext.drawImage(virtualImage, 0, 0, width, height);
        // Memory cleanup: stop the browser from holding the blob reference any longer than necessary.
        URL.revokeObjectURL(blobUrl);
        resolve(canvas);
      };
      virtualImage.onerror = (err) => reject(new Error("SVG-to-Canvas rendering failed."));
      virtualImage.src = blobUrl;
    });
  }

  /**
   * Finalizes the "Blob to File" process by creating a temporary
   * link and triggering a browser download.
   * @param {string} temproraryUrl - The 'blob:' URL created via URL.createObjectURL.
   * @param {string} downloadName - The filename the user will see (e.g., "chart.csv").
   * @param {boolean} shouldCleanUp - Whether to revoke the URL immediately after click.
   */
  function triggerDownload(temproraryUrl, downloadName, shouldCleanUp) {
    const hiddenLink = document.createElement("a");
    hiddenLink.href = temproraryUrl;
    hiddenLink.download = downloadName;
    document.body.appendChild(hiddenLink);
    hiddenLink.click();
    document.body.removeChild(hiddenLink);
    if (shouldCleanUp) {
      // Revoke the temporary URL to free up memory.
      // If the download is large, this can be important.
      URL.revokeObjectURL(temproraryUrl);
    }
  }

  async function downloadPng(svgNode, width, height, dpi, filename) {
    const drawingCanvas = await svgToCanvas(
      svgNode,
      width,
      height,
      dpi,
      "#ffffff"
    );
    // Convert the canvas pixels into a "Data URL" (a base64 encoded string of the image)
    const pngDataStream = drawingCanvas.toDataURL("image/png");
    const finalFilename = (filename || "chart") + ".png";
    // Trigger the browser download
    triggerDownload(pngDataStream, finalFilename);
  }

  async function downloadTiff(svgNode, width, height, dpi, filename) {
    const drawingCanvas = await svgToCanvas(
      svgNode,
      width,
      height,
      dpi,
      "#ffffff"
    );
    const finalFileName = (filename || "chart") + ".tif";

    // Check if the TIFF encoding library (UTIF.js) is available before attempting to use it.
    if (typeof UTIF !== "undefined") {
      const canvasContext = drawingCanvas.getContext("2d");
      const rawPixelData = canvasContext.getImageData( // Raw RGBA pixel data from the canvas
        0,
        0,
        drawingCanvas.width,
        drawingCanvas.height
      );
      const tiffBinaryBuffer = UTIF.encodeImage( // Encode the raw pixel data into a TIFF-firmatted binary bytes
        rawPixelData.data,
        drawingCanvas.width,
        drawingCanvas.height
      );
      // Wrap the binary bytes in a "Blob" (memory-file-like object) and create a temporary URL for it
      const tiffBlob = new Blob([tiffBinaryBuffer], { type: "image/tiff" });
      const blobUrl = URL.createObjectURL(tiffBlob);

      triggerDownload(blobUrl, finalFileName, true);
    } else {
      // Fallback: Library not available, so download as PNG but 
      // with a .tif extension (user can rename if needed)
      console.warn("UTIF.js not available. Downloading as PNG with .tif extension.");
      triggerDownload(drawingCanvas.toDataURL("image/png"), finalFileName);
    }
  }

  /* ---- Data Format Helpers ---- */

  /**
   * Convert columnar rawData {headers, columns} to row-based {headers, rows}.
   * Accepts both formats — returns row-based as-is.
   */
  function normalizeRawData(rawData) {
    if (!rawData || !rawData.headers) {
      return rawData;
    }
    if (rawData.rows) {
      return rawData;
    }
    if (!rawData.columns) {
      return rawData;
    }
    const nCols = rawData.columns.length;
    const nRows = nCols > 0 ? rawData.columns[0].length : 0;
    const rowBuffer = new Array(nRows);
    for (let rowIndex = 0; rowIndex < nRows; rowIndex++) {
      const singleRow = new Array(nCols);
      for (let colIndex = 0; colIndex < nCols; colIndex++) {
        singleRow[colIndex] = rawData.columns[colIndex][rowIndex];
      }
      rowBuffer[rowIndex] = singleRow;
    }
    return {
      headers: rawData.headers,
      rows: rowBuffer
    };
  }

  /**
  * Converts data from a columnar object {x: [], y: []} into a tidy array of objects [{x,y}, {x,y}].
  * In R terms: This is like converting a named list of vectors into a data.frame or tibble.
  * 
  * @param {Object|Array} pointData - The input points in either columnar or row-based format.
  * @returns {Object[]} An array of objects where each object represents a single data point.
  */
  function normalizePoints(pointData) {
    if (!pointData) {
      return [];
    }
    if (Array.isArray(pointData)) {
      return pointData;
    }
    // Extract the aesthetic names (e.g., ["y", "SubjectID"])
    const variableNames = Object.keys(pointData);
    if (variableNames.length === 0) {
      return [];
    }
    const n = pointData[variableNames[0]].length;
    const tidyDataArray = new Array(n);
    for (let rowIndex = 0; rowIndex < n; rowIndex++) {
      const observationObject = {};
      for (let variableIndex = 0; variableIndex < variableNames.length; variableIndex++) {
        const currentVariableName = variableNames[variableIndex];
        observationObject[currentVariableName] = pointData[currentVariableName][rowIndex];
      }
      tidyDataArray[rowIndex] = observationObject;
    }
    return tidyDataArray;
  }

  /* ---- Data Export Utilities (CSV / XLSX) ---- */

  /**
   * Serializes row-based data into a CSV string.
   * Equivalent to R's write.csv() with default settings
   * (comma separator, quoted fields if needed).
   * 
   * @param {Object} rawData - The row-based data to be serialized.
   * @param {string} [separator=','] - The field separator to use.
   * @returns {string} The CSV string representation of the data.
   */
  function rawDataToCSV(rawData, separator = ",") {
    if (!rawData || !rawData.headers || !rawData.rows) {
      return "";
    }
    const delimiter = separator || ",";
    const formatCellForCSV = function (cellValue) {
      const stringifiedValue = cellValue == null ? "" : String(cellValue);
      const containsDelimiter = stringifiedValue.indexOf(delimiter) >= 0;
      const containsQuote = stringifiedValue.indexOf('"') >= 0;
      const containsNewline = stringifiedValue.indexOf('\n') >= 0;
      if (containsDelimiter || containsQuote || containsNewline) {
        const escapedQuotes = stringifiedValue.replace(/"/g, '""');
        return '"' + escapedQuotes + '"';
      }
      return stringifiedValue;
    };
    const headerLine = rawData.headers.map(formatCellForCSV).join(delimiter);
    const dataLines = rawData.rows.map(row => {
      return row.map(formatCellForCSV).join(delimiter);
    });
    return [headerLine, ...dataLines].join("\n");
  }

  /**
   * Triggers a browser download of the data as a CSV file.
   * @param {*} rawData The row-based data to be downloaded.
   * @param {*} filename The name of the file to be downloaded.
   * @returns {void}
   */
  function downloadCSV(rawData, filename) {
    const csvContent = rawDataToCSV(rawData, ",");
    if (!csvContent) {
      return;
    }
    const byteOrderMark = "\uFEFF";
    const csvBlob = new Blob([byteOrderMark + csvContent], {
      type: "text/csv;charset=utf-8"
    });
    const downloadUrl = URL.createObjectURL(csvBlob);
    const finalFilename = (filename || "data") + ".csv";
    triggerDownload(downloadUrl, finalFilename, true);
  }

  /* ---- XLSX helpers (proper Office Open XML ZIP) ---- */
  const _xlsxCrcTable = (function () {
    const t = new Uint32Array(256);
    for (let n = 0; n < 256; n++) {
      let c = n;
      for (let k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
      t[n] = c;
    }
    return t;
  })();

  function _xlsxCrc32(data) {
    let crc = 0xFFFFFFFF;
    for (let i = 0; i < data.length; i++) crc = (crc >>> 8) ^ _xlsxCrcTable[(crc ^ data[i]) & 0xFF];
    return (crc ^ 0xFFFFFFFF) >>> 0;
  }

  function _xlsxColRef(i) {
    let r = "";
    i++;
    while (i > 0) { i--; r = String.fromCharCode(65 + (i % 26)) + r; i = Math.floor(i / 26); }
    return r;
  }

  function _xlsxCreateZip(files) {
    const enc = new TextEncoder();
    const locals = [], central = [];
    let offset = 0;
    files.forEach(function (f) {
      const nameB = enc.encode(f.name);
      const dataB = enc.encode(f.content);
      const crc = _xlsxCrc32(dataB);
      const lBuf = new ArrayBuffer(30 + nameB.length + dataB.length);
      const lv = new DataView(lBuf), lu = new Uint8Array(lBuf);
      lv.setUint32(0, 0x04034b50, true);
      lv.setUint16(4, 20, true);
      lv.setUint32(14, crc, true);
      lv.setUint32(18, dataB.length, true);
      lv.setUint32(22, dataB.length, true);
      lv.setUint16(26, nameB.length, true);
      lu.set(nameB, 30);
      lu.set(dataB, 30 + nameB.length);
      locals.push(lu);
      const cBuf = new ArrayBuffer(46 + nameB.length);
      const cv = new DataView(cBuf), cu = new Uint8Array(cBuf);
      cv.setUint32(0, 0x02014b50, true);
      cv.setUint16(4, 20, true);
      cv.setUint16(6, 20, true);
      cv.setUint32(16, crc, true);
      cv.setUint32(20, dataB.length, true);
      cv.setUint32(24, dataB.length, true);
      cv.setUint16(28, nameB.length, true);
      cv.setUint32(42, offset, true);
      cu.set(nameB, 46);
      central.push(cu);
      offset += lu.length;
    });
    const cdSize = central.reduce((a, b) => a + b.length, 0);
    const eocd = new ArrayBuffer(22);
    const ev = new DataView(eocd);
    ev.setUint32(0, 0x06054b50, true);
    ev.setUint16(8, files.length, true);
    ev.setUint16(10, files.length, true);
    ev.setUint32(12, cdSize, true);
    ev.setUint32(16, offset, true);
    const all = locals.concat(central, [new Uint8Array(eocd)]);
    const total = all.reduce((a, b) => a + b.length, 0);
    const out = new Uint8Array(total);
    let pos = 0;
    all.forEach(p => { out.set(p, pos); pos += p.length; });
    return out.buffer;
  }

  function downloadXLSX(rawData, filename) {
    if (!rawData || !rawData.headers || !rawData.rows) return;
    
    // Calculate approximate column widths (Max chars * 1.2)
    const colWidths = rawData.headers.map((header, colIndex) => {
      const maxChars = Math.max(
        String(header).length,
        ...rawData.rows.map(row => String(row[colIndex] == null ? "" : row[colIndex]).length)
      );
      return Math.min(80, maxChars * 1.2 + 2)
    });

    // Shared-strings pool
    const stringPool = [], stringMap = {};
    function getStringIndex(value) {
      const valueAsString = String(value);
      if (stringMap.hasOwnProperty(valueAsString)) {
        return stringMap[valueAsString];
      }
      const idx = stringPool.length;
      stringPool.push(valueAsString);
      stringMap[valueAsString] = idx;
      return idx;
    }
    rawData.headers.forEach(header => getStringIndex(header));

    // Build Worksheet XML
    let worksheetXml = [
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
      '<cols>',
    ];

    // Set column widths in the XML based on the calculated widths
    colWidths.forEach((calcWidth, ci) => {
      worksheetXml.push(`<col min="${ci+1}" max="${ci+1}" width="${calcWidth}" customWidth="1"/>`);
    });
    worksheetXml.push('</cols>', '<sheetData>');

    // Header Row (Style index 1 for bold font & centering)
    worksheetXml.push('<row r="1">');
    rawData.headers.forEach((header, ci) => {
      worksheetXml.push(`<c r="${_xlsxColRef(ci)}1" t="s" s="1"><v>${getStringIndex(header)}</v></c>`);
    });
    worksheetXml.push('</row>');

    // Data Rows
    rawData.rows.forEach((row, rowIndex) => {
      const excelRowNum = rowIndex + 2;
      worksheetXml.push(`<row r="${excelRowNum}">`);
      row.forEach((value, colIndex) => {
        if (value == null) return;
        const ref = _xlsxColRef(colIndex) + excelRowNum;
        if (typeof value === "number" && isFinite(value)) {
          worksheetXml.push(`<c r="${ref}" s="2"><v>${value}</v></c>`);
        } else {
          worksheetXml.push(`<c r="${ref}" t="s"><v>${getStringIndex(value)}</v></c>`);
        }
      });
      worksheetXml.push('</row>');
    });
    worksheetXml.push('</sheetData></worksheet>');

    // Shared strings XML
    const ssx = [
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"',
      ' count="' + stringPool.length + '" uniqueCount="' + stringPool.length + '">'
    ];
    stringPool.forEach(s => ssx.push('<si><t>' + escapeXml(s) + '</t></si>'));
    ssx.push('</sst>');

    const CT = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>' +
      '<Default Extension="xml" ContentType="application/xml"/>' +
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>' +
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' +
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>' +
      '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>' +
      '</Types>';

    const rootRels = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>' +
      '</Relationships>';

    const wb = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"' +
      ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">' +
      '<sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>';

    const wbRels = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">' +
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>' +
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>' +
      '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>' +
      '</Relationships>';

    // Update Styles (The Theme)
    const stylesXml = [
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
      '<numFmts count="1"><numFmt numFmtId="164" formatCode="0.00"/></numFmts>',
      '<fonts count="2">',
        '<font><sz val="11"/><name val="Calibri"/></font>', // Normal
        '<font><b/><sz val="11"/><name val="Calibri"/></font>', // Bold
      '</fonts>',
      '<fills count="2"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill></fills>',
      '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>',
      '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
      '<cellXfs count="3">',
        '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>', // Index 0: Default
        '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1" applyAlignment="1"><alignment horizontal="center"/></xf>', // Index 1: Header (Bold/Center)
        '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1" applyAlignment="1"><alignment horizontal="center"/></xf>', // Index 2: 2 Decimals
      '</cellXfs></styleSheet>'].join('');

    const zipBuf = _xlsxCreateZip([
      { name: "[Content_Types].xml", content: CT },
      { name: "_rels/.rels", content: rootRels },
      { name: "xl/workbook.xml", content: wb },
      { name: "xl/_rels/workbook.xml.rels", content: wbRels },
      { name: "xl/worksheets/sheet1.xml", content: worksheetXml.join("") },
      { name: "xl/styles.xml", content: stylesXml },
      { name: "xl/sharedStrings.xml", content: ssx.join("") }
    ]);
    const blob = new Blob([zipBuf], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
    triggerDownload(URL.createObjectURL(blob), (filename || "data") + ".xlsx", true);
  }

  function escapeXml(s) {
    return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&apos;');
  }

  /* ---- Toolbar Icon SVGs ---- */
  const ICON_DOWNLOAD = '<svg viewBox="0 0 16 16" width="13" height="13"><path d="M2 12h12v2H2zM8 2v7m0 0l-3-3m3 3l3-3" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  const ICON_FILE = '<svg viewBox="0 0 16 16" width="13" height="13"><path d="M4 1h5l4 4v9a1 1 0 01-1 1H4a1 1 0 01-1-1V2a1 1 0 011-1z" stroke="currentColor" stroke-width="1.2" fill="none"/><path d="M9 1v4h4" stroke="currentColor" stroke-width="1.2" fill="none" stroke-linejoin="round"/><path d="M5 9h6M5 11.5h4" stroke="currentColor" stroke-width="1" fill="none" stroke-linecap="round"/></svg>';
  const ICON_SCROLL = '<svg viewBox="0 0 16 16" width="13" height="13"><path d="M8 2v12M8 2l-3 3M8 2l3 3M8 14l-3-3M8 14l3-3" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>';
  const ICON_RESET = '<svg viewBox="0 0 16 16" width="13" height="13"><path d="M2 8a6 6 0 0111.3-2.8M14 2v3.2h-3.2M14 8a6 6 0 01-11.3 2.8M2 14v-3.2h3.2" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>';

  function toolbarBtnHtml(icon, label) {
    return '<span class="gd3-toolbar-btn-icon">' + icon + '</span><span class="gd3-toolbar-btn-label">' + label + '</span>';
  }

  /* ---- Build Download Toolbar ---- */
  function buildToolbar(containerElement, svgNode, getSvgDimensions, defaultFilename, rawData) {
    // Normalise columnar → row-based on demand (lazy, only when user exports)
    let normalisedRaw = null;
    function getRawData() {
      if (!normalisedRaw) {
        normalisedRaw = normalizeRawData(rawData);
      }
      return normalisedRaw;
    }
    const existing = containerElement.querySelector(".gd3-toolbar");
    if (existing) {
      existing.remove();
    }

    const toolbar = document.createElement("div");
    toolbar.className = "gd3-toolbar";

    // DPI selector (no label — DPI suffix in items)
    const dpiSelect = document.createElement("select");
    dpiSelect.className = "gd3-toolbar-select";
    [96, 150, 300, 600, 900, 1200].forEach(v => {
      const opt = document.createElement("option");
      opt.value = v;
      opt.textContent = v + " dpi";
      if (v === 300) opt.selected = true;
      dpiSelect.appendChild(opt);
    });
    toolbar.appendChild(dpiSelect);

    const pngBtn = document.createElement("button");
    pngBtn.className = "gd3-toolbar-btn";
    pngBtn.innerHTML = toolbarBtnHtml(ICON_DOWNLOAD, '.PNG');
    pngBtn.onclick = function () {
      const dims = getSvgDimensions();
      downloadPng(svgNode, dims.width, dims.height, parseInt(dpiSelect.value), defaultFilename);
    };
    toolbar.appendChild(pngBtn);

    const tifBtn = document.createElement("button");
    tifBtn.className = "gd3-toolbar-btn";
    tifBtn.innerHTML = toolbarBtnHtml(ICON_DOWNLOAD, '.TIF');
    tifBtn.onclick = function () {
      const dims = getSvgDimensions();
      downloadTiff(svgNode, dims.width, dims.height, parseInt(dpiSelect.value), defaultFilename);
    };
    toolbar.appendChild(tifBtn);

    // Data export buttons (CSV / XLSX) — only shown when rawData is provided
    const hasData = rawData && rawData.headers &&
      (rawData.rows ? rawData.rows.length > 0 :
       rawData.columns ? rawData.columns.length > 0 && rawData.columns[0].length > 0 : false);
    if (hasData) {
      // Separator
      const sep = document.createElement("span");
      sep.className = "gd3-toolbar-separator";
      toolbar.appendChild(sep);

      // CSV button
      const csvBtn = document.createElement("button");
      csvBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-data";
      csvBtn.innerHTML = toolbarBtnHtml(ICON_FILE, '.CSV');
      csvBtn.onclick = function () { downloadCSV(getRawData(), defaultFilename); };
      toolbar.appendChild(csvBtn);

      // XLSX button
      const xlsxBtn = document.createElement("button");
      xlsxBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-data";
      xlsxBtn.innerHTML = toolbarBtnHtml(ICON_FILE, '.XLSX');
      xlsxBtn.onclick = function () { downloadXLSX(getRawData(), defaultFilename); };
      toolbar.appendChild(xlsxBtn);
    }

    // Scroll toggle button (vertical scroll on/off)
    const scrollBtn = document.createElement("button");
    scrollBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-scroll";
    const vScrollEnabled = !containerElement.classList.contains("no-vscroll");
    if (vScrollEnabled) scrollBtn.classList.add("active");
    scrollBtn.innerHTML = toolbarBtnHtml(ICON_SCROLL, 'V-Scroll');
    scrollBtn.onclick = function () {
      containerElement.classList.toggle("no-vscroll");
      scrollBtn.classList.toggle("active");
    };
    toolbar.appendChild(scrollBtn);

    // Reset zoom button (white variant, pushed right)
    const resetBtn = document.createElement("button");
    resetBtn.className = "gd3-toolbar-btn gd3-toolbar-btn-reset";
    resetBtn.innerHTML = toolbarBtnHtml(ICON_RESET, 'Reset');
    toolbar.appendChild(resetBtn);

    containerElement.insertBefore(toolbar, containerElement.firstChild);

    // Default to no vertical scroll (height matches parent)
    if (!containerElement.classList.contains("no-vscroll")) {
      containerElement.classList.add("no-vscroll");
      scrollBtn.classList.remove("active");
    }

    return { dpiSelect, resetBtn };
  }


  /* ---- Utilities ---- */
  function debounce(func, wait) {
    let timeout;
    return function (...args) {
      clearTimeout(timeout);
      timeout = setTimeout(() => func.apply(this, args), wait);
    };
  }

  /* ================================================================== */
  /* ---- PLOT INSTANCE FACTORY ---- */
  /* ================================================================== */
  function createPlotInstance(containerId) {
    const containerElement = document.getElementById(containerId);
    
    if (!containerElement) {
      return {
        update: () => {},
        destroy: () => {}
      };
    }

    /* ---- Event Handlers ---- */
    const rootSelection = d3.select(containerElement);
    containerElement.tabIndex = 0;
    const handleMouseWheel = function (event) {
      if (event.ctrlKey) {
        event.preventDefault();
        event.stopPropagation();
      }
    };

    // Ctrl key tracking for zoom cursor and pointer-events on zoom rects
    let isControlKeyHeld = false;
    function onCtrlKeyDown(event) {
      if (event.key === "Control" && !isControlKeyHeld) {
        isControlKeyHeld = true;
        rootSelection.selectAll(".gd3-zoom-rect")
          .classed("gd3-ctrl-active", true);
      }
    }
    function onCtrlKeyUp(event) {
      if (event.key === "Control") {
        isControlKeyHeld = false;
        rootSelection.selectAll(".gd3-zoom-rect")
          .classed("gd3-ctrl-active", false);
      }
    }
    function onCtrlBlur() {
      if (isControlKeyHeld) {
        isControlKeyHeld = false;
        rootSelection.selectAll(".gd3-zoom-rect")
          .classed("gd3-ctrl-active", false);
      }
    }

    // Attach event listeners
    containerElement.addEventListener("wheel", handleMouseWheel, { passive: false });
    document.addEventListener("keydown", onCtrlKeyDown);
    document.addEventListener("keyup", onCtrlKeyUp);
    window.addEventListener("blur", onCtrlBlur);

    /* ---- DOM Construction ---- */

    // Base SVG
    const baseSvg = rootSelection.append("svg")
      .attr("class", "gd3-svg")
      .style("overflow", "visible")
      .attr("xmlns", "http://www.w3.org/2000/svg");

    // Basic Tooltip Setup
    const tooltipId = `gd3-tip-${safeId(containerId)}`;

    // Remove any existing tooltip with the same ID (in case of re-render)
    //  before creating a new one
    d3.select(`#${CSS.escape(tooltipId)}`).remove();

    const tooltipSelection = d3.select("body")
      .append("div")
      .attr("id", tooltipId)
      .attr("class", "gd3-tooltip")
      .style("opacity", 0);

    // Preserve the CSS-specified height before no-vscroll overrides it
    const specifiedHeight = parseInt(containerElement.style.height) || 500;

    /* ---- State Management ---- */
    const state = {
      lastPayload: null,
      resizeObserver: null,
      zoomBehavior: null,
      zoomTransform: d3.zoomIdentity
    };

    // Debounced Resize Observer
    state.resizeObserver = new ResizeObserver(
      debounce(() => {
        if (state.lastPayload) {
          update(state.lastPayload);
        }
      }, 100)
    );

    state.resizeObserver.observe(containerElement);

    /* ---- Lifecycle Functions ---- */

    function destroy() {
      containerElement.removeEventListener("wheel", handleMouseWheel, { passive: false });
      document.removeEventListener("keydown", onCtrlKeyDown);
      document.removeEventListener("keyup", onCtrlKeyUp);
      window.removeEventListener("blur", onCtrlBlur);
      if (state.resizeObserver) {
        state.resizeObserver.disconnect();
      }
      tooltipSelection.remove();
      rootSelection.selectAll("*").remove();
    }

    /* ---- Tooltip UI Logic ---- */
    
    function positionTooltip(clientX, clientY) {
      const tooltipNode = tooltipSelection.node();
      if (!tooltipNode) return;

      const tooltipWidth = tooltipNode.offsetWidth || 200;
      const tooltipHeight = tooltipNode.offsetHeight || 40;
      const padding = 8; // Minimum distance from cursor to tooltip
      const offset = 14; // Additional offset to prevent overlap
      const offsetoffset = 4; // Small nudge to better align with cursor

      let targetX = clientX + offset;
      let targetY = clientY - offset + offsetoffset;
      
      // Boundary Check: Right Edge
      if (targetX + tooltipWidth > window.innerWidth - padding) {
        targetX = clientX - tooltipWidth - offset;
      }
      // Boundary Check: Bottom Edge
      if (targetY + tooltipHeight > window.innerHeight - padding) {
        targetY = clientY - tooltipHeight - offset;
      }
      // Boundary Checks: Left and Top Edges
      if (targetX < padding) targetX = padding;
      if (targetY < padding) targetY = padding;
      
      tooltipSelection
        .style("left", targetX + "px")
        .style("top", targetY + "px");
    }
    
    function showTooltip(event, html) {
      tooltipSelection
        .html(html)
        .style("opacity", 1);
      
      positionTooltip(event.clientX, event.clientY);
    }

    function moveTooltip(event) {
      positionTooltip(event.clientX, event.clientY);
    }

    function hideTooltip() {
      tooltipSelection.style("opacity", 0);
    }

    /* ---- Render Dispatcher ---- */

    function update(payload) {
      state.lastPayload = payload;

      if (!payload || !payload.plot_type) return;
      
      // Clear previous render
      baseSvg.selectAll("*").remove();

      // Reset dimensions to prevent ResizeObserver feedback loops
      baseSvg
        .attr("width", null)
        .attr("height", null)
        .attr("viewBox", null);

      const plotType = payload.plot_type;
      const data = payload.data || {};
      const params = payload.params || {};

      switch (plotType) {
        case "subject_cvi":
          renderSubjectCVI(baseSvg, containerElement, data, params);
          break;
        case "cvi_vs_concentration":
          renderCVIvsConcentration(baseSvg, containerElement, data, params);
          break;
        case "cvi_vs_rcv":
          renderCVIvsRCV(baseSvg, containerElement, data, params);
          break;
        case "trace":
          renderTracePlots(baseSvg, containerElement, data, params);
          break;
        case "posterior":
          renderPosteriorDensity(baseSvg, containerElement, data, params);
          break;
        case "prior":
          renderPriorDensity(baseSvg, containerElement, data, params);
          break;
        case "exploration_scatter":
          renderExplorationScatter(baseSvg, containerElement, data, params);
          break;
        case "descriptive_dotplot":
          renderDescriptiveDotplot(baseSvg, containerElement, data, params);
          break;
        case "anova_components":
          renderAnovaComponents(baseSvg, containerElement, data, params);
          break;
        default:
          baseSvg.append("text")
            .attr("x", 20)
            .attr("y", 30)
            .text("Never heard of this plot type: " + plotType);
      }
    }

    /* ================================================================== */
    /* 1. SUBJECT-SPECIFIC CVI (dot-and-whisker, horizontal)              */
    /* ================================================================== */
    function renderSubjectCVI(svg, containerElement, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;

      const cviPredictionBand = data.pop_band || {};
      const title = params.title || "Subject-Specific CVs with 95% Credible Intervals";
      const subtitle = params.subtitle || "";

      const nSubjects = subjects.length;
      const rowH = Math.max(20, Math.min(36, 600 / nSubjects));
      const margin = { top: 70, right: 30, bottom: 60, left: 120 };
      const { plotW, plotH, totalW, totalH } = getDimensions(
        containerElement,
        margin,
        { plotH: nSubjects * rowH }
      );

      svg
        .attr("width", totalW)
        .attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      // Shared defs & gradients
      const idPrefix = "scvi-" + safeId(containerId);
      const gradIds = SharedGradients.apply(svg, idPrefix);
      const clipId = SharedGradients.addClipPath(svg, idPrefix + "-clip", plotW, plotH);

      // Title (centered 24px from top)
      svg.append("text")
        .attr("class", "gd3-title")
        .attr("x", totalW / 2)
        .attr("y", 24).text(title);
      
      // Subtitle (optional, centered 44px from top)
      if (subtitle) {
        svg.append("text")
          .attr("class", "gd3-subtitle")
          .attr("x", totalW / 2)
          .attr("y", 44).text(subtitle);
      }

      // Setup Base Group
      const chartArea = svg
        .append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const allXValues = subjects.flatMap(d => [d.lwr, d.upr, d.median]);
      if (cviPredictionBand.lwr != null) {
        allXValues.push(
          cviPredictionBand.lwr,
          cviPredictionBand.upr,
          cviPredictionBand.median
        );
      }
      const xPaddedDataRange = niceExtent(allXValues, 0.05, [0, 20]);

      const x = d3
        .scaleLinear()
        .domain(xPaddedDataRange)
        .range([0, plotW]);
      const y = d3
        .scaleBand()
        .domain(subjects.map(d => d.label))
        .range([plotH, 0])
        .padding(0.25);

      // Pop band (green shading with gradient)
      if (cviPredictionBand.lwr != null) {
        chartArea
          .append("rect")
          .attr("class", "gd3-pop-band")
          .attr("x", x(cviPredictionBand.lwr))
          .attr("width", x(cviPredictionBand.upr) - x(cviPredictionBand.lwr))
          .attr("y", 0)
          .attr("height", plotH)
          .attr("fill", `url(#${gradIds.popBandH})`);
        [
          cviPredictionBand.lwr,
          cviPredictionBand.median,
          cviPredictionBand.upr
        ].forEach(value => {
          chartArea
            .append("line")
            .attr("class", "gd3-pop-line")
            .attr("x1", x(value))
            .attr("x2", x(value))
            .attr("y1", 0)
            .attr("y2", plotH);
        });
      }

      // Clip group for zoomed content
      const mainContentGroup = chartArea
        .append("g")
        .attr("clip-path", `url(#${clipId})`);

      // Error bars + points
      const subjectSpecificContent = mainContentGroup
        .selectAll("g.gd3-subject")
        .data(subjects)
        .join("g")
        .attr("class", "gd3-subject");

      subjectSpecificContent
        .append("line")
        .attr("class", "gd3-errbar")
        .attr("x1", d => x(d.lwr)).attr("x2", d => x(d.upr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2);

      // Whisker caps
      const whiskerCapHeight = Math.min(y.bandwidth() * 0.3, 6);
      subjectSpecificContent
        .append("line")
        .attr("class", "gd3-errbar-cap")
        .attr("x1", d => x(d.lwr))
        .attr("x2", d => x(d.lwr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2 - whiskerCapHeight)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2 + whiskerCapHeight);
      subjectSpecificContent
        .append("line")
        .attr("class", "gd3-errbar-cap")
        .attr("x1", d => x(d.upr))
        .attr("x2", d => x(d.upr))
        .attr("y1", d => y(d.label) + y.bandwidth() / 2 - whiskerCapHeight)
        .attr("y2", d => y(d.label) + y.bandwidth() / 2 + whiskerCapHeight);

      subjectSpecificContent
        .append("circle")
        .attr("class", "gd3-point")
        .attr("cx", d => x(d.median))
        .attr("cy", d => y(d.label) + y.bandwidth() / 2)
        .attr("r", 5.5)
        .attr("fill", `url(#${gradIds.glossyPoint})`)
        .attr("stroke", COLORS.purpleDark)
        .attr("stroke-width", 1.2);

      // Tooltip helper for subject data
      function subjectTipHtml(d) {
        return `<strong>Subject ID: ${d.label}</strong><br>
          <span class="gd3-tip-muted">Median CV<sub>P(i)</sub>:</span> ${formatPercentage(d.median)}<br>
          <span class="gd3-tip-muted">95% CrI:</span> ${formatPercentage(d.lwr)} – ${formatPercentage(d.upr)}` +
          (d.mean != null ? `<br><span class="gd3-tip-muted">Mean:</span> ${formatPercentage(d.mean)}` : "");
      }

      // Tooltip on error bars
      subjectSpecificContent.select(".gd3-errbar")
        .style("pointer-events", "visibleStroke")
        .on("pointerenter", function (event, d) {
          d3.select(this).attr("stroke-width", 3.5);
          showTooltip(event, subjectTipHtml(d));
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () {
          d3.select(this).attr("stroke-width", null); hideTooltip();
        });

      // Tooltip on error bar caps
      subjectSpecificContent.selectAll(".gd3-errbar-cap")
        .style("pointer-events", "visibleStroke")
        .on("pointerenter", function (event, d) {
          showTooltip(event, subjectTipHtml(d));
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () { hideTooltip(); });

      // Tooltip on points
      subjectSpecificContent.select("circle")
        .on("pointerenter", function (event, d) {
          d3.select(this).attr("r", 7.5).attr("stroke-width", 2);
          showTooltip(event, subjectTipHtml(d));
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () {
          d3.select(this).attr("r", 5.5).attr("stroke-width", 1.2); hideTooltip();
        });

      // Axes
      const xAxisG = chartArea.append("g")
        .attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(8).tickFormat(d => formatPercentage(d)));
      const yAxisG = chartArea.append("g")
        .attr("class", "gd3-axis gd3-y-axis").call(d3.axisLeft(y));

      // Axis labels
      chartArea.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45)
        .text("Within-Individual CV (CVₚ₍ᵢ₎, %)");
      chartArea.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -100).text("Subject");

      // Zoom
      const zoom = d3.zoom()
        .scaleExtent([1, 20])
        .wheelDelta(event => -event.deltaY * 0.001)
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const newX = event.transform.rescaleX(x);
          xAxisG.call(d3.axisBottom(newX).ticks(8).tickFormat(d => formatPercentage(d)));
          // Update pop band
          if (cviPredictionBand.lwr != null) {
            chartArea.select(".gd3-pop-band")
              .attr("x", newX(cviPredictionBand.lwr))
              .attr("width", newX(cviPredictionBand.upr) - newX(cviPredictionBand.lwr));
            chartArea.selectAll(".gd3-pop-line").each(function (_, i) {
              const v = [cviPredictionBand.lwr, cviPredictionBand.median, cviPredictionBand.upr][i];
              d3.select(this).attr("x1", newX(v)).attr("x2", newX(v));
            });
          }
          subjectSpecificContent.select(".gd3-errbar").attr("x1", d => newX(d.lwr)).attr("x2", d => newX(d.upr));
          // Re-draw caps at zoomed positions
          subjectSpecificContent.selectAll(".gd3-errbar-cap").remove();
          subjectSpecificContent.append("line").attr("class", "gd3-errbar-cap")
            .attr("x1", d => newX(d.lwr)).attr("x2", d => newX(d.lwr))
            .attr("y1", d => y(d.label) + y.bandwidth() / 2 - whiskerCapHeight)
            .attr("y2", d => y(d.label) + y.bandwidth() / 2 + whiskerCapHeight);
          subjectSpecificContent.append("line").attr("class", "gd3-errbar-cap")
            .attr("x1", d => newX(d.upr)).attr("x2", d => newX(d.upr))
            .attr("y1", d => y(d.label) + y.bandwidth() / 2 - whiskerCapHeight)
            .attr("y2", d => y(d.label) + y.bandwidth() / 2 + whiskerCapHeight);
          // Re-attach tooltips on re-drawn caps
          subjectSpecificContent.selectAll(".gd3-errbar-cap")
            .style("pointer-events", "visibleStroke")
            .on("pointerenter", function (event, d) { showTooltip(event, subjectTipHtml(d)); })
            .on("pointermove", moveTooltip)
            .on("pointerleave", function () { hideTooltip(); });
          subjectSpecificContent.select("circle").attr("cx", d => newX(d.median));
        });
      zoom.filter(event => event.ctrlKey || event.type === "dblclick");

      const zoomRect = chartArea.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom);
      zoomRect.on("dblclick.zoom", null);
      svg.on("dblclick.gd3reset", function () {
        zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      });

      // Toolbar
      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "subject_cvi_plot", data.raw_data);
      resetBtn.onclick = function () {
        zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      };
    }

    /* ================================================================== */
    /* 2. CVI vs CONCENTRATION                                            */
    /* ================================================================== */
    function renderCVIvsConcentration(svg, containerElement, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;
      const cviPredictionBand = data.pop_band || {};
      const title = params.title || "Subject-Specific CVs vs. Concentration";
      const subtitle = params.subtitle || "";

      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const { plotW, plotH, totalW, totalH } = getDimensions(containerElement, margin, {
        plotH: Math.max(350, specifiedHeight - margin.top - margin.bottom)
      });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      // Shared defs & gradients
      const idPrefix = "cvc-" + safeId(containerId);
      const gradIds = SharedGradients.apply(svg, idPrefix);
      const clipId = SharedGradients.addClipPath(svg, idPrefix + "-clip", plotW, plotH);

      // Title
      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const xVals = subjects.map(d => d.concentration);
      const yVals = subjects.flatMap(d => [d.lwr, d.upr, d.median]);
      if (cviPredictionBand.lwr != null) yVals.push(cviPredictionBand.lwr, cviPredictionBand.upr);

      const x = d3.scaleLinear().domain(niceExtent(xVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      // Pop band (vertical, green gradient)
      if (cviPredictionBand.lwr != null) {
        g.append("rect").attr("class", "gd3-pop-band")
          .attr("x", 0).attr("width", plotW)
          .attr("y", y(cviPredictionBand.upr)).attr("height", y(cviPredictionBand.lwr) - y(cviPredictionBand.upr))
          .attr("fill", `url(#${gradIds.popBandV})`);
        [cviPredictionBand.lwr, cviPredictionBand.median, cviPredictionBand.upr].forEach(v => {
          g.append("line").attr("class", "gd3-pop-line")
            .attr("x1", 0).attr("x2", plotW).attr("y1", y(v)).attr("y2", y(v));
        });
      }

      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Error bars (vertical)
      const ptG = plotArea.selectAll("g.gd3-pt").data(subjects).join("g").attr("class", "gd3-pt");
      ptG.append("line").attr("class", "gd3-errbar")
        .attr("x1", d => x(d.concentration)).attr("x2", d => x(d.concentration))
        .attr("y1", d => y(d.lwr)).attr("y2", d => y(d.upr));

      ptG.append("circle").attr("class", "gd3-point")
        .attr("cx", d => x(d.concentration)).attr("cy", d => y(d.median))
        .attr("r", 4.5).attr("fill", `url(#${gradIds.glossyPoint})`)
        .attr("stroke", COLORS.purpleDark).attr("stroke-width", 1);

      // Tooltip helper
      function concTipHtml(d) {
        return `<strong>${d.label}</strong><br>
            <span class="gd3-tip-muted">Concentration:</span> ${formatNumber(d.concentration)}<br>
            <span class="gd3-tip-muted">CV<sub>P(i)</sub>:</span> ${formatPercentage(d.median)}<br>
            <span class="gd3-tip-muted">95% CrI:</span> ${formatPercentage(d.lwr)} – ${formatPercentage(d.upr)}`;
      }

      // Tooltip on error bars
      ptG.select(".gd3-errbar")
        .style("pointer-events", "visibleStroke")
        .on("pointerenter", function (event, d) {
          d3.select(this).attr("stroke-width", 4.5);
          showTooltip(event, concTipHtml(d));
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () {
          d3.select(this).attr("stroke-width", null); hideTooltip();
        });

      // Tooltip on points
      ptG.select("circle")
        .on("pointerenter", function (event, d) {
          d3.select(this).attr("r", 6.5).attr("stroke-width", 2);
          showTooltip(event, concTipHtml(d));
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () {
          d3.select(this).attr("r", 4.5).attr("stroke-width", 1); hideTooltip();
        });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(8));
      const yAxisG = g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(8).tickFormat(d => formatPercentage(d)));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Concentration");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("Within-Individual CV (CVₚ₍ᵢ₎, %)");

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .wheelDelta(event => -event.deltaY * 0.001)
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(8));
          yAxisG.call(d3.axisLeft(ny).ticks(8).tickFormat(d => formatPercentage(d)));
          if (cviPredictionBand.lwr != null) {
            g.select(".gd3-pop-band")
              .attr("y", ny(cviPredictionBand.upr))
              .attr("height", ny(cviPredictionBand.lwr) - ny(cviPredictionBand.upr));
            g.selectAll(".gd3-pop-line").each(function (_, i) {
              const v = [cviPredictionBand.lwr, cviPredictionBand.median, cviPredictionBand.upr][i];
              d3.select(this).attr("y1", ny(v)).attr("y2", ny(v));
            });
          }
          ptG.select(".gd3-errbar")
            .attr("x1", d => nx(d.concentration)).attr("x2", d => nx(d.concentration))
            .attr("y1", d => ny(d.lwr)).attr("y2", d => ny(d.upr));
          ptG.select("circle")
            .attr("cx", d => nx(d.concentration)).attr("cy", d => ny(d.median));
        });
      zoom.filter(event => event.ctrlKey || event.type === "dblclick");

      const zoomRect = g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom);
      zoomRect.on("dblclick.zoom", null);
      svg.on("dblclick.gd3reset", function () {
        zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      });

      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "cvi_vs_concentration", data.raw_data);
      resetBtn.onclick = () => zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 3. CVI vs RCV                                                      */
    /* ================================================================== */
    function renderCVIvsRCV(svg, containerElement, data, params) {
      const subjects = data.subjects || [];
      if (subjects.length === 0) return;
      const popRCV = data.pop_rcv || {};
      const title = params.title || "Subject-Specific RCVs with Uncertainty";
      const subtitle = params.subtitle || "Incorporated from CV Credible Intervals";

      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const { plotW, plotH, totalW, totalH } = getDimensions(containerElement, margin, {
        plotH: Math.max(350, specifiedHeight - margin.top - margin.bottom)
      });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      // Shared defs & gradients
      const idPrefix = "rcv-" + safeId(containerId);
      const gradIds = SharedGradients.apply(svg, idPrefix);
      const clipId = SharedGradients.addClipPath(svg, idPrefix + "-clip", plotW, plotH);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const xVals = subjects.map(d => d.cv_median);
      const yVals = subjects.flatMap(d => [d.upper_rcv_lwr, d.upper_rcv_upr, d.lower_rcv_lwr, d.lower_rcv_upr]);
      const x = d3.scaleLinear().domain(niceExtent(xVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      const plotArea = g.append("g").attr("clip-path", `url(#${clipId})`);

      // Sort by cv_median for ribbons
      const sorted = subjects.slice().sort((a, b) => a.cv_median - b.cv_median);

      // Upper RCV ribbon
      const areaUpper = d3.area()
        .x(d => x(d.cv_median))
        .y0(d => y(d.upper_rcv_lwr))
        .y1(d => y(d.upper_rcv_upr));
      plotArea.append("path").datum(sorted).attr("class", "gd3-ribbon")
        .attr("d", areaUpper).attr("fill", `url(#${gradIds.ribbonV})`);

      // Lower RCV ribbon
      const areaLower = d3.area()
        .x(d => x(d.cv_median))
        .y0(d => y(d.lower_rcv_lwr))
        .y1(d => y(d.lower_rcv_upr));
      plotArea.append("path").datum(sorted).attr("class", "gd3-ribbon")
        .attr("d", areaLower).attr("fill", `url(#${gradIds.ribbonV})`);

      // Median RCV lines
      const lineUpper = d3.line().x(d => x(d.cv_median)).y(d => y(d.upper_rcv_median));
      const lineLower = d3.line().x(d => x(d.cv_median)).y(d => y(d.lower_rcv_median));
      plotArea.append("path").datum(sorted).attr("d", lineUpper)
        .attr("fill", "none").attr("stroke", COLORS.black).attr("stroke-width", 1.5);
      plotArea.append("path").datum(sorted).attr("d", lineLower)
        .attr("fill", "none").attr("stroke", COLORS.black).attr("stroke-width", 1.5);

      // Population RCV lines
      if (popRCV.upper != null) {
        g.append("line").attr("class", "gd3-ref-line")
          .attr("x1", 0).attr("x2", plotW).attr("y1", y(popRCV.upper)).attr("y2", y(popRCV.upper));
        g.append("line").attr("class", "gd3-ref-line")
          .attr("x1", 0).attr("x2", plotW).attr("y1", y(popRCV.lower)).attr("y2", y(popRCV.lower));
      }

      // Points
      const ptG = plotArea.selectAll("g.gd3-rcv-pt").data(sorted).join("g").attr("class", "gd3-rcv-pt");
      ptG.append("circle").attr("cx", d => x(d.cv_median)).attr("cy", d => y(d.upper_rcv_median))
        .attr("r", 4).attr("fill", `url(#${gradIds.glossyPoint})`).attr("stroke", COLORS.purpleDark).attr("stroke-width", 1);
      ptG.append("circle").attr("cx", d => x(d.cv_median)).attr("cy", d => y(d.lower_rcv_median))
        .attr("r", 4).attr("fill", `url(#${gradIds.glossyPoint})`).attr("stroke", COLORS.purpleDark).attr("stroke-width", 1);

      // Tooltip on points
      ptG.selectAll("circle")
        .attr("class", "gd3-point")
        .on("pointerenter", function (event, d) {
          d3.select(this).attr("r", 6).attr("stroke-width", 2);
          const isFirst = this === this.parentNode.firstElementChild;
          showTooltip(event, `<strong>${d.label}</strong><br>
            <span class="gd3-tip-muted">CV<sub>P(i)</sub>:</span> ${formatPercentage(d.cv_median)}<br>
            <span class="gd3-tip-muted">${isFirst ? "Upper" : "Lower"} RCV:</span> ${formatPercentage(isFirst ? d.upper_rcv_median : d.lower_rcv_median)}<br>
            <span class="gd3-tip-muted">95% CrI:</span> ${formatPercentage(isFirst ? d.upper_rcv_lwr : d.lower_rcv_lwr)} – ${formatPercentage(isFirst ? d.upper_rcv_upr : d.lower_rcv_upr)}`);
        })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () { d3.select(this).attr("r", 4).attr("stroke-width", 1); hideTooltip(); });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`).call(d3.axisBottom(x).ticks(10).tickFormat(d => formatPercentage(d)));
      const yAxisG = g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(10).tickFormat(d => formatPercentage(d)));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Within-Individual CV (CVₚ₍ᵢ₎, %)");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("RCV (%)");

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .wheelDelta(event => -event.deltaY * 0.001)
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(10).tickFormat(d => formatPercentage(d)));
          yAxisG.call(d3.axisLeft(ny).ticks(10).tickFormat(d => formatPercentage(d)));
          const au = d3.area().x(d => nx(d.cv_median)).y0(d => ny(d.upper_rcv_lwr)).y1(d => ny(d.upper_rcv_upr));
          const al = d3.area().x(d => nx(d.cv_median)).y0(d => ny(d.lower_rcv_lwr)).y1(d => ny(d.lower_rcv_upr));
          plotArea.selectAll(".gd3-ribbon").each(function (_, i) {
            d3.select(this).attr("d", i === 0 ? au(sorted) : al(sorted));
          });
          const lu = d3.line().x(d => nx(d.cv_median)).y(d => ny(d.upper_rcv_median));
          const ll = d3.line().x(d => nx(d.cv_median)).y(d => ny(d.lower_rcv_median));
          plotArea.selectAll("path:not(.gd3-ribbon)").each(function (_, i) {
            d3.select(this).attr("d", i === 0 ? lu(sorted) : ll(sorted));
          });
          ptG.selectAll("circle").each(function (d) {
            const el = d3.select(this);
            el.attr("cx", nx(d.cv_median));
            // Determine if upper or lower by checking index within parent
            const isFirst = this === el.node().parentNode.firstElementChild;
            el.attr("cy", isFirst ? ny(d.upper_rcv_median) : ny(d.lower_rcv_median));
          });
          if (popRCV.upper != null) {
            g.selectAll(".gd3-ref-line").each(function (_, i) {
              const v = i === 0 ? popRCV.upper : popRCV.lower;
              d3.select(this).attr("y1", ny(v)).attr("y2", ny(v));
            });
          }
        });
      zoom.filter(event => event.ctrlKey || event.type === "dblclick");

      const zoomRect = g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom);
      zoomRect.on("dblclick.zoom", null);
      svg.on("dblclick.gd3reset", function () {
        zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      });

      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "cvi_vs_rcv", data.raw_data);
      resetBtn.onclick = () => zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 4. TRACE PLOTS (faceted small multiples)                           */
    /* ================================================================== */
    function renderTracePlots(svg, containerElement, data, params) {
      const panels = data.panels || [];
      if (panels.length === 0) return;
      const title = params.title || "Trace Plots";
      const subtitle = params.subtitle || "";

      const nCols = params.ncols || 3;
      const nRows = Math.ceil(panels.length / nCols);
      const gap = 50;
      const outerMargin = { top: 70, right: 60, bottom: 20, left: 10 };
      const panelMargin = { top: 30, right: 10, bottom: 25, left: 55 };

      const availW = Math.max(600, containerElement.clientWidth || 800) - outerMargin.left - outerMargin.right;
      const panelW = (availW - gap * (nCols - 1)) / nCols;
      const panelH = Math.max(140, Math.min(200, panelW * 0.65));

      const { totalW, totalH } = getDimensions(containerElement, outerMargin, {
        totalW: outerMargin.left + nCols * panelW + (nCols - 1) * gap + outerMargin.right,
        totalH: outerMargin.top  + nRows * panelH + (nRows - 1) * gap + outerMargin.bottom
      });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      // Collect unique chains for legend
      const allChains = [...new Set(panels.flatMap(p => (p.traces || []).map(t => t.chain)))].sort();

      // Draw legend on right side
      const legendX = totalW - outerMargin.right + 10;
      const legendG = svg.append("g").attr("transform", `translate(${legendX}, ${outerMargin.top})`);
      legendG.append("text").attr("class", "gd3-legend-title").attr("y", 0).text("Chain");
      allChains.forEach((ch, i) => {
        legendG.append("rect").attr("x", 0).attr("y", 12 + i * 18).attr("width", 12).attr("height", 4)
          .attr("fill", CHAIN_COLORS[i % CHAIN_COLORS.length]);
        legendG.append("text").attr("class", "gd3-legend-text").attr("x", 16).attr("y", 17 + i * 18)
          .text("Chain " + ch);
      });

      panels.forEach((panel, idx) => {
        const col = idx % nCols;
        const row = Math.floor(idx / nCols);
        const px = outerMargin.left + col * (panelW + gap);
        const py = outerMargin.top + row * (panelH + gap);
        const innerW = panelW - panelMargin.left - panelMargin.right;
        const innerH = panelH - panelMargin.top - panelMargin.bottom;

        const panelG = svg.append("g").attr("transform", `translate(${px},${py})`);

        // Strip title
        const stripH = 22;
        panelG.append("rect").attr("class", "gd3-strip-bg")
          .attr("x", 0).attr("y", 0).attr("width", panelW).attr("height", stripH).attr("rx", 4);
        panelG.append("text")
          .attr("class", "gd3-strip-text")
          .attr("x", panelW / 2).attr("y", stripH / 2 + 1).text(panel.label || panel.parameter);

        const ig = panelG.append("g")
          .attr("transform", `translate(${panelMargin.left},${panelMargin.top})`);

        // Clip
        const clipId = "clip-trace-" + safeId(containerId) + "-" + idx;
        ig.append("defs").append("clipPath").attr("id", clipId).append("rect")
          .attr("width", innerW).attr("height", innerH);

        const traces = panel.traces || [];
        const allIter = traces.map(d => d.iteration);
        const allVal = traces.map(d => d.value);
        const iterExtent = d3.extent(allIter);
        const valExtent = niceExtent(allVal, 0.04, [0, 1]);

        const x = d3.scaleLinear().domain(iterExtent).range([0, innerW]);
        const y = d3.scaleLinear().domain(valExtent).range([innerH, 0]);

        // Group traces by chain
        const byChain = d3.group(traces, d => d.chain);
        const plotArea = ig.append("g").attr("clip-path", `url(#${clipId})`);

        byChain.forEach((chainData, chainId) => {
          const sorted = chainData.slice().sort((a, b) => a.iteration - b.iteration);
          const ci = allChains.indexOf(chainId);
          const color = CHAIN_COLORS[ci % CHAIN_COLORS.length];

          // Raw trace (transparent) - Downsample if large
          let rawData = sorted;
          if (sorted.length > 2000) {
            const step = Math.ceil(sorted.length / 2000);
            rawData = sorted.filter((_, i) => i % step === 0);
          }

          const line = d3.line().x(d => x(d.iteration)).y(d => y(d.value));
          plotArea.append("path").datum(rawData)
            .attr("fill", "none").attr("stroke", color).attr("stroke-width", 0.8)
            .attr("stroke-opacity", 0.2).attr("d", line);

          // Smoothed line (bin average approach for performance)
          const nBins = Math.min(80, Math.ceil(sorted.length / 20));
          if (nBins > 2) {
            const binSize = Math.ceil(sorted.length / nBins);
            const smoothed = [];
            for (let b = 0; b < nBins; b++) {
              const start = b * binSize;
              const end = Math.min(start + binSize, sorted.length);
              const slice = sorted.slice(start, end);
              if (slice.length === 0) continue;
              smoothed.push({
                iteration: d3.mean(slice, d => d.iteration),
                value: d3.mean(slice, d => d.value),
              });
            }
            const smoothLine = d3.line().curve(d3.curveBasis)
              .x(d => x(d.iteration)).y(d => y(d.value));
            plotArea.append("path").datum(smoothed)
              .attr("fill", "none").attr("stroke", color).attr("stroke-width", 1.5)
              .attr("d", smoothLine);
          }
        });

        // Axes
        const iterShift = iterExtent[0];
        const xAxisG = ig.append("g").attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${innerH})`);
        const yAxisG = ig.append("g").attr("class", "gd3-axis gd3-y-axis");

        function drawAxes(xS, yS) {
          xAxisG.call(d3.axisBottom(xS).ticks(4).tickFormat(d => {
            return ((d - iterShift) / 1e3).toFixed(1) + "K";
          }));
          const yTicks = yS.ticks(5);
          const yr = yTicks.length > 1 ? yTicks[yTicks.length - 1] - yTicks[0] : 1;
          yAxisG.call(d3.axisLeft(yS).ticks(5).tickFormat(d => {
            const isCV = (panel.is_cv === true);
            const suffix = isCV ? " %" : "";
            if (yr < 1.5) return d.toFixed(2) + suffix;
            if (yr < 15) return d.toFixed(1) + suffix;
            return Math.round(d) + suffix;
          }));
        }
        drawAxes(x, y);

        // Tooltip on hover
        const hoverLine = ig.append("line")
          .attr("y1", 0).attr("y2", innerH)
          .attr("stroke", COLORS.grayLight).attr("stroke-width", 1).attr("stroke-dasharray", "3,3")
          .style("opacity", 0).style("pointer-events", "none");

        ig.append("rect").attr("class", "gd3-hover-rect")
          .attr("width", innerW).attr("height", innerH)
          .attr("fill", "transparent").style("cursor", "crosshair")
          .on("pointerenter", function () { hoverLine.style("opacity", 1); })
          .on("pointermove", function (event) {
            const [mx] = d3.pointer(event);
            hoverLine.attr("x1", mx).attr("x2", mx).style("opacity", 1);
            const iter = x.invert(mx);
            let html = `<span class="gd3-tip-muted">Iteration:</span> ${Math.round(iter)}<br>`;
            byChain.forEach((cd, ch) => {
              const closest = cd.reduce((best, d) =>
                Math.abs(d.iteration - iter) < Math.abs(best.iteration - iter) ? d : best, cd[0]);
              const ci = allChains.indexOf(ch);
              html += `<span style="color:${CHAIN_COLORS[ci % CHAIN_COLORS.length]}">Chain ${ch}:</span> ${formatNumber(closest.value, 3)}<br>`;
            });
            showTooltip(event, html);
          })
          .on("pointerleave", function () { hoverLine.style("opacity", 0); hideTooltip(); });

        // Zoom (Ctrl+wheel)
        const zoom = d3.zoom().scaleExtent([1, 30])
          .wheelDelta(event => -event.deltaY * 0.001)
          .translateExtent([[0, 0], [innerW, innerH]])
          .extent([[0, 0], [innerW, innerH]])
          .on("zoom", function (event) {
            const nx = event.transform.rescaleX(x);
            const ny = event.transform.rescaleY(y);
            drawAxes(nx, ny);
            plotArea.selectAll("path").each(function () {
              const path = d3.select(this);
              const pathData = path.datum();
              if (!pathData || !Array.isArray(pathData)) return;
              if (path.attr("stroke-opacity")) {
                // Raw trace
                const line = d3.line().x(d => nx(d.iteration)).y(d => ny(d.value));
                path.attr("d", line);
              } else {
                // Smoothed
                const line = d3.line().curve(d3.curveBasis).x(d => nx(d.iteration)).y(d => ny(d.value));
                path.attr("d", line);
              }
            });
          });
        zoom.filter(event => event.ctrlKey || event.type === "dblclick");
        ig.select(".gd3-hover-rect").call(zoom)
          .on("dblclick.zoom", function () {
            d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
          });
      });

      // Toolbar
      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "trace_plots", data.raw_data);
      resetBtn.onclick = () => {
        // Reset all zooms — re-render
        if (state.lastPayload) update(state.lastPayload);
      };
    }

    /* ================================================================== */
    /* 5. POSTERIOR DENSITY (faceted small multiples)                      */
    /* ================================================================== */
    function renderPosteriorDensity(svg, containerElement, data, params) {
      const panels = data.panels || [];
      if (panels.length === 0) return;
      const title = params.title || "Posterior Density Plots";
      const subtitle = params.subtitle || "";
      const includeHistogram = params.include_histogram || false;

      const nCols = params.ncols || 3;
      const nRows = Math.ceil(panels.length / nCols);
      const gap = 50;
      const outerMargin = { top: 70, right: 20, bottom: 20, left: 10 };
      const panelMargin = { top: 30, right: 10, bottom: 30, left: 55 };

      const availW = Math.max(600, containerElement.clientWidth || 800) - outerMargin.left - outerMargin.right;
      const panelW = (availW - gap * (nCols - 1)) / nCols;
      const panelH = Math.max(140, Math.min(200, panelW * 0.65));

      const { totalW, totalH } = getDimensions(containerElement, outerMargin, {
        totalW: outerMargin.left + nCols * panelW + (nCols - 1) * gap + outerMargin.right,
        totalH: outerMargin.top  + nRows * panelH + (nRows - 1) * gap + outerMargin.bottom
      });

      svg.attr("width", totalW)
         .attr("height", totalH)
         .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text")
         .attr("class", "gd3-title")
         .attr("x", totalW / 2)
         .attr("y", 24).text(title);
      if (subtitle) {
        svg.append("text")
           .attr("class", "gd3-subtitle")
           .attr("x", totalW / 2)
           .attr("y", 44)
           .text(subtitle);
      }

      const landmarksOnly = params.tooltip_landmarks_only === true;

      panels.forEach((panel, idx) => {
        const col = idx % nCols;
        const row = Math.floor(idx / nCols);
        const px = outerMargin.left + col * (panelW + gap);
        const py = outerMargin.top + row * (panelH + gap);
        const innerW = panelW - panelMargin.left - panelMargin.right;
        const innerH = panelH - panelMargin.top - panelMargin.bottom;

        const panelG = svg.append("g").attr("transform", `translate(${px},${py})`);

        // Strip
        const stripH = 22;
        panelG.append("rect").attr("class", "gd3-strip-bg")
          .attr("x", 0).attr("y", 0).attr("width", panelW).attr("height", stripH).attr("rx", 4);
        panelG.append("text").attr("class", "gd3-strip-text")
          .attr("x", panelW / 2).attr("y", stripH / 2 + 1).text(panel.label || panel.parameter);

        const ig = panelG.append("g").attr("transform", `translate(${panelMargin.left},${panelMargin.top})`);

        const clipId = "clip-post-" + safeId(containerId) + "-" + idx;
        ig.append("defs").append("clipPath").attr("id", clipId).append("rect")
          .attr("width", innerW).attr("height", innerH);

        // Use pre-computed density from R when available (prior panels),
        // otherwise fall back to JS-side KDE (posterior panels).
        let density, mean, cri025, cri975, values;
        const hasPrecomputed = panel.density && panel.density.x && panel.density.y;

        if (hasPrecomputed) {
          density = panel.density.x.map((xi, i) => [xi, panel.density.y[i]]);
          mean = panel.mean;
          cri025 = panel.cri025;
          cri975 = panel.cri975;
          values = []; // not needed for rendering
        } else {
          values = (panel.values || []).slice().sort((a, b) => a - b);
          if (values.length === 0) return;
          const bw = silvermanBandwidth(values);
          const kdeTicks = landmarksOnly ? 100 : 200;
          const kde = kernelDensityEstimator(kernelEpanechnikov(bw),
            d3.scaleLinear().domain(niceExtent(values, 0.02, [0, 1])).range([0, innerW]).ticks(kdeTicks));
          density = kde(values);
          mean = d3.mean(values);
          cri025 = quantile(values, 0.025);
          cri975 = quantile(values, 0.975);
        }

        if (density.length === 0) return;

        const valExtent = [density[0][0], density[density.length - 1][0]];
        const x = d3.scaleLinear().domain(valExtent).range([0, innerW]);

        const yMax = d3.max(density, d => d[1]) * 1.05;
        const y = d3.scaleLinear().domain([0, yMax]).range([innerH, 0]);

        const plotArea = ig.append("g").attr("clip-path", `url(#${clipId})`);

        // Optional histogram
        if (includeHistogram) {
          const bins = d3.bin().domain(x.domain()).thresholds(x.ticks(60))(values);
          const binMaxY = d3.max(bins, b => b.length / values.length / (b.x1 - b.x0)) * 1.05;
          const yHist = d3.scaleLinear().domain([0, binMaxY]).range([innerH, 0]);
          plotArea.selectAll("rect.gd3-hist-bar").data(bins).join("rect")
            .attr("class", "gd3-hist-bar")
            .attr("x", d => x(d.x0) + 1).attr("width", d => Math.max(0, x(d.x1) - x(d.x0) - 1))
            .attr("y", d => yHist(d.length / values.length / (d.x1 - d.x0)))
            .attr("height", d => innerH - yHist(d.length / values.length / (d.x1 - d.x0)));
        }

        // Density curve
        const area = d3.area()
          .x(d => x(d[0]))
          .y0(innerH)
          .y1(d => y(d[1]))
          .curve(d3.curveBasis);

        if (!includeHistogram) {
          plotArea.append("path").datum(density)
            .attr("class", "gd3-density-fill").attr("d", area);
        }

        const line = d3.line().x(d => x(d[0])).y(d => y(d[1])).curve(d3.curveBasis);
        plotArea.append("path").datum(density)
          .attr("class", "gd3-density-line").attr("d", line);

        // Mean line (pre-computed or computed above)
        plotArea.append("line").attr("class", "gd3-mean-line")
          .attr("x1", x(mean)).attr("x2", x(mean)).attr("y1", 0).attr("y2", innerH);
        [cri025, cri975].forEach(v => {
          plotArea.append("line").attr("class", "gd3-cri-line")
            .attr("x1", x(v)).attr("x2", x(v)).attr("y1", 0).attr("y2", innerH);
        });

        // Axes
        const isCV = panel.is_cv === true;
        const suffix = isCV ? " %" : "";

        const xAxisG = ig.append("g")
          .attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${innerH})`);
        const valRange = valExtent[1] - valExtent[0];
        xAxisG.call(d3.axisBottom(x).ticks(5).tickFormat(d => {
          if (valRange < 1.5) {
            return d.toFixed(2) + suffix;
          }
          if (valRange < 15) {
            return d.toFixed(1) + suffix;
          }
          return Math.round(d) + suffix;
        }));

        // Hover — landmarks-only mode skips the expensive per-pixel crosshair
        if (landmarksOnly) {
          // Hover rect FIRST so it sits behind landmark hit zones in SVG
          // (later elements render on top and capture pointer events first)
          ig.append("rect")
            .attr("class", "gd3-hover-rect")
            .attr("width", innerW)
            .attr("height", innerH)
            .attr("fill", "transparent");

          // Tooltip on the mean line and CrI lines only
          const landmarkTip = (label, val) =>
            `<strong>${panel.label || panel.parameter}</strong><br>` +
            `<span class="gd3-tip-muted">${label}:</span> ${formatNumber(val, 3)}${suffix}<br>` +
            `<span class="gd3-tip-muted">Mean:</span> ${formatNumber(mean, 3)}${suffix}<br>` +
            `<span class="gd3-tip-muted">95% CrI:</span> ${formatNumber(cri025, 3)} \u2013 ${formatNumber(cri975, 3)}${suffix}`;

          const hitW = 10; // invisible hit-zone width around each line
          // Mean line hit zone
          ig.append("rect")
            .attr("class", "gd3-landmark-hit")
            .attr("x", x(mean) - hitW / 2)
            .attr("y", 0)
            .attr("width", hitW)
            .attr("height", innerH)
            .attr("fill", "transparent").style("cursor", "pointer")
            .datum({ label: "Mean", value: mean })
            .on("pointerenter", function (event, d) { showTooltip(event, landmarkTip(d.label, d.value)); })
            .on("pointermove", moveTooltip)
            .on("pointerleave", function () { hideTooltip(); });
          // CrI hit zones
          [{label: "2.5th percentile", value: cri025},
           {label: "97.5th percentile", value: cri975}].forEach(lm => {
            ig.append("rect")
              .attr("class", "gd3-landmark-hit")
              .attr("x", x(lm.value) - hitW / 2)
              .attr("y", 0)
              .attr("width", hitW)
              .attr("height", innerH)
              .attr("fill", "transparent").style("cursor", "pointer")
              .datum(lm)
              .on("pointerenter", function (event, d) { showTooltip(event, landmarkTip(d.label, d.value)); })
              .on("pointermove", moveTooltip)
              .on("pointerleave", function () { hideTooltip(); });
          });
        } else {
          const hoverLine = ig.append("line")
            .attr("y1", 0)
            .attr("y2", innerH)
            .attr("stroke", COLORS.grayLight)
            .attr("stroke-width", 1)
            .attr("stroke-dasharray", "3,3")
            .style("opacity", 0).style("pointer-events", "none");

          ig.append("rect")
            .attr("class", "gd3-hover-rect")
            .attr("width", innerW)
            .attr("height", innerH)
            .attr("fill", "transparent").style("cursor", "crosshair")
            .on("pointerenter", function () { hoverLine.style("opacity", 1); })
            .on("pointermove", function (event) {
              const [mx] = d3.pointer(event);
              hoverLine.attr("x1", mx).attr("x2", mx).style("opacity", 1);
              const xVal = x.invert(mx);
              const closestDens = density.reduce((best, d) =>
                Math.abs(d[0] - xVal) < Math.abs(best[0] - xVal) ? d : best, density[0]);
              showTooltip(event, `<strong>${panel.label || panel.parameter}</strong><br>
                <span class="gd3-tip-muted">Value:</span> ${formatNumber(xVal, 3)}${suffix}<br>
                <span class="gd3-tip-muted">Density:</span> ${formatNumber(closestDens[1], 4)}<br>
                <span class="gd3-tip-muted">Mean:</span> ${formatNumber(mean, 3)}${suffix}<br>
                <span class="gd3-tip-muted">95% CrI:</span> ${formatNumber(cri025, 3)} \u2013 ${formatNumber(cri975, 3)}${suffix}`);
            })
            .on("pointerleave", function () { hoverLine.style("opacity", 0); hideTooltip(); });
        }

        // Zoom
        const zoom = d3.zoom().scaleExtent([1, 30])
          .wheelDelta(event => -event.deltaY * 0.001)
          .translateExtent([[0, 0], [innerW, innerH]])
          .extent([[0, 0], [innerW, innerH]])
          .on("zoom", function (event) {
            const nx = event.transform.rescaleX(x);
            xAxisG.call(d3.axisBottom(nx).ticks(5).tickFormat(d => {
              const r = nx.domain()[1] - nx.domain()[0];
              if (r < 1.5) {
                return d.toFixed(2) + suffix;
              }
              if (r < 15) {
                return d.toFixed(1) + suffix;
              }
              return Math.round(d) + suffix;
            }));
            const newArea = d3.area().x(d => nx(d[0])).y0(innerH).y1(d => y(d[1])).curve(d3.curveBasis);
            const newLine = d3.line().x(d => nx(d[0])).y(d => y(d[1])).curve(d3.curveBasis);
            plotArea.select(".gd3-density-fill")
              .attr("d", newArea);
            plotArea.select(".gd3-density-line")
              .attr("d", newLine);
            plotArea.select(".gd3-mean-line")
              .attr("x1", nx(mean))
              .attr("x2", nx(mean));
            plotArea.selectAll(".gd3-cri-line").each(function (_, i) {
              const v = i === 0 ? cri025 : cri975;
              d3.select(this)
                .attr("x1", nx(v))
                .attr("x2", nx(v));
            });
            // Reposition landmark hit zones if present
            if (landmarksOnly) {
              const hitW = 10;
              ig.selectAll(".gd3-landmark-hit").each(function () {
                const el = d3.select(this);
                const d = el.datum();
                el.attr("x", nx(d.value) - hitW / 2);
              });
            }
            if (includeHistogram) {
              const bins = d3.bin().domain(nx.domain()).thresholds(nx.ticks(60))(values);
              const binMaxY = d3.max(bins, b => b.length ? b.length / values.length / (b.x1 - b.x0) : 0) * 1.05 || 1;
              const yH = d3.scaleLinear().domain([0, binMaxY]).range([innerH, 0]);
              plotArea.selectAll(".gd3-hist-bar").data(bins)
                .join("rect")
                .attr("class", "gd3-hist-bar")
                .attr("x", d => nx(d.x0) + 1)
                .attr("width", d => Math.max(0, nx(d.x1) - nx(d.x0) - 1))
                .attr("y", d => yH(d.length / values.length / (d.x1 - d.x0)))
                .attr("height", d => innerH - yH(d.length / values.length / (d.x1 - d.x0)));
            }
          });
        zoom.filter(event => event.ctrlKey || event.type === "dblclick");
        if (landmarksOnly) {
          // Attach zoom to the ig group so events from both the hover rect
          // and the landmark hit zones (which sit on top) bubble up correctly
          ig.call(zoom)
            .on("dblclick.zoom", function () {
              d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
            });
        } else {
          ig.select(".gd3-hover-rect").call(zoom)
            .on("dblclick.zoom", function () {
              d3.select(this).transition().duration(500).call(zoom.transform, d3.zoomIdentity);
            });
        }
      });

      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "posterior_density", data.raw_data);
      resetBtn.onclick = () => { if (state.lastPayload) update(state.lastPayload); };
    }

    /* ================================================================== */
    /* 6. PRIOR DENSITY (3x3 grid)                                        */
    /* ================================================================== */
    function renderPriorDensity(svg, containerElement, data, params) {
      // Reuse posterior density renderer with prior-specific defaults
      const priorPanels = data.panels || [];
      if (priorPanels.length === 0) return;

      const title = params.title || "Prior Density Plots";
      const subtitle = params.subtitle || "";

      // Prior densities share the layout logic with posterior — delegate
      renderPosteriorDensity(svg, containerElement,
        { panels: priorPanels, raw_data: data.raw_data },
        {
          title: title,
          subtitle: subtitle,
          ncols: params.ncols || 3,
          include_histogram: false,
          tooltip_landmarks_only: true,
        }
      );
    }

    /* ---- KDE helpers ---- */
    function kernelDensityEstimator(kernel, X) {
      return function (V) {
        return X.map(function (x) {
          return [x, d3.mean(V, function (v) { return kernel(x - v); })];
        });
      };
    }

    function kernelEpanechnikov(k) {
      return function (v) {
        v = v / k;
        return Math.abs(v) <= 1 ? 0.75 * (1 - v * v) / k : 0;
      };
    }

    /**
     * Silverman's rule-of-thumb bandwidth for KDE.
     * h = 0.9 * min(sd, IQR/1.34) * n^(-1/5)
     * Falls back to range/10 if both sd and IQR are zero.
     * @param {number[]} values - Array of values (must already be sorted ascending).
     */
    function silvermanBandwidth(values) {
      const n = values.length;
      if (n < 2) return 1;
      const mean = d3.mean(values);
      const variance = d3.mean(values, v => (v - mean) ** 2);
      const sd = Math.sqrt(variance);
      const q25 = values[Math.floor(n * 0.25)];
      const q75 = values[Math.floor(n * 0.75)];
      const iqr = q75 - q25;
      const spread = iqr > 0 ? Math.min(sd, iqr / 1.34) : sd;
      if (spread === 0) {
        const range = values[n - 1] - values[0];
        return range > 0 ? range / 10 : 1;
      }
      return 2 * 0.9 * spread * Math.pow(n, -0.2);
    }

    /* ================================================================== */
    /* 7. EXPLORATION SCATTER (click-to-exclude/include)                   */
    /* ================================================================== */
    function renderExplorationScatter(svg, containerElement, data, params) {
      const points = normalizePoints(data.points);
      if (points.length === 0) return;

      const grandMean = data.grand_mean;
      const title = params.title || "Data Points";
      const subtitle = params.subtitle || "";
      const nsId = containerId + "_click";
      const viewMode = params.view_mode || "combined"; // "combined" or "faceted"

      if (viewMode === "faceted") {
        renderExplorationScatterFaceted(svg, containerElement, data, params);
        return;
      }

      // ---- COMBINED VIEW ----
      const margin = { top: 70, right: 30, bottom: 60, left: 80 };
      const { plotW, plotH, totalW, totalH } = getDimensions(containerElement, margin, {
        plotH: Math.max(220, Math.min(320, specifiedHeight - margin.top - margin.bottom))
      });

      svg.attr("width", totalW)
        .attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text")
        .attr("class", "gd3-title")
        .attr("x", totalW / 2)
        .attr("y", 24).text(title);

      if (subtitle) {
        svg.append("text")
          .attr("class", "gd3-subtitle")
          .attr("x", totalW / 2)
          .attr("y", 44).text(subtitle);
      }

      const g = svg.append("g")
        .attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const x = d3.scaleLinear().domain([0, points.length + 1]).range([0, plotW]);
      const yVals = points.map(d => d.y);
      const y = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([plotH, 0]);

      // Grand mean line
      if (grandMean != null) {
        g.append("line")
          .attr("x1", 0)
          .attr("x2", plotW)
          .attr("y1", y(grandMean))
          .attr("y2", y(grandMean))
          .attr("stroke", COLORS.gray)
          .attr("stroke-width", 1.5)
          .attr("stroke-dasharray", "6,4")
          .attr("opacity", 0.6);
        g.append("text")
          .attr("x", plotW - 5)
          .attr("y", y(grandMean) - 6)
          .attr("text-anchor", "end")
          .attr("fill", COLORS.gray)
          .attr("font-size", "11px")
          .text("Grand Mean: " + formatNumber(grandMean, 2));
      }

      // Clip
      const clipId = "clip-escat-" + safeId(containerId);
      g.append("defs")
        .append("clipPath")
        .attr("id", clipId)
        .append("rect")
        .attr("width", plotW)
        .attr("height", plotH);
      const plotArea = g.append("g")
        .attr("clip-path", `url(#${clipId})`);

      // Points
      const ptG = plotArea.selectAll("g.gd3-scatter-pt")
        .data(points)
        .join("g")
        .attr("class", "gd3-scatter-pt")
        .style("cursor", "pointer");

      ptG.append("circle")
        .attr("cx", d => x(d.idx))
        .attr("cy", d => y(d.y))
        .attr("r", d => d.excluded ? 5.5 : 4.5)
        .attr("fill", d => d.excluded ? "transparent" : d.color)
        .attr("stroke", d => d.excluded ? COLORS.red : d.color)
        .attr("stroke-width", d => d.excluded ? 2 : 1)
        .attr("opacity", d => d.excluded ? 0.6 : 0.85);

      // X markers for excluded points
      ptG.each(function (d) {
        if (d.excluded) {
          const cx = x(d.idx), cy = y(d.y), sz = 4;
          const el = d3.select(this);
          el.append("line")
            .attr("x1", cx - sz).attr("y1", cy - sz)
            .attr("x2", cx + sz).attr("y2", cy + sz)
            .attr("stroke", COLORS.red).attr("stroke-width", 1.8)
            .style("pointer-events", "none");
          el.append("line")
            .attr("x1", cx - sz).attr("y1", cy + sz)
            .attr("x2", cx + sz).attr("y2", cy - sz)
            .attr("stroke", COLORS.red).attr("stroke-width", 1.8)
            .style("pointer-events", "none");
        }
      });

      // Click → send to Shiny
      ptG.on("click", function (event, d) {
        event.stopPropagation();
        if (typeof Shiny !== "undefined") {
          Shiny.setInputValue(nsId, {
            SubjectID: d.SubjectID,
            SampleID: d.SampleID,
            ReplicateID: d.ReplicateID,
            excluded: d.excluded,
            timestamp: Date.now()
          }, { priority: "event" });
        }
        // Quick visual pulse
        d3.select(this).select("circle")
          .transition().duration(120).attr("r", 9)
          .transition().duration(200).attr("r", d.excluded ? 5.5 : 4.5);
      });

      // Tooltip
      ptG.on("pointerenter", function (event, d) {
        d3.select(this).select("circle").attr("r", 7).attr("stroke-width", 2.5);
        const status = d.excluded
          ? '<span style="color:#DC1932">\u2717 Excluded</span>'
          : '<span style="color:#28A745">\u2713 Included</span>';
        showTooltip(event, `<strong>Subject ${d.SubjectID}</strong><br>
          <span class="gd3-tip-muted">Sample:</span> ${d.SampleID}<br>
          <span class="gd3-tip-muted">Replicate:</span> ${d.ReplicateID}<br>
          <span class="gd3-tip-muted">Value:</span> ${formatNumber(d.y, 4)}<br>
          ${status}<br>
          <em style="font-size:10px;color:#888">Click to ${d.excluded ? "restore" : "exclude"}</em>`);
      })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function (event, d) {
          d3.select(this).select("circle")
            .attr("r", d.excluded ? 5.5 : 4.5)
            .attr("stroke-width", d.excluded ? 2 : 1);
          hideTooltip();
        });

      // Axes
      const xAxisG = g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(10).tickFormat(d3.format("d")));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y).ticks(8));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Observation Index");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -60).text("Measurement Value");

      // Subject legend (up to 30)
      const uSubjects = [...new Set(points.map(p => p.SubjectID))];
      if (uSubjects.length <= 30) {
        const legX = plotW - 6, legG = g.append("g").attr("transform", `translate(${legX},0)`);
        legG.append("text").attr("class", "gd3-legend-title").attr("y", -5).attr("text-anchor", "end").text("Subject");
        uSubjects.forEach((s, i) => {
          const c = points.find(p => p.SubjectID === s).color;
          legG.append("circle").attr("cx", -5).attr("cy", 10 + i * 15).attr("r", 3.5).attr("fill", c);
          legG.append("text").attr("class", "gd3-legend-text")
            .attr("x", -12).attr("y", 13 + i * 15).attr("text-anchor", "end").text(s);
        });
      }

      // Excluded count annotation
      const nExcl = points.filter(p => p.excluded).length;
      if (nExcl > 0) {
        g.append("text")
          .attr("x", 5).attr("y", -8)
          .attr("fill", COLORS.red).attr("font-size", "12px").attr("font-weight", "600")
          .text(nExcl + " point" + (nExcl > 1 ? "s" : "") + " excluded");
      }

      // Zoom
      const zoom = d3.zoom().scaleExtent([1, 20])
        .wheelDelta(event => -event.deltaY * 0.001)
        .translateExtent([[0, 0], [plotW, plotH]])
        .extent([[0, 0], [plotW, plotH]])
        .on("zoom", function (event) {
          const nx = event.transform.rescaleX(x);
          const ny = event.transform.rescaleY(y);
          xAxisG.call(d3.axisBottom(nx).ticks(10).tickFormat(d3.format("d")));
          g.select(".gd3-y-axis").call(d3.axisLeft(ny).ticks(8));
          ptG.select("circle")
            .attr("cx", d => nx(d.idx)).attr("cy", d => ny(d.y));
          ptG.selectAll("line").each(function () {
            const l = d3.select(this);
            const pd = d3.select(this.parentNode).datum();
            if (pd && pd.excluded) {
              const cx = nx(pd.idx), cy = ny(pd.y), sz = 4;
              // Determine which diagonal this line belongs to
              const x1Cur = parseFloat(l.attr("x1"));
              const y1Cur = parseFloat(l.attr("y1"));
              // Diagonal 1: top-left to bottom-right
              // Diagonal 2: bottom-left to top-right
              if (y1Cur <= parseFloat(l.attr("y2"))) {
                l.attr("x1", cx - sz).attr("y1", cy - sz)
                  .attr("x2", cx + sz).attr("y2", cy + sz);
              } else {
                l.attr("x1", cx - sz).attr("y1", cy + sz)
                  .attr("x2", cx + sz).attr("y2", cy - sz);
              }
            }
          });
          if (grandMean != null) {
            g.selectAll("line").filter(function () {
              return d3.select(this).attr("stroke-dasharray") === "6,4";
            }).attr("y1", ny(grandMean)).attr("y2", ny(grandMean));
          }
        });
      zoom.filter(event => event.ctrlKey || event.type === "dblclick");

      const zoomRect = g.append("rect").attr("class", "gd3-zoom-rect")
        .attr("width", plotW).attr("height", plotH)
        .call(zoom);
      zoomRect.on("dblclick.zoom", null);
      svg.on("dblclick.gd3reset", function () {
        zoomRect.transition().duration(500).call(zoom.transform, d3.zoomIdentity);
      });

      const { resetBtn } = buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "exploration_scatter", data.raw_data);
      resetBtn.onclick = () => zoomRect.transition().duration(500)
        .call(zoom.transform, d3.zoomIdentity);
    }

    /* ================================================================== */
    /* 7b. EXPLORATION SCATTER — FACETED VIEW                             */
    /* ================================================================== */
    function renderExplorationScatterFaceted(svg, containerElement, data, params) {
      const allPoints = normalizePoints(data.points);
      if (allPoints.length === 0) return;

      const grandMean = data.grand_mean;
      const title = params.title || "Data Points — Faceted by Subject";
      const subtitle = params.subtitle || "";
      const nsId = containerId + "_click";

      // Group points by subject
      const subjectMap = new Map();
      allPoints.forEach(p => {
        if (!subjectMap.has(p.SubjectID)) subjectMap.set(p.SubjectID, []);
        subjectMap.get(p.SubjectID).push(p);
      });
      const subjects = [...subjectMap.keys()];
      const nSubjects = subjects.length;

      // Layout: determine grid
      const containerW = containerElement.clientWidth || 700;
      const cols = Math.min(nSubjects, containerW < 600 ? 2 : containerW < 900 ? 3 : containerW < 1200 ? 4 : 5);
      const rows = Math.ceil(nSubjects / cols);
      const cellPad = 12;
      const cellW = Math.floor((containerW - cellPad * (cols + 1)) / cols);
      const cellH = Math.max(140, Math.min(180, 160));
      const margin = { top: 56, right: 10, bottom: 10, left: 10 };
      const innerMargin = { top: 24, right: 12, bottom: 28, left: 38 };

      const { totalW, totalH } = getDimensions(containerElement, margin, {
        totalW: containerW,
        totalH: margin.top + rows * (cellH + cellPad) + margin.bottom
      });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title")
        .attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) {
        svg.append("text").attr("class", "gd3-subtitle")
          .attr("x", totalW / 2).attr("y", 44).text(subtitle);
      }

      // Global y domain (shared across all facets for comparability)
      const allYVals = allPoints.map(d => d.y);
      const yDomain = niceExtent(allYVals, 0.08);

      // Excluded count annotation
      const nExcl = allPoints.filter(p => p.excluded).length;
      if (nExcl > 0) {
        svg.append("text")
          .attr("x", 15).attr("y", 22)
          .attr("fill", COLORS.red).attr("font-size", "12px").attr("font-weight", "600")
          .text(nExcl + " point" + (nExcl > 1 ? "s" : "") + " excluded");
      }

      subjects.forEach((subj, si) => {
        const col = si % cols;
        const row = Math.floor(si / cols);
        const cellX = cellPad + col * (cellW + cellPad);
        const cellY = margin.top + row * (cellH + cellPad);
        const pts = subjectMap.get(subj);
        const subjColor = pts[0].color;

        const facetG = svg.append("g")
          .attr("transform", `translate(${cellX},${cellY})`);

        // Facet background
        facetG.append("rect")
          .attr("width", cellW).attr("height", cellH)
          .attr("rx", 8).attr("ry", 8)
          .attr("fill", "rgba(96,92,168,0.03)")
          .attr("stroke", "rgba(96,92,168,0.12)")
          .attr("stroke-width", 1);

        // Facet title
        facetG.append("text")
          .attr("x", cellW / 2).attr("y", 16)
          .attr("text-anchor", "middle")
          .attr("fill", subjColor)
          .attr("font-size", "11px").attr("font-weight", "700")
          .text("Subject " + subj);

        const plotAreaW = cellW - innerMargin.left - innerMargin.right;
        const plotAreaH = cellH - innerMargin.top - innerMargin.bottom;

        const ig = facetG.append("g")
          .attr("transform", `translate(${innerMargin.left},${innerMargin.top})`);

        // Scales
        const xF = d3.scaleLinear().domain([0, pts.length + 1]).range([0, plotAreaW]);
        const yF = d3.scaleLinear().domain(yDomain).nice().range([plotAreaH, 0]);

        // Grand mean line
        if (grandMean != null) {
          ig.append("line")
            .attr("x1", 0).attr("x2", plotAreaW)
            .attr("y1", yF(grandMean)).attr("y2", yF(grandMean))
            .attr("stroke", COLORS.gray).attr("stroke-width", 1)
            .attr("stroke-dasharray", "4,3").attr("opacity", 0.4);
        }

        // Points
        pts.forEach((d, pi) => {
          const ptG = ig.append("g").attr("class", "gd3-scatter-pt").style("cursor", "pointer");
          const cx = xF(pi + 1), cy = yF(d.y);

          ptG.append("circle")
            .attr("cx", cx).attr("cy", cy)
            .attr("r", d.excluded ? 5 : 4)
            .attr("fill", d.excluded ? "transparent" : d.color)
            .attr("stroke", d.excluded ? COLORS.red : d.color)
            .attr("stroke-width", d.excluded ? 2 : 1)
            .attr("opacity", d.excluded ? 0.6 : 0.85);

          if (d.excluded) {
            const sz = 3.5;
            ptG.append("line")
              .attr("x1", cx - sz).attr("y1", cy - sz)
              .attr("x2", cx + sz).attr("y2", cy + sz)
              .attr("stroke", COLORS.red).attr("stroke-width", 1.5)
              .style("pointer-events", "none");
            ptG.append("line")
              .attr("x1", cx - sz).attr("y1", cy + sz)
              .attr("x2", cx + sz).attr("y2", cy - sz)
              .attr("stroke", COLORS.red).attr("stroke-width", 1.5)
              .style("pointer-events", "none");
          }

          // Click → send to Shiny
          ptG.on("click", function (event) {
            event.stopPropagation();
            if (typeof Shiny !== "undefined") {
              Shiny.setInputValue(nsId, {
                SubjectID: d.SubjectID,
                SampleID: d.SampleID,
                ReplicateID: d.ReplicateID,
                excluded: d.excluded,
                timestamp: Date.now()
              }, { priority: "event" });
            }
            d3.select(this).select("circle")
              .transition().duration(120).attr("r", 8)
              .transition().duration(200).attr("r", d.excluded ? 5 : 4);
          });

          // Tooltip
          ptG.on("pointerenter", function (event) {
            d3.select(this).select("circle").attr("r", 6.5).attr("stroke-width", 2.5);
            const status = d.excluded
              ? '<span style="color:#DC1932">\u2717 Excluded</span>'
              : '<span style="color:#28A745">\u2713 Included</span>';
            showTooltip(event, `<strong>Subject ${d.SubjectID}</strong><br>
              <span class="gd3-tip-muted">Sample:</span> ${d.SampleID}<br>
              <span class="gd3-tip-muted">Replicate:</span> ${d.ReplicateID}<br>
              <span class="gd3-tip-muted">Value:</span> ${formatNumber(d.y, 4)}<br>
              ${status}<br>
              <em style="font-size:10px;color:#888">Click to ${d.excluded ? "restore" : "exclude"}</em>`);
          })
            .on("pointermove", moveTooltip)
            .on("pointerleave", function () {
              d3.select(this).select("circle")
                .attr("r", d.excluded ? 5 : 4)
                .attr("stroke-width", d.excluded ? 2 : 1);
              hideTooltip();
            });
        });

        // Axes (minimal)
        ig.append("g").attr("class", "gd3-axis gd3-x-axis")
          .attr("transform", `translate(0,${plotAreaH})`)
          .call(d3.axisBottom(xF).ticks(Math.min(5, pts.length)).tickFormat(d3.format("d")))
          .selectAll("text").attr("font-size", "9px");
        ig.append("g").attr("class", "gd3-axis gd3-y-axis")
          .call(d3.axisLeft(yF).ticks(4))
          .selectAll("text").attr("font-size", "9px");
      });

      buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "exploration_scatter_faceted", data.raw_data);
    }

    /* ================================================================== */
    /* 8. DESCRIPTIVE DOTPLOT (strip chart by subject)                     */
    /* ================================================================== */
    function renderDescriptiveDotplot(svg, containerElement, data, params) {
      const points = data.points || [];
      const subjectMeans = data.subject_means || [];
      const grandMean = data.grand_mean;
      const title = params.title || "Data Distribution by Subject";
      const subtitle = params.subtitle || "";

      if (points.length === 0) return;

      const subjects = [...new Set(points.map(p => p.SubjectID))];
      const nSubjects = subjects.length;

      const margin = { top: 70, right: 30, bottom: 60, left: 100 };
      const rowH = Math.max(28, Math.min(50, 500 / nSubjects));
      const { plotW, plotH, totalW, totalH } = getDimensions(containerElement, margin, { plotH: nSubjects * rowH });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Scales
      const yVals = points.map(d => d.y);
      const x = d3.scaleLinear().domain(niceExtent(yVals, 0.08)).nice().range([0, plotW]);
      const y = d3.scaleBand().domain(subjects).range([0, plotH]).padding(0.2);

      // Alternating row stripes
      subjects.forEach((s, i) => {
        if (i % 2 === 1) {
          g.append("rect")
            .attr("x", 0).attr("y", y(s))
            .attr("width", plotW).attr("height", y.bandwidth())
            .attr("fill", "rgba(96,92,168,0.03)");
        }
      });

      // Grand mean line
      if (grandMean != null) {
        g.append("line")
          .attr("x1", x(grandMean)).attr("x2", x(grandMean))
          .attr("y1", 0).attr("y2", plotH)
          .attr("stroke", COLORS.purple).attr("stroke-width", 2)
          .attr("stroke-dasharray", "6,3").attr("opacity", 0.5);
        g.append("text")
          .attr("x", x(grandMean)).attr("y", -8)
          .attr("text-anchor", "middle")
          .attr("fill", COLORS.purple).attr("font-size", "11px")
          .text("Grand Mean: " + formatNumber(grandMean, 2));
      }

      // Build subject mean map
      const meanMap = {};
      subjectMeans.forEach(d => { meanMap[d.SubjectID] = d; });

      // Subject mean diamonds + SD whiskers
      subjects.forEach(subj => {
        const sm = meanMap[subj];
        if (!sm) return;
        const cy = y(subj) + y.bandwidth() / 2;
        const cx = x(sm.mean);
        const dSize = 6;

        // SD whisker
        if (sm.sd != null && sm.sd > 0 && isFinite(sm.sd)) {
          g.append("line")
            .attr("x1", x(sm.mean - sm.sd)).attr("x2", x(sm.mean + sm.sd))
            .attr("y1", cy).attr("y2", cy)
            .attr("stroke", COLORS.grayDark).attr("stroke-width", 1)
            .attr("stroke-dasharray", "3,2").attr("opacity", 0.4);
        }

        // Diamond
        g.append("polygon")
          .attr("points",
            `${cx},${cy - dSize} ${cx + dSize},${cy} ${cx},${cy + dSize} ${cx - dSize},${cy}`)
          .attr("fill", sm.color || COLORS.magenta)
          .attr("stroke", COLORS.black).attr("stroke-width", 0.8)
          .attr("opacity", 0.85);
      });

      // Individual data points (jittered)
      const jitter = y.bandwidth() * 0.3;
      // Deterministic jitter using simple hash
      points.forEach((d, i) => {
        const jitterY = ((i * 7 + 3) % 100) / 100 * jitter - jitter / 2;
        const cy = y(d.SubjectID) + y.bandwidth() / 2 + jitterY;
        g.append("circle")
          .attr("cx", x(d.y)).attr("cy", cy)
          .attr("r", 3)
          .attr("fill", d.color || COLORS.magenta)
          .attr("stroke", "white").attr("stroke-width", 0.5)
          .attr("opacity", 0.55)
          .on("pointerenter", function (event) {
            d3.select(this).attr("r", 5).attr("opacity", 1);
            showTooltip(event, `<strong>Subject ${d.SubjectID}</strong><br>
              <span class="gd3-tip-muted">Value:</span> ${formatNumber(d.y, 4)}`);
          })
          .on("pointermove", moveTooltip)
          .on("pointerleave", function () {
            d3.select(this).attr("r", 3).attr("opacity", 0.55);
            hideTooltip();
          });
      });

      // Axes
      g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(8));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45).text("Measurement Value");
      g.append("text").attr("class", "gd3-axis-label")
        .attr("transform", "rotate(-90)")
        .attr("x", -plotH / 2).attr("y", -80).text("Subject");

      buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "descriptive_dotplot", data.raw_data);
    }

    /* ================================================================== */
    /* 9. ANOVA COMPONENTS (horizontal bar chart)                         */
    /* ================================================================== */
    function renderAnovaComponents(svg, containerElement, data, params) {
      const components = data.components || [];
      if (components.length === 0) return;

      const title = params.title || "ANOVA-based Variance Components";
      const subtitle = params.subtitle || "";

      const margin = { top: 70, right: 90, bottom: 60, left: 230 };
      const barH = 48;
      const gap = 22;
      const { plotW, plotH, totalW, totalH } = getDimensions(containerElement, margin, {
        minPlotW: 350,
        plotH: components.length * (barH + gap) - gap
      });

      svg.attr("width", totalW).attr("height", totalH)
        .attr("viewBox", `0 0 ${totalW} ${totalH}`);

      svg.append("text").attr("class", "gd3-title").attr("x", totalW / 2).attr("y", 24).text(title);
      if (subtitle) svg.append("text").attr("class", "gd3-subtitle").attr("x", totalW / 2).attr("y", 44).text(subtitle);

      const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

      // Determine x-scale domain from values and CI upper bounds
      const allVals = components.flatMap(d => {
        const vals = [d.value];
        if (d.upper != null && isFinite(d.upper)) vals.push(d.upper);
        return vals;
      });
      const maxVal = d3.max(allVals) * 1.25 || 1;
      const x = d3.scaleLinear().domain([0, maxVal]).range([0, plotW]);
      const y = d3.scaleBand()
        .domain(components.map(d => d.label))
        .range([0, plotH]).padding(0.3);

      const barG = g.selectAll("g.gd3-bar-group")
        .data(components).join("g").attr("class", "gd3-bar-group");

      // Background bars
      barG.append("rect")
        .attr("x", 0).attr("y", d => y(d.label))
        .attr("width", plotW).attr("height", y.bandwidth())
        .attr("fill", "rgba(240,240,240,0.7)").attr("rx", 6);

      // Value bars with gradient-like colour
      barG.append("rect")
        .attr("x", 0).attr("y", d => y(d.label))
        .attr("width", d => Math.max(4, x(d.value)))
        .attr("height", y.bandwidth())
        .attr("fill", d => d.color).attr("rx", 6)
        .attr("opacity", 0.85);

      // CI error bars (horizontal whiskers)
      barG.each(function (d) {
        const hasCI = d.lower != null && d.upper != null &&
                      isFinite(d.lower) && isFinite(d.upper);
        if (!hasCI) return;

        const barCenter = y(d.label) + y.bandwidth() / 2;
        const xLower = x(d.lower);
        const xUpper = x(d.upper);
        const whiskerCapHeight = 10;
        const sel = d3.select(this);

        // Horizontal line
        sel.append("line")
          .attr("x1", xLower).attr("x2", xUpper)
          .attr("y1", barCenter).attr("y2", barCenter)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);

        // Left cap
        sel.append("line")
          .attr("x1", xLower).attr("x2", xLower)
          .attr("y1", barCenter - whiskerCapHeight / 2).attr("y2", barCenter + whiskerCapHeight / 2)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);

        // Right cap
        sel.append("line")
          .attr("x1", xUpper).attr("x2", xUpper)
          .attr("y1", barCenter - whiskerCapHeight / 2).attr("y2", barCenter + whiskerCapHeight / 2)
          .attr("stroke", COLORS.grayDark).attr("stroke-width", 1.8)
          .attr("opacity", 0.7);
      });

      // Value labels (shifted right to accommodate potential CI upper bound)
      barG.append("text")
        .attr("x", d => {
          const hasCI = d.upper != null && isFinite(d.upper) && !isNaN(d.upper);
          const rightEdge = hasCI ? Math.max(x(d.value), x(d.upper)) : x(d.value);
          return Math.max(4, rightEdge) + 10;
        })
        .attr("y", d => y(d.label) + y.bandwidth() / 2)
        .attr("dy", "0.35em")
        .attr("fill", COLORS.grayDark)
        .attr("font-size", "14px").attr("font-weight", "700")
        .text(d => formatNumber(d.value, 2) + " %");

      // Tooltip
      barG.on("pointerenter", function (event, d) {
        d3.select(this).selectAll("rect").filter((_, i) => i === 1).attr("opacity", 1);
        const hasCI = d.lower != null && d.upper != null &&
                      isFinite(d.lower) && isFinite(d.upper);
        let tipHtml = `<strong>${d.label}</strong><br>
          <span class="gd3-tip-muted">CV:</span> ${formatNumber(d.value, 2)} %`;
        if (hasCI) {
          tipHtml += `<br><span class="gd3-tip-muted">95% CI:</span> [${formatNumber(d.lower, 2)}, ${formatNumber(d.upper, 2)}] %`;
        }
        showTooltip(event, tipHtml);
      })
        .on("pointermove", moveTooltip)
        .on("pointerleave", function () {
          d3.select(this).selectAll("rect").filter((_, i) => i === 1).attr("opacity", 0.85);
          hideTooltip();
        });

      // Axes
      g.append("g").attr("class", "gd3-axis gd3-x-axis")
        .attr("transform", `translate(0,${plotH})`)
        .call(d3.axisBottom(x).ticks(6).tickFormat(d => formatNumber(d, 1) + " %"));
      g.append("g").attr("class", "gd3-axis gd3-y-axis")
        .call(d3.axisLeft(y));

      g.append("text").attr("class", "gd3-axis-label")
        .attr("x", plotW / 2).attr("y", plotH + 45)
        .text("Coefficient of Variation (%)");

      buildToolbar(containerElement, svg.node(),
        () => ({ width: totalW, height: totalH }), "anova_components", data.raw_data);
    }

    return { update, destroy };
  }
})();
