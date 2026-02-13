// GLASS FILE INPUT - CUSTOM BINDING v10.0 (Multi-instance Fix + DnD)
var glassFileBinding = new Shiny.InputBinding();

$.extend(glassFileBinding, {

  find: function(scope) {
    return $(scope).find('.glass-file-binding');
  },

  initialize: function(el) {
    var $el = $(el);
    var $input = $el.find('.glass-native-input');
    var $btn = $el.find('.glass-file-btn');
    var $dropZone = $el.find('.glass-file-container'); // The drag target
    var self = this;

    // --- 1. Click Handling (Browse Button) ---
    $btn.on('click', function(e) {
      // Prevent bubbling so it doesn't trigger double events if nested
      e.stopPropagation();
      $input.click();
    });

    // --- 2. Change Handling (Native Input Selection) ---
    $input.on('change', function(e) {
      var file = e.target.files[0];
      self.handleFile(file, $el);
    });

    // --- 3. Drag and Drop Handling ---

    // Prevent default browser behavior (opening the file)
    $dropZone.on('drag dragstart dragend dragover dragenter dragleave drop', function(e) {
      e.preventDefault();
      e.stopPropagation();
    });

    // Visual State: Drag Enter/Over
    $dropZone.on('dragover dragenter', function() {
      $dropZone.addClass('drag-over');
    });

    // Visual State: Drag Leave/End
    $dropZone.on('dragleave dragend drop', function() {
      $dropZone.removeClass('drag-over');
    });

    // Drop Action
    $dropZone.on('drop', function(e) {
      // Access the dataTransfer object
      var droppedFiles = e.originalEvent.dataTransfer.files;

      if(droppedFiles && droppedFiles.length > 0) {
        // We only accept the first file for single-file inputs
        self.handleFile(droppedFiles[0], $el);
      }
    });
  },

  // Centralized logic to handle a file object (whether from Click or Drop)
  handleFile: function(file, $el) {
    if (!file) {
      this.resetUI($el);
      return;
    }

    // 1. Update UI Text & Tooltip State
    var $textZone = $el.find('.glass-file-text-zone');
    var $filename = $el.find('.glass-file-filename');

    $filename.text(file.name);
    $textZone.addClass('has-file');
    $textZone.attr('data-state', 'filled');

    // 2. Start Read Process
    this.readFile(file, $el);
  },

  readFile: function(file, $el) {
    var reader = new FileReader();
    var $track = $el.find('.glass-progress-track');
    var $fill = $el.find('.glass-bar-fill');

    // Reset Bar
    $track.removeClass('finished').addClass('active');
    $fill.css('width', '0%');

    reader.onprogress = function(e) {
      if (e.lengthComputable) {
        var percent = Math.round((e.loaded / e.total) * 100);
        $fill.css('width', percent + '%');
      }
    };

    reader.onload = function(e) {
      $fill.css('width', '100%');
      $track.removeClass('active').addClass('finished');

      // Store the file data on the specific element
      $el.data('fileData', {
        name: file.name,
        size: file.size,
        type: file.type,
        content: e.target.result
      });

      // CRITICAL FIX: Trigger a custom event on THIS specific element
      // This tells 'subscribe' that data has changed.
      $el.trigger('glass:file-updated');
    };

    reader.onerror = function() {
      console.error("Error reading file");
      $el.find('.glass-file-filename').text("Error reading file");
      // Reset data
      $el.removeData('fileData');
      $el.trigger('glass:file-updated');
    };

    reader.readAsDataURL(file);
  },

  resetUI: function($el) {
    var $textZone = $el.find('.glass-file-text-zone');
    var $filename = $el.find('.glass-file-filename');
    var placeholder = $filename.attr('data-placeholder');
    var $track = $el.find('.glass-progress-track');

    $filename.text(placeholder);

    // RESET STATE FOR TOOLTIPS
    $textZone.removeClass('has-file');
    $textZone.attr('data-state', 'empty');

    $track.removeClass('active finished');
    $el.removeData('fileData');

    // Notify Shiny that the value is now null
    $el.trigger('glass:file-updated');
  },

  getValue: function(el) {
    return $(el).data('fileData') || null;
  },

  subscribe: function(el, callback) {
    // CRITICAL FIX: Listen for the custom event on the specific element
    $(el).on('glass:file-updated.glassFileBinding', function(e) {
      callback();
    });
  },

  unsubscribe: function(el) {
    $(el).off('.glassFileBinding');
  }
});

Shiny.inputBindings.register(glassFileBinding, 'glass.fileInput');
