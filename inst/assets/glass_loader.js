$(document).ready(function() {

  // Show loader handler
  Shiny.addCustomMessageHandler('glass-loader-show', function(message) {
    var id = message.id;
    var text = message.text || "Arbeider...";
    var selector = message.selector;

    // Fjern eksisterende loader med samme ID for å unngå duplikater
    $('#' + id).remove();

    // HTML struktur for loaderen
    var loaderHtml = `
      <div id="${id}" class="glass-loader-wrapper" style="display: none; opacity: 0;">
        <div class="glass-loader-spinner"></div>
        ${text ? `<div class="glass-loader-text">${text}</div>` : ''}
      </div>
    `;

    var $loader = $(loaderHtml);

    if (selector) {
      // LOKAL LOADER
      var $target = $(selector);
      if ($target.length > 0) {
        // Legg til klasse på forelderen for å styre posisjonering
        $target.addClass('glass-loader-parent-active');

        $loader.addClass('localized');
        $target.append($loader);
      } else {
        console.warn("Glass Loader: Fant ikke elementet med selector:", selector);
        // Fallback til fullskjerm hvis target ikke finnes?
        // Vi velger å ikke vise noe for å unngå forvirring, eller du kan uncomment under:
        // $('body').append($loader.addClass('fullscreen'));
        return;
      }
    } else {
      // FULLSKJERM LOADER (Default)
      $loader.addClass('fullscreen');
      $('body').append($loader);
    }

    // Fade in
    $loader.css('display', 'flex').animate({ opacity: 1 }, 200);
  });

  // Hide loader handler
  Shiny.addCustomMessageHandler('glass-loader-hide', function(message) {
    var id = message.id;
    var $loader = $('#' + id);

    if ($loader.length) {
      $loader.animate({ opacity: 0 }, 200, function() {
        // Fjern klassen fra forelderen hvis det var en lokal loader
        var $parent = $loader.parent();
        if ($parent.hasClass('glass-loader-parent-active')) {
          $parent.removeClass('glass-loader-parent-active');
        }

        $(this).remove();
      });
    }
  });

});
