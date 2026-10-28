
// Fonction permettant de fermer les (i) 

function toggleLandscapeInfo(event, button) {
  event.stopPropagation();

  const popup = button.nextElementSibling;
  const wasOpen = popup.classList.contains('is-visible');

  // Fermer toutes les fenêtres explicatives
  document.querySelectorAll('.landscape-legend-popup.is-visible')
    .forEach(function(el) {
      el.classList.remove('is-visible');
    });

  // Ouvrir la fenêtre sélectionnée si elle était fermée
  if (!wasOpen) {
    popup.classList.add('is-visible');
  }
}

// Fermer les fenêtres lorsqu'on clique ailleurs
document.addEventListener('click', function(event) {
  if (!event.target.closest('.landscape-legend-popup')) {
    document.querySelectorAll('.landscape-legend-popup.is-visible')
      .forEach(function(el) {
        el.classList.remove('is-visible');
      });
  }
});