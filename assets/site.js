document.addEventListener('DOMContentLoaded', function () {
  const page = location.pathname.split('/').pop() || 'index.html';
  document.body.dataset.page = page.replace('.html', '').toLowerCase();
  const main = document.querySelector('.main-container');
  if (main) { main.id = 'main-content'; main.setAttribute('role', 'main'); main.tabIndex = -1; }
  const skip = document.createElement('a'); skip.href = '#main-content'; skip.className = 'skip-link'; skip.textContent = 'Skip to content'; document.body.prepend(skip);
  document.querySelectorAll('.navbar-nav a').forEach(function (link) {
    if (link.getAttribute('href') === page) { link.setAttribute('aria-current', 'page'); link.parentElement.classList.add('active'); }
  });
  const toggle = document.querySelector('.navbar-toggle');
  if (toggle) { toggle.setAttribute('aria-label', 'Toggle navigation'); toggle.setAttribute('aria-controls', 'navbar'); toggle.setAttribute('aria-expanded', 'false'); }
  if (window.jQuery) {
    jQuery('#navbar').on('shown.bs.collapse', function () { toggle?.setAttribute('aria-expanded', 'true'); });
    jQuery('#navbar').on('hidden.bs.collapse', function () { toggle?.setAttribute('aria-expanded', 'false'); });
  }
  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape' && document.querySelector('#navbar.in') && window.jQuery) { jQuery('#navbar').collapse('hide'); toggle.focus(); }
  });
  if (page === 'Tutorials.html') {
    const header = document.querySelector('#header');
    const intro = document.createElement('p'); intro.className = 'page-intro'; intro.textContent = 'Courses, practical tutorials and open resources for working with ecological data.'; header.append(intro);
    const cards = Array.from(document.querySelectorAll('.main-container > .section.level1'));
    if (cards.length) { const grid = document.createElement('div'); grid.className = 'teaching-grid'; cards[0].before(grid); cards.forEach(function (card, i) { grid.append(card); const label = document.createElement('span'); label.className = 'course-number'; label.textContent = String(i + 1).padStart(2, '0') + ' / RESOURCE'; card.prepend(label); }); }
  }
});
