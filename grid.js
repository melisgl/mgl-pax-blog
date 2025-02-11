function padToBaselineGrid() {
  document.querySelectorAll('pre, img, #toc').forEach(el => {
    if (el.alt == 'end-of-post' || el.alt == 'about-me-die') {
      return;
    }
    // We either add a wrapper div and pad that or pad the element
    // itself.
    let padSelf = (el.id == 'toc');
    let currentHeight = el.getBoundingClientRect().height;
    if (padSelf) {
      var paddingTop = parseFloat(window.getComputedStyle(el).getPropertyValue('padding-top'));
      var paddingBottom = parseFloat(window.getComputedStyle(el).getPropertyValue('padding-bottom'));
      currentHeight -= paddingTop + paddingBottom;
    }
    let lineHeight = parseFloat(window.getComputedStyle(document.body).lineHeight);
    let targetHeight = Math.ceil(currentHeight / lineHeight) * lineHeight;
    let extraPadding = targetHeight - currentHeight;
    if (padSelf) {
      elToPad = el;
    } else {
      if (el.parentNode.className != 'padding-wrapper') {
        w = document.createElement('div');
        w.className = 'padding-wrapper';
        el.replaceWith(w);
        w.appendChild(el);
      }
      if (extraPadding < 0.75*lineHeight) {
        extraPadding += lineHeight;
      }
      elToPad = el.parentNode;
    }
    // Specify padding in rem, so that it scales with font size
    // changes.
    let remPx = parseFloat(getComputedStyle(document.documentElement).fontSize)
    extraPadding = extraPadding / 2 /remPx;
    elToPad.style.paddingTop = `${extraPadding}rem`;
    elToPad.style.paddingBottom = `${extraPadding}rem`;
  });
}
window.addEventListener('load', padToBaselineGrid);
window.addEventListener('resize', padToBaselineGrid);
