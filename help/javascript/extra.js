(function () {
  const LANG_PREF_KEY = 'lang_pref';
  const LANG_REDIRECTED_KEY = 'lang_redirected';
  const FAQ_REDIRECTED_KEY = 'faq_redirected'
  const SELECTOR = 'a[hreflang][href]';

  function findCommonULAncestor(nodes) {
    const arr = Array.from(nodes || []);
    if (arr.length === 0) return null;

    // Ensure nodes are elements and belong to this document
    if (!arr.every(n => n instanceof Element && document.documentElement.contains(n))) return null;

    // Get UL ancestors of the first node (array of ancestor elements for a node (closest first, up to <html>))
    const firstULs = [];
    let p = arr[0].parentElement;
    while (p) {
      if (p.tagName && p.tagName.toLowerCase() === 'ul') firstULs.push(p);
      p = p.parentElement;
    }
    if (firstULs.length === 0) return null; // First node has no ul ancestor at all

    // For each UL ancestor of first node (nearest first), check if it contains all nodes
    for (const ul of firstULs) {
      let ok = true;
      for (let i = 1; i < arr.length; i++) {
        if (!ul.contains(arr[i])) { ok = false; break; }
      }
      if (ok) return ul;
    }

    // No common UL ancestor found
    return null;
  }

  // Build the map and attach listeners
  function buildMapAndBind() {
    const anchors = Array.from(document.querySelectorAll(SELECTOR));
    if (!anchors.length) return null;

    // Language map: hreflang => href
    const map = {};

    anchors.forEach(a => {
      const hreflang = a.getAttribute('hreflang');
      const href = a.getAttribute('href');
      if (!hreflang || !href) return;
      map[hreflang.toLowerCase()] = href;
    });

    // Bind click listeners only to those anchors who are children of a common <ul> ancestor
    const ul = findCommonULAncestor(anchors);

    if (ul) {
      anchors.forEach(a => {
        if (!ul.contains(a)) return;

        const hreflang = a.getAttribute('hreflang');
        if (!hreflang) return;

        a.addEventListener('click', () => { setSaved(LANG_PREF_KEY, hreflang.toLowerCase()); }, { passive: true });
      });
    } else {
      // No single container: do NOT attach click listeners (avoids accidental saves)
    }

    return map;
  }

  function getSaved(key) {
    try { return localStorage.getItem(key); } catch (e) { return null; }
  }
  function setSaved(key, hreflang) {
    try { localStorage.setItem(key, hreflang); } catch (e) { }
  }
  function removeSaved(key) {
    try { localStorage.removeItem(key); } catch (e) { }
  }

  // Pick browser preference: exact region first, then primary-sub-tag fallback
  function getBrowserLang(langIds) {
    const browserLangs = (navigator.languages && navigator.languages.length)
      ? [...navigator.languages, navigator.language].filter(Boolean)
      : [navigator.language || navigator.userLanguage || ''];

    for (let lang of browserLangs) {
      if (!lang) continue;
      lang = lang.toLowerCase();

      // Exact match (pt-br)
      if (langIds.includes(lang)) return langIds.find(k => k === lang);

      // Primary-sub-tag (pt)
      const primary = lang.split('-')[0];
      if (langIds.includes(primary)) return primary;

      // Multiple candidates with same primary: pick one (prefer region if any)
      const candidates = langIds.filter(k => k.split('-')[0] === primary);
      if (candidates.length === 1) return candidates[0];
      if (candidates.length > 1) {
        const withRegion = candidates.find(c => c.includes('-'));
        return withRegion || candidates[0];
      }
    }
    return null;
  }

  // If there's an FAQ hash fragment on the root path, send the visitor to the new path + same query + same hash
  function redirectFAQ() {
    if (sessionStorage.getItem(FAQ_REDIRECTED_KEY)) return false
    sessionStorage.setItem(FAQ_REDIRECTED_KEY, 'true');

    if (!/^\/permissionmanagerx\/help\/?$/.test(location.pathname.toLowerCase())) return false;

    if (/^#faq\d+$/.test(location.hash)) {
      location.href = '/PermissionManagerX/help/faqs/' + location.search + location.hash
      return true;
    }

    return false;
  }

  function init() {
    if (redirectFAQ()) return;

    const map = buildMapAndBind();
    if (!map || !Object.keys(map).length) return;

    if (sessionStorage.getItem(LANG_REDIRECTED_KEY)) return
    sessionStorage.setItem(LANG_REDIRECTED_KEY, 'true');

    let langId = getSaved(LANG_PREF_KEY);

    if (langId && !map[langId]) {
      removeSaved(LANG_PREF_KEY);
      langId = null;
    }

    if (!langId) langId = getBrowserLang(Object.keys(map));

    if (!langId) return;

    const selectedHref = map[langId];

    const normPath = p => {
      p = p.replace(/\/\/+/g, '/');
      if (!p.startsWith('/')) p = '/' + p;
      if (!p.endsWith('/')) p += '/';
      return p;
    };

    const newPath = normPath((new URL(selectedHref, location)).pathname)

    if (normPath(location.pathname) != newPath) {
      location.href = newPath + location.search + location.hash;
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
