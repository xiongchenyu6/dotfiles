// Publishes the Matrix delegation documents on the apex, so identifiers can
// read @user:starslab.qzz.io while the homeserver itself runs on HOMESERVER.
//
// The apex is a Cloudflare Pages site this repository does not build, so the
// two /.well-known/matrix documents are served by this Worker on a route
// scoped to that path prefix. Everything else on the apex is untouched.
//
// Keep HOMESERVER in sync with tuwunel_host_name in ../vars/main.yml.

const CORS = {
  // The client document is fetched cross-origin by web clients (Element and
  // friends), which the specification requires servers to allow.
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type',
};

function json(body) {
  return new Response(JSON.stringify(body), {
    headers: {
      'Content-Type': 'application/json',
      'Cache-Control': 'public, max-age=3600',
      ...CORS,
    },
  });
}

export default {
  fetch(request, env) {
    const homeserver = env.HOMESERVER;
    const { pathname } = new URL(request.url);

    if (request.method === 'OPTIONS') {
      return new Response(null, { status: 204, headers: CORS });
    }
    if (request.method !== 'GET' && request.method !== 'HEAD') {
      return new Response('Method Not Allowed', { status: 405, headers: { Allow: 'GET, HEAD, OPTIONS' } });
    }

    switch (pathname) {
      case '/.well-known/matrix/client':
        return json({ 'm.homeserver': { base_url: `https://${homeserver}` } });

      // Federation would otherwise default to port 8448, which the Cloudflare
      // edge in front of this zone does not proxy.
      case '/.well-known/matrix/server':
        return json({ 'm.server': `${homeserver}:443` });

      case '/.well-known/matrix/support':
        return json({ contacts: [], support_page: `https://${homeserver}` });

      default:
        return new Response('Not Found', { status: 404, headers: CORS });
    }
  },
};
