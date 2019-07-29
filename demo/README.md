# Sample application for TreeView

## Building

Install
- node and npm
- Elm 0.19
- Run `npm install` to grab node stuff

Build
- `npm run build`
- built app distribution is under `dist/`


## Local config JSON (experimental)

Manually copy stuff
- `src/data/*` to `dist/data/`

*TODO:* Figure out how to do that with webpack.config.js.

Install node module `http-server`, locally.

This is needed because Firefox (and possibly other browsers) will not satisfy
an HTTP request to a local file anymore, see
https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors/CORSRequestNotHttp .
- Elm makes, on startup, an HTTP request to load that file,
- but if Elm itself is loaded from file (as opposed to having it loaded
  from a web server), then that HTTP request will be to a file, which is
  not allowed anymore.

So we need to load everything via a simple HTTP server that just serves
anything there is in a folder.

Run `http-server`, e.g. by
```
> ./node_modules/.bin/http-server dist
```

Point your browser to localhost:8080, and voila, JSON is loaded right
after app startup.
