# Trains
A rails-like framework written for Common Lisp.
![Trains logo](./logo.png)

Comes out the box with
- Rails-like MVC
- SQlite support and migrations
- htmx
- tailwind
- authentication
- Dockerfile

It's closely modeled after rails, bringing everything necessary to build a web app so you can get your results into a browser.

## Quickstart
1. Setup quicklisp and a compiler of your choice (tested with SBCL)
2. Create a new project with
```bash
$ cd ~/quicklisp/local-projects # so can load your new project
$ sbcl --eval "(ql:quickload 'trains:create)" my-app
```
3. `cd my-app`
4. `sbcl --eval "(ql:quickload :my-app)" --eval "(my-app:main-fn)"`
5. Head to `http://localhost:8080` in your browser

