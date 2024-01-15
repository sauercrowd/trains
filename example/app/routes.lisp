(in-package :trains-example)

(route :get "/user" :users :index)
(route :get "/user/:id" :users :show)
