# declarative-ddl

WIP

Declarative database schemas in Clojure data structures.  Create Postgresql (maybe other DBs to come) migrations from those declared schemas when the change.  Similarly to how Django and Rails models are declared by the developer and migrations are created from those.

Also creates clojure.spec.alpha schemas from the field definitions in the schema declaration.

## Usage

### automated testing

- clj `$ lein test`
- cljs `$ lein doo node test`

### CLJS

#### run figwheel repl

- open `./figwheel.html` file in browser
- `$ lein figwheel repl`

## License

Copyright © 2019 Frank Henard

MIT
