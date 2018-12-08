# haskell-elm01-sample

- server side is almost copied from [this repo](https://github.com/matsubara0507/haskell-and-elm/tree/unuse-extensible)

# Requirements

- [haskell stack](https://docs.haskellstack.org)
- [yarn](https://yarnpkg.com)
- [elm](https://elm-lang.org) (no need to install manually, ```yarn install``` will do)

# Install

```
git clone https://github.com/reouno/todo-app-haskell-elm.git
cd todo-app-haskell-elm
stack build
cd simple-todo-app
yarn install
elm-package install
```

# Run the app

```
cd simple-todo-app
yarn start
```

access to [http://localhost:3000](http://localhost:3000)

# Run only server

```
stack exec haskell-elm01-sample-exe
```

get resource with [http://localhost:8081/todos](http://localhost:8081/todos)
