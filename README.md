# haskell-elm01-sample

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

# Run only server

```
stack exec haskell-elm01-sample-exe
```
