# ghcdoc

## usage

When you have a cabal package that supports `cabal configure --write-ghc-environment-files=always` (probably ghc8.4.4+):

Generate local documentation setting the following in `~/.cabal/config`

     haddock
       hoogle: True
       html: True
       hyperlink-source: True
       quickjump: True

Then run it

```shell
>ls *.cabal
portages.cabal

# see all dependencies
>ghcdoc
```

```
# or if you know which packages
>ghcdoc base JuicyPixels
```


### todo

when you are not in a cabal project, open the most recent one used, or perhaps open the one
closest with respect to distance between pwds the (perhaps deletion is free)

after adding dependencies or changing configuration options, the index page could become stale

## why

It is inconvenient to navigate cabal/store/ghc-8.10.4/ which has directories like

```
active-0.2.0.14-654db471a95e22980fb0d3b3d7cec63011e700774c9c5a3211a049f9f528295a/
active-0.2.0.14-6883a704d312a6ea3f081cd18a143566eb4c5fcd8fdfb6ccfefb7ed538b84cd8/
adjunctions-4.4-3aabe59fdc0aecdac7fedfdfeddb7a224aeb48182eb612d08ac9c759810e58f9/
adjunctions-4.4-78971b508863d00b0b99318bbcd79b8a5c2c4283c475502cdda830d02e38a42e/
aeson-1.5.6.0-1400182fb6135a245119f175b183386c2e2643075c1b26232f7d33d7c4a0a1fd/
aeson-1.5.6.0-2f2faf03923cd0fc0ab5211880ad2cdadd12bd5c666566be3af8ad1480e017d3/
aeson-1.5.6.0-629873db2839631743eb375cd1eaea7f8e731ec4e104ea17d5149b368c58a486/
aeson-1.5.6.0-6c0cb681b94efe7dcc84c9c01346ffb127b3e5fcc679006c658777bbceee143a/
```

Furthermore many browsers don't run `.js` on `file://` links, so we open the files through `http://localhost`
