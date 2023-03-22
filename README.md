# ghcdoc

Opens local haddock documentation for a named package. It is inconvenient to
navigate cabal/store/ghc-8.10.4/ which has directories like the following:

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

Furthermore, even if you do get the right package for your project here, the
browser may not run `.js` on `file://` links so haddock's "Quick Jump" doesn't
work. Furthermore, inter-package links in the hscolour source are less broken
when ghcdoc serves them.

## usage

You must have a cabal package that supports `cabal configure --write-ghc-environment-files=always` (which probably means using ghc8.4.4+). Before building your project, ask cabal to build local documentation by setting
the following in `~/.cabal/config`

     haddock
       hoogle: True
       html: True
       hyperlink-source: True
       quickjump: True

Then there are two ways to call ghcdoc:

```shell
>ls *.cabal
portages.cabal

# open the index page, also starts the server that keeps running
>ghcdoc &

# open index pages for listed packages. It would starts a server if needed
>ghcdoc base JuicyPixels
```

The index page looks like ![index page](screenshot_index.png)

There are a few options:

```
ghcdoc --help
The ghcdoc program

ghcdoc [OPTIONS] [ITEM]

Common flags:
  -e --envfile=ITEM
  -b --browser=ITEM       default xdg-open
  -p --port=INT
  -o --originalinstances  by default instances look like:
                            instances class Eq, class Ord
                          where class links to the class and Eq links to the
                          instance source
                          with this flag, they are spread over many lines like
                            instance Eq a => Eq (Maybe a)
                            instance Ord a => Ord (Maybe a)
  -n --noopen             suppress the default opening of the packageindex
                          page when no packages are specified
  -? --help               Display help message
  -V --version            Print version information
```

### haskell-language-server coc (neo)vim integration

With the following in your `~/.vimrc`, type `,M` in normal mode opens the haddock page for the identifier under the cursor:

```
nnoremap <silent> ,M :call <SID>open_documentation()<CR>
function! s:open_documentation()
        call CocAction('doHover')
        :normal wGkc2f/http://localhost:8000
        :normal 0gxw
endfunction
function! s:open_documentation_source()
        call CocAction('doHover')
        :normal wGc2f/http://localhost:8000
        :normal 0gxw
endfunction
```

#### how it works

haddock/haskell-language-server's hover has urls like:

> Documentation: file:///home/aavogt/.ghcup/ghc/8.10.7/share/doc/ghc-8.10.7/html/libraries/base-4.14.3.0/Data-Foldable.html#v:msum

The first `:normal` jumps into the hover window and changes the url into something like:

> http://localhost:8000/home/aavogt/.ghcup/ghc/8.10.7/share/doc/ghc-8.10.7/html/libraries/ghc-prim-0.6.1/GHC-Types.html#t:IO
> http://localhost:8000/home/aavogt/.cabal/store/ghc-8.10.7/happstack-server-7.8.0.2-e24c28cee67026bfec1c9dbb55c984ddaf1dc4a5e9662928507a7bebda56edeb/share/doc/html/Happstack-Server-Internal-Monads.html#t:ServerPartT

The second `:normal` opens that url, and `redirectPages :: ServerPartT IO Response` in `src/Server.hs` sends the above urls to the following:

> http://localhost:8000/ghc-prim0/GHC-Types.html#t:IO
> http://localhost:8000/happstack-server0/Happstack-Server-Internal-Monads.html#t:ServerPartT

# todo


Without -o, http://localhost:8000/base0/Data-Monoid.html#t:Monoid has `instances ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,` so the simplification code is not perfect

`-o` flag only applies to the very first ghcdoc called. Afterwards it will have no effect. Either
the url needs to reflect that flag, or the second ghcdoc

source links ex. to base from another package point to http://localhost:8000/base0/src instead of http://localhost:8000/base0/src/Data-Either.html#Either but this problem is in the original file and is therefore haddock or hscolour's problem

When you are not in a cabal project, open the most recent one used, or perhaps open the one
closest with respect to distance between pwds the (perhaps deletion is free)

After adding dependencies or changing configuration options, the index page could become stale

Many others see src/Server.hs

