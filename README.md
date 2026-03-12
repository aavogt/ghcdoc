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

# type search

> +T means True is in the Set
> -T means False is in the Set
> T means anything is in the Set
> T- means T is not in the set
> +T- means?
> -T- means?
> `-+T` distinguished from `T`?
> T+ distinguished from T?

Type parameters also pose a question. `D a` might prompt for searches for `D` with or without a particular type instead of `a`. `(Maybe Int)-` makes sense, as does `(Maybe (Int-))`, but `(Maybe (Int-))-` means Int except inside Maybe? How should operator precedence work?


# todo

 - [ ] when the package is just an executable, only ghc-supplied libraries are listed
 - [ ] polarity search
        - [x] parse hoogle, parse query language
        - [x] search for concrete types in concrete function signatures
        - [ ] data types to describe data constructors, type families, type classes, fill these
        - [ ] run ghci to get :info for type families. Or get it from HI, HIE files
        - [ ] unification-fd for instance selection, type variable searching
                - [x] TypF
                - [ ] 
        - [ ] query language type parameters (variables or concrete)
        - [ ] MPTC


 - [ ] Without -o, http://localhost:8000/base0/Data-Monoid.html#t:Monoid has `instances ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,` so the simplification code is not perfect
 - [ ] `-o` flag only applies to the very first ghcdoc called. Afterwards it will have no effect. Either
the url needs to reflect that flag, or the second ghcdoc
 - [ ] source links ex. to base from another package point to http://localhost:8000/base0/src instead of http://localhost:8000/base0/src/Data-Either.html#Either but this problem is in the original file and is therefore haddock or hscolour's problem
 - [ ] When you are not in a cabal project, open the most recent one used, or perhaps open the one
closest with respect to distance between pwds the (perhaps deletion is free)
 - [ ] multiple cabal projects. Either pick a new port or somehow serve both.
 - [ ] use the haddock --qual=full or aliased and then shorten them?
 - After adding dependencies or changing configuration options, the index page could become stale

Many others see src/Server.hs


## classes and polarity search

Attempt 3:

instance C a => D a
instance C Int
if I am searching for D Int,

query Int+

First candidate:
f :: D a => a
first find the substitution that makes `a` become `Int`. I would like to have explicit manipulation of the substitution but that is not how unification-fd represents it. So I have to change my mental representation of the operation to use `subsumes` or infix: 

> do
>   -- turn "C a => D a" into a "C a" and a "D a" such that "a" is the same variable
>   ...
>   True <- "D a" <:= "D Int"
>   -- where 

Second Candidate:
g :: D a => Double -> (a, Char)
try to find the `Int` inside Double (not present),
inside (a,Char) leads to a 
     search for the constructor (,) :: a -> b -> (a,b) which in turn leads to searches inside `a`, inside Char (not present)
search inside `a` means `a` <:= Int
must be true, Or D has an instance of a type that contains an `Int`. The latter seems too difficult?
 I need to access dataCons and instanceTable

I am not interested if the type variable could be instantiated without using any class instances. Why? Parametricity prevents the function from doing much with the value. But what if `f :: E Int` is supposed to be found and `data E a = E a a`. That one is supposed to be found without a problem. The problem comes up when 



Attempt 2:
Each function has class constraints C1 through Cn. f :: (C1 a b, C2 .., CN d) => t
I want to find out if there are any instances of those classes which eventually contain a concrete type T
So starting with T I can find all classes that have instances which unify with C T a etc.
How do I stop expanding

        class C a
        instance (C a, C b) => C (a,b)

In hopes of eventually finding a T? It is beyond me to search infinites and some degree
of expansion might be desirable (though not with my current simplifying assumtion in which
searching for multiple types gives those functions which could contain those types in separate instantiations and not simultaneously... this is more of a Jacobi iteration than a Gauss Seidel to use a poor analogy.

Attempt 1:
1. separate what comes before =>
2. unification-fd on what comes after =>
3. if it unifies (with a given member of the cxt tuple), then apply the substitution to what comes before =>, and then recurse. But I have a concrete type in mind, so how do I know which ones will eventually lead to my desired concrete type?
   Am I guaranteed to eventually terminate?
   Type classes usually shrink the instance head. If there is undecidableinstances there is no immediate proof of termination. But I think it would be acceptable to have a timeout/ -fcontext-reduction-stack that will be finite rather than asking the user to kill a process.
   I cannot directly copy what happens with type inference

Attempt 0:
1. identify the class name, here it is Data.Vector.Generic.Base.Vector
2. find the class and type signature of it's methods
3. substitute the instance head into the methods
4. treat the methods as free functions

 , Dat [ Name "V2" , Name "a" ]
, Exp
    (Fun
       [ Name "V2" ]
       (App
          HasType
          (Bang
             (Sym
                (Iden (Name "a"))
                (Symb "->")
                (Bang
                   (Sym
                      (Iden (Name "a"))
                      (Symb "->")
                      (App (Iden (Name "V2")) (Iden (Name "a")))))))))



# development

        $ ghcid -TServer.main --warnings -c 'cabal repl Server'
        $ ghcid -Ttest --warnings -c 'cabal repl test'

        these are quite slow they have to recompile everything twice

        $ ghcid -Ttest --warnings src/ParseHoogle.hs --reload hl3.txt # fast
        $ ghcid -r --reload test.txt src/Search.hs --warnings -c 'ghci -isrc' # also fast
