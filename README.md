# Live GHCi

Live GHCi lets you put comments in your program...

```haskell
-- 5*5 ==>
```

...that are evaluated whenever the file is saved:

```haskell
-- 5*5 ==> 25
```

If re-evaluation of all special comments is getting too slow, you can change a `==>` to a `=>` and it will be skipped.

## Setup

Clone and make it:

```
$ git clone https://github.com/brianhempel/live-ghci.git
$ cd live-ghci
$ make
```

You may have to install some packages with `cabal install package_name`. A binary will be produced in `./bin/live-ghci`. Copy or symlink it into your PATH.

Then in your Haskell project directory, start Live GHCi as follows:

```
$ live-ghci
```

Live GHCi watches all Haskell files in the current and deeper subdirectories for changes.