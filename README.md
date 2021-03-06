# The Monad Shop

This is an example for a [Yesod](https://www.yesodweb.com/) web application with Instana integration via the [Instana Haskell Trace SDK](http://hackage.haskell.org/package/instana-haskell-trace-sdk).
You can also buy the best monads in town here, and at a bargain price, too!

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

## Development

### Local Database Setup

After installing Postgres, run the following (in a shell, not in psql):

```
createuser --pwprompt --superuser monad-shop # use monad-shop as password when prompted
createdb monad-shop
createdb monad-shop_test
```

### Development Server

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

