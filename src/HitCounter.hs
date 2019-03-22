{-

Hit counter

We’re going to provide an initial scaffold of a scotty application which
counts hits to specific URIs. It also prefixes the keys with a prefix
defined on app initialization, retrieved via the command line argu-
ments.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Function              (($))
import Data.IORef                 (IORef, atomicModifyIORef', newIORef)
import Data.Maybe                 (Maybe (Just, Nothing))
import Data.Text.Lazy             (Text)
import GHC.Integer                (Integer)
import GHC.Num                    ((+))
import System.Environment         (getArgs)
import System.IO                  (IO)
import Text.Show                  (show)
import Web.Scotty.Trans

import qualified Data.Map       as M
import qualified Data.Text.Lazy as TL

data Config = Config
  { -- that's one, one click!
    -- two...two clicks!
    -- Three BEAUTIFUL clicks! ah ah ahhhh
    _counts :: IORef (M.Map Text Integer)
  , _prefix :: Text
  }

{-

Stuff inside ScottyT is, except for things that escape via IO, ef-
fectively read-only so we can’t use StateT. It would overcomplicate
things to attempt to do so and you should be using a proper database
for production applications.

-}

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
  Nothing         -> (M.insert k 1 m, 1)
  Just n          -> (M.adjust (+ 1) k m, n + 1)

app                                             :: Scotty ()
app        = get "/:key" $ do
  unprefixed <- param "key"
  config     <- lift ask
  let key' = TL.append (_prefix config) unprefixed
  newInteger <- liftIO $ atomicModifyIORef' (_counts config) (bumpBoomp key')
  html $ TL.concat [ "<h1>Success! Count was: "
                   , TL.pack $ show (newInteger :: Integer)
                   , "</h1>"
                   ]

main :: IO ()
main         = do
  [prefixArg] <- getArgs
  counter     <- newIORef M.empty
  let prefix = TL.pack prefixArg
      config = Config counter prefix
      runR r = runReaderT r config
  scottyT 3000 runR app

{-

Code is missing and broken. Your task is to make it work, whatever
is necessary.

You should be able to run the server from inside of GHCi, passing
arguments like so:

Prelude> :main lol
Setting phasers to stun... (port 3000) (ctrl-c to quit)

You could also build a binary and pass the arguments from your
shell, but do what you like. Once it’s running, you should be able to
bump the counts like so:

$ curl localhost:3000/woot
<h1>Success! Count was: 1</h1>
$ curl localhost:3000/woot
<h1>Success! Count was: 2</h1>
$ curl localhost:3000/blah
<h1>Success! Count was: 1</h1>

Note that the underlying “key” used in the counter when you GET
/woot is "lolwoot" because we passed ”lol” to main. For a giggle, try the
URI for one of the keys in your browser and mash refresh a bunch.
If you get stuck, consider checking for examples such as the reader
file in scotty’s examples directory of the git repository.

-}
