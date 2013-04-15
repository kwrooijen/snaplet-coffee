Snaplet Coffee
==============

Snaplet Coffee is a snaplet for the Haskell web framework Snap.
It will let you easily use CoffeeScript in your Snap web applications.

This snaplet is based off of the [Snaplet-Fay](https://github.com/faylang/snaplet-fay) And I've made it so that they basically work the same way.



Example Usage
------------

Practically copied from [Snaplet-Fay](https://github.com/faylang/snaplet-fay)

Site.hs:
```
import Snap.Snaplet.Coffee

routes = [..., ("/coffee", with coffee coffeeServe)]

app :: SnapletInit App App
app = makeSnaplet "app" "A snaplet example application." Nothing $ do
  coffee' <- nestSnaplet "coffee" coffee initCoffee
  return $ App { _coffee = coffee' }
```

Application.hs:
```
import Snap.Snaplet.Coffee

data App = App { _coffee :: Snaplet CoffeeScript }

makeLenses ''App
```

Run your application now.

A snaplet config file will be generated at snaplets/coffee/devel.cfg the first time your application initializes the snaplet. The defaults are the recommended ones for development.

Place your CoffeeScript .coffee files in snaplets/coffee/coffee. Note that a default devel.cfg will not be created if you have already created the coffee directory. If this happens to you, move snaplets/coffee, start your application, and then move the files back into snaplets/coffee.


Any requests to the specified directory (in this case /coffee/) will compile the appropriate CoffeeScript file and serve it.
