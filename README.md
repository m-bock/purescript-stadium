# purescript-stadium

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png" />
    <source media="(prefers-color-scheme: light)" srcset="assets/logo-light.png" />
    <img alt="Stadium logo" src="assets/logo-light.png" width="560">
  </picture>
</p>

stadium in a nutshell:

1. Write your state machines in **PureScript**
2. Use it as regular Hook in a **React** app

See [this project](https://github.com/m-bock/gcode-viewer) as usage example.

## Choosing the monad

Your operations run in whatever monad you name, and the library lifts
its own into it. `Effect` is the plain choice:

```purescript
dispatchers :: Stadium.DispatcherApi Effect Msg State Unit -> Dispatchers
```

`Aff` is worth it once the operations make requests, because otherwise
each one needs a `launchAff_` of its own and the module that should only
know about states and messages ends up knowing about fibers. React still
wants `Effect`, so the two meet at the edge instead:

```purescript
dispatchers: ops >>> \o -> { countUp: launchAff_ o.countUp }
```

Anything with a `MonadEffect` instance works, `Run (EFFECT + r)`
included. `onUpdate` stays in `Effect`: the library calls that one
rather than handing it out, and running an arbitrary `m` back to
`Effect` would need a runner it has no way to get.
