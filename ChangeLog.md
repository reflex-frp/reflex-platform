# Revision history for reflex-platform

## 2a34ca864a5b51bf0f9f6cf94faf8996db7d6a5c - 2019-04-02

* Move git "thunks" to `dep/` dirs within each overlay, rather than one big
  `dep/` directory at the top level. This does make checking out thunks a bit
  more involved, but has the benefit of untangling reflex-platform a bit and
  making it more modular.

* Many of the overlays have a `_dep` attribute with all the source derivations
  introduced by the overlay. This is just so `release.nix` can build all the
  sources, ensuring that these build-time dependencies also make it to the
  cache. This attribute is *not* intended to be overridden by consumers of
  reflex-platform. In particular it's possible the way we now push to the cache
  doesn't need this, and it will be removed entirely.

## daabd633155da624198ed3302551bb2e834d2ac0 - 2019-03-29

* Remove support for GHCs older than 8.4.
