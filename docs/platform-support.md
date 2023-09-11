# Supported Platforms

reflex-platform supports building *on* x86_64 and aarch64 linux and macOS systems. It supports building *for* Android, iOS, Javascript, Linux, and macOS targets.

Depending on the build system you are using, you can build for the following targets:

|               | Javascript | Android | iOS | Linux x86_64 | Linux aarch64 | macOS (intel) | macOS (m1/m2) |
|---------------|------------|---------|-----|--------------|---------------|---------------|---------------|
| Linux x86_64  | ✅         | ✅      |     | ✅           |               |               |               |
| Linux aarch64 | ✅         | ✅      |     |              | ✅            |               |               |
| macOS (intel) | ✅         |         | ✅  |              |               | ✅            |               |
| macOS (m1/m2) | ✅         |         | ✅  |              |               |               | ✅            |

* On aarch64 platforms (aarch64-linux and aarch64-darwin), we do not support ghc-8.6.5 or ghcjs-8.6.5.
* From Linux, you can't build for iOS or macOS because the apple toolchain is restricted to macOS.
* Android builds aren't yet supported from macOS.
* When building a desktop app on x86_64-linux you can use webkitgtk, but that isn't yet supported on aarch64-linux. You can, however, build your backend executable and warp-based frontend executables on either architecture. 
* When building a desktop app on macOS you can use wkwebview or warp.
* 32-bit android builds are only supported via ghc-8.6.5 because later versions of ghc don't support that platform.

## Binary Caches

Binary caches are provided for x86_64-linux, x86_64-darwin, and aarch64-darwin. We will be adding an aarch64-linux cache in the future.
