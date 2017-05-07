prependSearchPath() {
  export NIX_@target_prefix@_CFLAGS_COMPILE="-F@out@/System/Library/Frameworks -F@out@/System/Library/PrivateFrameworks -I@out@/usr/include ${NIX_@target_prefix@_CFLAGS_COMPILE}"
}

preConfigureHooks+=(prependSearchPath)
