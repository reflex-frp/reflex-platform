prependSearchPath() {
  NIX_CFLAGS_COMPILE="-F@out@/System/Library/Frameworks -F@out@/System/Library/PrivateFrameworks -I@out@/usr/include ${NIX_CFLAGS_COMPILE}"
}

preConfigureHooks+=(prependSearchPath)
