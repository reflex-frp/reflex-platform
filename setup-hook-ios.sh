prependSearchPath() {
  NIX_CFLAGS_COMPILE="-F@out@/Developer/SDKs/System/Library/Frameworks -I@out@/Developer/SDKs/usr/include ${NIX_CFLAGS_COMPILE}"
}

preConfigureHooks+=(prependSearchPath)
