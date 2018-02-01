#ifndef HASKELLACTIVITY_H_INCLUDED
#define HASKELLACTIVITY_H_INCLUDED

#include <jni.h>

typedef struct ActivityCallbacks {
  void (*onCreate) (); //TODO: Support savedInstanceState
  void (*onCreateWithIntent) (const char*, const char*); //TODO: Support savedInstanceState
  void (*onStart) ();
  void (*onResume) ();
  void (*onPause) ();
  void (*onStop) ();
  void (*onDestroy) ();
  void (*onRestart) ();
  void (*onNewIntent) (const char *, const char *); //TODO: Pass the whole argument and use JNI
} ActivityCallbacks;

extern JavaVM* HaskellActivity_jvm;

#endif
