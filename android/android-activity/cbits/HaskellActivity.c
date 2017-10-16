#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <unistd.h>
#include <jni.h>
#include <stdio.h>
#include <android/log.h>
#include <setjmp.h>
#include "HsFFI.h"
#include "Rts.h"
#include "HaskellActivity.h"

// From ghc/rts/RtsStartup.c
extern void (*exitFn)(int);

extern StgClosure ZCMain_main_closure;

//TODO: Get everything above this from an existing header file if possible

JavaVM* HaskellActivity_jvm = NULL;

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnCreate (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onCreate) {
    callbacks->onCreate();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnStart (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onStart) {
    callbacks->onStart();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnResume (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onResume) {
    callbacks->onResume();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnPause (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onPause) {
    callbacks->onPause();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnStop (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onStop) {
    callbacks->onStop();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnDestroy (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onDestroy) {
    callbacks->onDestroy();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnRestart (JNIEnv *env, jobject thisObj, jlong callbacksLong) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onRestart) {
    callbacks->onRestart();
  }
}

JNIEXPORT void JNICALL Java_systems_obsidian_HaskellActivity_haskellOnNewIntent (JNIEnv *env, jobject thisObj, jlong callbacksLong, jstring intent, jstring intentdata) {
  const ActivityCallbacks *callbacks = (const ActivityCallbacks *)callbacksLong;
  if(callbacks->onNewIntent) {
    const char *cstring_intent = (*env)->GetStringUTFChars(env, intent, 0);
    const char *cstring_intentdata = (*env)->GetStringUTFChars(env, intentdata, 0);
    callbacks->onNewIntent(cstring_intent, cstring_intentdata);
    (*env)->ReleaseStringUTFChars(env, intent, cstring_intent);
    (*env)->ReleaseStringUTFChars(env, intentdata, cstring_intentdata);
  }
}

static jmp_buf mainJmpbuf;

// setjmp returns 0 on its initial call, but we want to be able to return all
// exit codes (0 - 255) through it.  So, we offset them by this amount and then
// subtract it off later.
#define EXIT_CODE_OFFSET 0x10000

static void mainFinished(int exitCode) {
	longjmp(mainJmpbuf, exitCode + EXIT_CODE_OFFSET);
}

// Filled in by Java_systems_obsidian_HaskellActivity_haskellStartMain and disposed
// of by haskellActivityContinueWithCallbacks
static jobject haskellActivity = 0;
static jobject setCallbacksQueue = 0;

jobject HaskellActivity_get() {
  return haskellActivity;
}

// Continue constructing the HaskellActivity.
// WARNING: This may only be invoked once per Haskell 'main' invocation
void HaskellActivity_continueWithCallbacks(const ActivityCallbacks *callbacks) {
  assert(haskellActivity);

  JNIEnv *env;
  jint attachResult = (*HaskellActivity_jvm)->AttachCurrentThread(HaskellActivity_jvm, &env, NULL);
  assert(attachResult == JNI_OK);

  jclass cls = (*env)->GetObjectClass(env, haskellActivity);
  jmethodID continueWithCallbacks = (*env)->GetStaticMethodID(env, cls, "continueWithCallbacks", "(Ljava/util/concurrent/SynchronousQueue;J)V");
  assert(continueWithCallbacks);

  (*env)->CallStaticVoidMethod(env, cls, continueWithCallbacks, setCallbacksQueue, callbacks);
  if((*env)->ExceptionOccurred(env)) {
    __android_log_write(ANDROID_LOG_DEBUG, "HaskellActivity", "continueWithCallbacks exception");
    (*env)->ExceptionDescribe(env);
  }

  //TODO: Provide a better way of having access to this
  // (*env)->DeleteGlobalRef(env, haskellActivity);
  // haskellActivity = 0;

  (*env)->DeleteGlobalRef(env, setCallbacksQueue);
  setCallbacksQueue = 0;
}

JNIEXPORT int JNICALL Java_systems_obsidian_HaskellActivity_haskellStartMain (JNIEnv *env, jobject thisObj, jobject setCallbacksQueue_) {
  // Retain the HaskellActivity that we're running under
  haskellActivity = (*env)->NewGlobalRef(env, thisObj);
  setCallbacksQueue = (*env)->NewGlobalRef(env, setCallbacksQueue_);

  // Override Haskell's exit behavior so that it returns here instead of exiting
  // the program when 'main' finishes
  exitFn = mainFinished;
  int exitCode;
  if(exitCode = setjmp(mainJmpbuf)) {
    return exitCode - EXIT_CODE_OFFSET;
  }

  static int argc = 5;
  static char *argv[] = {"jsaddle", "+RTS", "-N2", "-I0", "-RTS"};

  RtsConfig rts_opts = defaultRtsConfig;
  rts_opts.rts_opts_enabled = RtsOptsAll;

  hs_main(argc, argv, &ZCMain_main_closure, rts_opts);

  return 0; // Should never hit this
}

////////////////////////////////////////////////////////////////////////////////
// Logger
////////////////////////////////////////////////////////////////////////////////
// Converts output on stdout and stderr to android log messages

typedef struct loggerConfig {
  int pipe;
  const char *tag;
} loggerConfig;

void *loggerThread(void *cfgVoid) {
  assert(cfgVoid);
  const loggerConfig *cfg = (const loggerConfig *)cfgVoid;
  ssize_t rdsz;
  char buf[256];

  __android_log_write(ANDROID_LOG_DEBUG, cfg->tag, "Logger running");
  while((rdsz = read(cfg->pipe, buf, sizeof buf - 1)) > 0) {
    //TODO: This elides '\n's at the end of the buffer, but what should we do with ones in the middle?
    if(buf[rdsz - 1] == '\n')
      --rdsz;

    buf[rdsz] = 0; // Add null terminator
    __android_log_write(ANDROID_LOG_DEBUG, cfg->tag, buf);
  }
}

// Start the logging thread.  tag will not be copied, and must continue to
// be available indefinitely.
// Note: if the thread is killed, the loggerConfig will be leaked
void startLogger(const char *tag) {
  int pfd[2];

  setvbuf(stdout, 0, _IOLBF, 0);
  setvbuf(stderr, 0, _IOLBF, 0);

  // Create the pipe and redirect stdout and stderr
  pipe(pfd);
  dup2(pfd[1], 1);
  dup2(pfd[1], 2);

  loggerConfig *cfg = malloc(sizeof(loggerConfig));
  cfg->pipe = pfd[0];
  cfg->tag = tag;

  pthread_t thr;
  // Spawn the logging thread
  if(pthread_create(&thr, 0, loggerThread, cfg) == -1) {
      __android_log_write(ANDROID_LOG_DEBUG, tag, "Could not start logger");
      assert(false);
  }
  pthread_detach(thr);
}

////////////////////////////////////////////////////////////////////////////////
// JNI Initialization
////////////////////////////////////////////////////////////////////////////////

JNIEXPORT jint JNICALL JNI_OnLoad ( JavaVM *vm, void *reserved ) {
  HaskellActivity_jvm = vm;

  startLogger("HaskellActivity"); //TODO: Use the app name

  return JNI_VERSION_1_6;
}
