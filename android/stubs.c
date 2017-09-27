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

// start jsaddle.h
typedef struct jsaddleCallbacks {
  void (* jsaddleStart) ();
  void (* jsaddleResult) (const char *);
  char * (* jsaddleSyncResult) (const char *);
  char * jsaddleJsData;
  char * jsaddleHtmlData;
} jsaddleCallbacks;

typedef struct firebaseCallbacks {
  void (* firebaseInstanceIdService_sendRegistrationToServer) (const char *);
}
// end jsaddle.h

// start focus.h
void __stginit_Main (void);

int start_logger(const char *app_name);

void evaluateJavascriptWrapper (const char* js);
// end focus.h

extern StgClosure ZCMain_main_closure;
static jsaddleCallbacks* hsCallbacks = NULL;
static jobject javaCallback = NULL;
static jmethodID evaluateJSCallback = NULL;
static jmethodID mainStartedCallback = NULL;
static jmethodID mainFinishedCallback = NULL;

static int pfd[2];
static pthread_t thr;
static const char *tag = "";

JNIEXPORT void JNICALL Java_systems_obsidian_LocalFirebaseInstanceIDService_handleDeviceToken ( JNIEnv *env, jobject thisObj, jstring token ) {
  if(hsAppCallbacks.firebaseInstanceIdService_sendRegistrationToServer) {
    const char *cstring_token = (*env)->GetStringUTFChars(env, token, 0);
    hsAppCallbacks.firebaseInstanceIdService_sendRegistrationToServer(cstring_token);
    (*env)->ReleaseStringUTFChars(env, token, cstring_token);
  }
  return;
}

JNIEXPORT void JNICALL Java_org_reflexfrp_JSaddleShim_processMessage (JNIEnv *env, jobject thisObj, jstring msg) {
  const char *msg_str = (*env)->GetStringUTFChars(env, msg, NULL);
  (*(hsCallbacks->jsaddleResult))(msg_str);
  (*env)->ReleaseStringUTFChars(env, msg, msg_str);
  return;
}

JNIEXPORT jstring JNICALL Java_org_reflexfrp_JSaddleShim_processSyncMessage (JNIEnv *env, jobject thisObj, jstring msg) {
  const char *msg_str = (*env)->GetStringUTFChars(env, msg, NULL);
  char *next_str = (*(hsCallbacks->jsaddleSyncResult))(msg_str);
  jstring next_jstr = (*env)->NewStringUTF(env,next_str);
  free(next_str);
  (*env)->ReleaseStringUTFChars(env, msg, msg_str);
  return next_jstr;
}

JNIEXPORT void JNICALL Java_org_reflexfrp_JSaddleShim_injectJavascript (JNIEnv *env) {
  __android_log_write(ANDROID_LOG_DEBUG, "JSADDLEC", "injectJavascript");
  jstring js_str = (*env)->NewStringUTF(env, hsCallbacks->jsaddleJsData);
  (*env)->CallVoidMethod(env, javaCallback, evaluateJSCallback, js_str);
  (*env)->DeleteLocalRef(env, js_str);
  __android_log_write(ANDROID_LOG_DEBUG, "JSADDLEC", "injectJavascript done");
  return;
}

JNIEXPORT void JNICALL Java_org_reflexfrp_JSaddleShim_startProcessing (JNIEnv *env) {
  (*(hsCallbacks->jsaddleStart))();
  return;
}

JNIEXPORT void JNICALL Java_systems_obsidian_LocalFirebaseMessagingService_handleNotification ( JNIEnv *env, jobject thisObj, jstring intent, jstring notificationdata) {
  if(hsAppCallbacks.mainActivity_onNewIntent) {
    const char *cstring_intent = (*env)->GetStringUTFChars(env, intent, 0);
    const char *cstring_notificationdata = (*env)->GetStringUTFChars(env, notificationdata, 0);
    hsAppCallbacks.mainActivity_onNewIntent(cstring_intent, cstring_notificationdata);
    (*env)->ReleaseStringUTFChars(env, intent, cstring_intent);
    (*env)->ReleaseStringUTFChars(env, notificationdata, cstring_notificationdata);
  }
  return;
}


void evaluateJavascript (const char* js) {
  JNIEnv *env;
  jint attachResult = (*jvm)->AttachCurrentThread(jvm, &env, NULL);
  assert (attachResult == JNI_OK);
  jstring js_str = (*env)->NewStringUTF(env, js);
  (*env)->CallVoidMethod(env, javaCallback, evaluateJSCallback, js_str);
  (*env)->DeleteLocalRef(env, js_str);
  //  jint detachResult = (*jvm) -> DetachCurrentThread(jvm); //TODO: I think this is unnecessary
  //  assert (detachResult == JNI_OK);
  return;
}
