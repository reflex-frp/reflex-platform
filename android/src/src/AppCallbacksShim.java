package org.reflexfrp;

import android.webkit.JavascriptInterface;
import android.webkit.WebView;
import android.os.Handler;

import android.util.Log;

public class AppCallbacksShim {
  public native void mainActivityOnCreate();
  public native void mainActivityOnStart();
  public native void mainActivityOnResume();
  public native void mainActivityOnPause();
  public native void mainActivityOnStop();
  public native void mainActivityOnDestroy();
  public native void mainActivityOnRestart();
  public native void mainActivityOnNewIntent(String intent, String intentdata);
}
