package org.reflexfrp;

import android.webkit.JavascriptInterface;
import android.webkit.WebView;
import android.os.Handler;
import java.util.concurrent.SynchronousQueue;
import java.util.function.Consumer;

import android.util.Log;

public class JSaddleShim {
  private WebView wv;
  private Handler hnd;
  private Consumer<Integer> onMainFinished;
  SynchronousQueue done = new SynchronousQueue<Integer>(); // The Int is unused, but we can't use Void because SynchronousQueue doesn't allow null items
  private native void startProcessing();
  private native void processMessage(String msg);
  private native String processSyncMessage(String msg);

  public native int init();
  public native void deinit();
  public native void injectJavascript();

  public JSaddleShim(WebView _wv, Handler _hnd, Consumer<Integer> _onMainFinished) {
    wv = _wv;
    hnd = _hnd;
    onMainFinished = _onMainFinished;
  }

  public void startMain() {
    new Thread() {
      public void run() {
        final int exitCode = init();
        Log.d("JSADDLE", String.format("haskell main exited with code %d", exitCode));
        try {
          done.put(0); // Since Haskell's main has exited, it won't call mainStarted.  Instead, we unblock the main thread here.
        } catch(InterruptedException e) {
          //TODO: Should we do something with this?
        }
        hnd.post(new Runnable() {
          @Override
          public void run() {
            onMainFinished.accept(exitCode);
          }
        });
      }
    }.start();
    try {
      done.take();
    } catch(InterruptedException e) {
      //TODO: Should we do something with this?
    }
  }

  public void mainStarted() {
    try {
      done.put(0); // This value is not significant, but SynchronousQueue requires that it not be null
    } catch(InterruptedException e) {
      //TODO: Should we do something with this?
    }
  }

  public void evaluateJavascript(final String js) {
    hnd.post(new Runnable() {
      @Override
      public void run() {
        // Log.d("JSADDLE", js);
        wv.evaluateJavascript (js, null);
      }
    });
  }

  @JavascriptInterface
  public boolean postMessage(final String msg) {
    hnd.post(new Runnable() {
      @Override
      public void run() {
        // Log.d("JSADDLE", msg);
        processMessage(msg);
      }
    });
    return true;
  }

  @JavascriptInterface
  public String syncMessage(final String msg) {
    // Log.d("JSADDLE", msg);
    return processSyncMessage(msg);
  }

  @JavascriptInterface
  public boolean postReady() {
    hnd.post(new Runnable() {
      @Override
      public void run() {
        // Log.d("JSADDLE", "###startProcessing");
        startProcessing();
      }
    });
    return true;
  }

}
