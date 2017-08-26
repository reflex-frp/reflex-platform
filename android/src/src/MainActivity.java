package systems.obsidian.focus;

import android.Manifest;
import android.content.pm.PackageManager;
import android.app.Activity;
import android.app.DownloadManager.Request;
import android.app.DownloadManager;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Process;
import android.os.SystemClock;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.ConsoleMessage;
import android.webkit.CookieManager;
import android.webkit.DownloadListener;
import android.webkit.URLUtil;
import android.webkit.ValueCallback;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.BiConsumer;
import org.reflexfrp.JSaddleShim;
import org.reflexfrp.AppCallbacksShim;

public class MainActivity extends Activity {
  private JSaddleShim jsaddle;
  private ValueCallback<Uri[]> fileUploadCallback;
  private AppCallbacksShim appCallbacks;

  static {
    System.loadLibrary("@APPNAME@");
  }

  private static final int REQUEST_CODE_FILE_PICKER = 51426;

  @Override
  public void onCreate(Bundle savedInstanceState)
  {
    Log.d("@APPNAME@", "onCreate");
    CookieManager.setAcceptFileSchemeCookies(true);
    super.onCreate(savedInstanceState);
    // Remove title and notification bars, obv.
    this.requestWindowFeature(Window.FEATURE_NO_TITLE);
    //this.getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);

    setContentView(R.layout.main);
    // find the web view
    final WebView wv = (WebView) findViewById (R.id.webview);
    // enable javascript and debugging
    WebSettings ws = wv.getSettings();
    ws.setJavaScriptEnabled(true);
    ws.setAllowFileAccessFromFileURLs(true); //Maybe you don't need this rule
    ws.setAllowUniversalAccessFromFileURLs(true);
    wv.setWebContentsDebuggingEnabled(true);
    Intent intent = getIntent();

    // allow video to play without user interaction
    wv.getSettings().setMediaPlaybackRequiresUserGesture(false);
    // Set a handler for download links
    wv.setDownloadListener(new DownloadListener() {
        public void onDownloadStart(final String url, String userAgent, final String contentDisposition, final String mimetype, long contentLength) {
          BiConsumer<String[], int[]> doDownload = new BiConsumer<String[], int[]>() {
              // permissions and grantResults will be null in the case where no permission was needed
              public void accept(String[] permissions, int[] grantResults) {
                Request request = new Request(Uri.parse(url));
                request.allowScanningByMediaScanner();
                request.setNotificationVisibility(DownloadManager.Request.VISIBILITY_VISIBLE_NOTIFY_COMPLETED);
                request.setDestinationInExternalPublicDir(Environment.DIRECTORY_DOWNLOADS, URLUtil.guessFileName(url, contentDisposition, mimetype));
                DownloadManager dm = (DownloadManager) getSystemService(DOWNLOAD_SERVICE);
                dm.enqueue(request);
              }
            };

          // Obtain permission to store downloaded files.
          if (checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
            requestPermissionsAndThen(new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE}, doDownload);
          } else {
            doDownload.accept(null, null);
          }
        }});
    // init an object mediating the interaction with JSaddle
    final Handler hnd = new Handler();
    Log.d("@APPNAME@", "new JSaddleShim");
    jsaddle = new JSaddleShim(wv, hnd, new Consumer<Integer>() {
      @Override
      public void accept(Integer exitCode) {
        Log.d("@APPNAME@", "finishing");
        finish();
      }
    });
    Log.d("@APPNAME@", "new JSaddleShim done");
    // initialize the application callbacks object
    appCallbacks = new AppCallbacksShim();
    // create and set a web view client aware of the JSaddle
    wv.setWebViewClient(new JSaddleWebViewClient(jsaddle));
    // create and set a web chrome client for console message handling
    wv.setWebChromeClient(new JSaddleWebChromeClient());
    // register jsaddle javascript interface
    wv.addJavascriptInterface(jsaddle, "jsaddle");
    // tell C about the shim so that it can spin up Haskell and connect the two
    Log.d("@APPNAME@", "new JSaddleShim.startMain");
    jsaddle.startMain();
    Log.d("@APPNAME@", "new JSaddleShim.startMain done");
    wv.loadUrl("file:///android_asset/index.html");
    if (intent.getExtras() != null && intent.getStringExtra("custom") != null) {
      appCallbacks.mainActivityOnNewIntent("custom-background", intent.getStringExtra("custom"));
    } else if (intent != null && intent.getData() != null && intent.getAction() != null) {
      appCallbacks.mainActivityOnNewIntent(intent.getAction(), intent.getDataString());
    }
    appCallbacks.mainActivityOnCreate();
  }

  private void requestPermissionsAndThen(String[] permissions, BiConsumer<String[], int[]> callback) {
    int code = nextPermissionRequestCode.getAndIncrement();
    outstandingPermissionsRequests.put(code, callback);
    requestPermissions(new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE}, code);
  }

  private AtomicInteger nextPermissionRequestCode = new AtomicInteger(1);
  private Map<Integer, BiConsumer<String[], int[]>> outstandingPermissionsRequests = new ConcurrentHashMap();

  @Override
  public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
    outstandingPermissionsRequests.remove(requestCode).accept(permissions, grantResults);
  }

  @Override
  public void onStart() {
    super.onStart();
    appCallbacks.mainActivityOnStart();
  }
  @Override
  public void onResume() {
    super.onResume();
    appCallbacks.mainActivityOnResume();
  }
  @Override
  public void onPause() {
    super.onPause();
    appCallbacks.mainActivityOnPause();
  }
  @Override
  public void onStop() {
    CookieManager.getInstance().flush();
    super.onStop();
    appCallbacks.mainActivityOnStop();
  }
  @Override
  public void onDestroy() {
    Log.d("@APPNAME@", "onDestroy");
    // jsaddle.deinit(); crashes because we're not deinit'ing native threads correctly
    super.onDestroy();
    appCallbacks.mainActivityOnDestroy();
    android.os.Process.killProcess(android.os.Process.myPid()); //TODO: Properly handle the process surviving between invocations which means that the Haskell RTS needs to not be initialized twice.
  }
  @Override
  public void onRestart() {
    super.onRestart();
    appCallbacks.mainActivityOnRestart();
  }
  @Override
  public void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    if (intent != null && intent.getData() != null && intent.getAction() != null) {
      appCallbacks.mainActivityOnNewIntent(intent.getAction(), intent.getDataString());
    }
  }

  // File uploads don't work out of the box.
  // You have to start an 'Intent' from 'onShowFileChooser', and handle the result here.
  @Override
  public void onActivityResult(final int requestCode, final int resultCode, final Intent intent) {
    if (requestCode == REQUEST_CODE_FILE_PICKER) {
      if (resultCode == Activity.RESULT_OK) {
        if (intent != null) {
          if (fileUploadCallback != null) {
            Uri[] dataUris = null;

            try {
              if (intent.getDataString() != null) {
                dataUris = new Uri[] { Uri.parse(intent.getDataString()) };
              }
              else {
                if (intent.getClipData() != null) {
                  final int numSelectedFiles = intent.getClipData().getItemCount();

                  dataUris = new Uri[numSelectedFiles];

                  for (int i = 0; i < numSelectedFiles; i++) {
                    dataUris[i] = intent.getClipData().getItemAt(i).getUri();
                  }
                }
              }
            }
            catch (Exception ignored) { }

            fileUploadCallback.onReceiveValue(dataUris);
            fileUploadCallback = null;
          }
        }
      }
      else if (fileUploadCallback != null) {
        fileUploadCallback.onReceiveValue(null);
        fileUploadCallback = null;
      }
    }
  }

  private class JSaddleWebChromeClient extends WebChromeClient {
    @Override
    public boolean onConsoleMessage(ConsoleMessage cm) {
      Log.d("@APPNAME@", String.format("%s @ %d: %s", cm.message(), cm.lineNumber(), cm.sourceId()));
      return true;
    }

    // file upload callback (Android 5.0 (API level 21) -- current)
    @Override
    public boolean onShowFileChooser(WebView webView, ValueCallback<Uri[]> filePathCallback, WebChromeClient.FileChooserParams fileChooserParams) {
      final boolean allowMultiple = fileChooserParams.getMode() == FileChooserParams.MODE_OPEN_MULTIPLE;

      if (fileUploadCallback != null) {
        fileUploadCallback.onReceiveValue(null);
      }
      fileUploadCallback = filePathCallback;

      Intent i = new Intent(Intent.ACTION_GET_CONTENT);
      i.addCategory(Intent.CATEGORY_OPENABLE);

      if (allowMultiple) {
        i.putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true);
      }

      i.setType("*/*");

      startActivityForResult(Intent.createChooser(i, "Choose a File"), REQUEST_CODE_FILE_PICKER);

      return true;
    }
  }
}
