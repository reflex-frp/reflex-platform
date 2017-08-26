package systems.obsidian.focus;

import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.ConsoleMessage;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;
import org.reflexfrp.JSaddleShim;

public class JSaddleWebViewClient extends WebViewClient {
  private JSaddleShim jsaddle;

  public JSaddleWebViewClient(JSaddleShim _jsaddle) {
    super();
    jsaddle = _jsaddle;
  }

  @Override
  public boolean shouldOverrideUrlLoading(WebView view, String url) {
    if (Uri.parse(url).getHost().endsWith("obsidian.systems")) { //TODO: Doesn't seem right
      return false;
    }

    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
    view.getContext().startActivity(intent);
    return true;
  }

  public void onPageFinished(WebView view, String url) {
    // jsaddle.injectJavascript(); //TODO
  }
}
