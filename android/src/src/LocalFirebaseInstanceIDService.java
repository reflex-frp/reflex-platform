package systems.obsidian.focus;

import android.util.Log;
import com.google.firebase.iid.FirebaseInstanceId;
import com.google.firebase.iid.FirebaseInstanceIdService;

public class LocalFirebaseInstanceIDService extends FirebaseInstanceIdService {

  private static final String TAG = "LocalFirebaseIIDService";

  private native void handleDeviceToken(String token);

  @Override
  public void onTokenRefresh() {
      // Get updated InstanceID token.
      String refreshedToken = FirebaseInstanceId.getInstance().getToken();
      Log.d(TAG, "Refreshed token: " + refreshedToken);

      // If you want to send messages to this application instance or
      // manage this apps subscriptions on the server side, send the
      // Instance ID token to your app server.
      sendRegistrationToServer(refreshedToken);
  }

  private void sendRegistrationToServer(String token) {
    handleDeviceToken(token);
  }

}
