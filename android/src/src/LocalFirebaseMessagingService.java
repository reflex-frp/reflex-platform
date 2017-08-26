package systems.obsidian.focus;

import android.util.Log;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

public class LocalFirebaseMessagingService extends FirebaseMessagingService {

  private native void handleNotification(String intent, String notificationdata);
  @Override
  public void onMessageReceived(RemoteMessage remoteMessage) {
    if (remoteMessage.getData().size() > 0) {
      String custom = remoteMessage.getData().get("custom");
      if (custom != null) {
        handleNotification("custom-foreground", custom);
      }
    }
  }
}
