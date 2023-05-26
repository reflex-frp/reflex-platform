{ applicationId
, activityAttributes
, version
, iconPath
, intentFilters
, services
, permissions
, allowBackup ? false
, fullBackupContent ? allowBackup
, usesCleartextTraffic ? false
}:
let
  boolStr = x: if x then "true" else "false";
in ''
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
      package="${applicationId}"
      android:versionCode="${version.code}"
      android:versionName="${version.name}">
    <application android:label="@string/app_name"
                 android:icon="${iconPath}"
                 android:allowBackup="${boolStr allowBackup}"
                 android:fullBackupContent="${boolStr fullBackupContent}"
                 android:hardwareAccelerated="true"
                 android:usesCleartextTraffic="${boolStr usesCleartextTraffic}">
        <activity android:name="systems.obsidian.HaskellActivity"
                  android:label="@string/app_name"
                  android:configChanges="orientation|screenSize"
                  android:windowSoftInputMode="adjustResize"
                  ${activityAttributes}
                  >
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
            ${intentFilters}
        </activity>
        ${services}
    </application>
    <uses-permission android:name="android.permission.INTERNET" />
    ${permissions}
</manifest>
''
