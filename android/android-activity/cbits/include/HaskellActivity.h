typedef struct ActivityCallbacks {
  void (*onCreate) (); //TODO: Support savedInstanceState
  void (*onStart) ();
  void (*onResume) ();
  void (*onPause) ();
  void (*onStop) ();
  void (*onDestroy) ();
  void (*onRestart) ();
  void (*onNewIntent) (const char *, const char *); //TODO: Pass the whole argument and use JNI
} ActivityCallbacks;

extern void haskellActivity_continueWithCallbacks(const ActivityCallbacks *callbacks);
