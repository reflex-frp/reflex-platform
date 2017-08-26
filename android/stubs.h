#ifndef _INCLUDE_FOCUS_H_
#define _INCLUDE_FOCUS_H_
#include "jsaddle.h"

void __stginit_Main (void);

native_callbacks* appMain (void (* evaluateJs) (const char*), app_callbacks* hsAppCallbacks);

int start_logger(const char *app_name);

void evaluateJavascriptWrapper (const char* js);

#endif
