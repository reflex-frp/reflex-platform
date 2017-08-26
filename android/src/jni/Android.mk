LOCAL_PATH := $(call my-dir)



include $(CLEAR_VARS)

LOCAL_MODULE := libiconv
LOCAL_SRC_FILES := ../lib/$(TARGET_ARCH_ABI)/libiconv.so

include $(PREBUILT_SHARED_LIBRARY)



include $(CLEAR_VARS)

LOCAL_MODULE := libcharset
LOCAL_SRC_FILES := ../lib/$(TARGET_ARCH_ABI)/libcharset.so

include $(PREBUILT_SHARED_LIBRARY)



include $(CLEAR_VARS)

LOCAL_MODULE := app
LOCAL_SRC_FILES := ../lib/$(TARGET_ARCH_ABI)/@APP_LIB_NAME@
LOCAL_SHARED_LIBRARIES := "libiconv" "libcharset"
LOCAL_LDLIBS := -llog

include $(PREBUILT_SHARED_LIBRARY)
