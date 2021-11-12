#ifndef NATIVE_LIB
#define NATIVE_LIB

#include <jni.h>
#include <string>

#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "android/bitmap.h"

cv::Mat bitmap2Mat(JNIEnv *env, jobject bitmap){
    AndroidBitmapInfo bitmapInfo;
    AndroidBitmap_getInfo(env, bitmap, &bitmapInfo);
    int *pixels = nullptr;
    AndroidBitmap_lockPixels(env, bitmap, (void **) &pixels);
    cv::Mat rgba(cv::Size(bitmapInfo.width, bitmapInfo.height), CV_8UC4, pixels);
    return rgba;
}

jobject createMatchResult(JNIEnv *env, jdouble value, jint x, jint y){
    auto resultClass = (jclass) env->FindClass("top/bogey/auto_touch/util/MatchResult");
    jmethodID mid = env->GetMethodID(resultClass, "<init>", "(DII)V");
    jobject result = env->NewObject(resultClass, mid, value, x, y);
    return result;
}

extern "C"
JNIEXPORT jobject JNICALL
Java_top_bogey_auto_1touch_util_NativeUtil_matchTemplate(JNIEnv *env, jclass clazz, jobject bitmap, jobject temp, jint method) {
    cv::Mat src = bitmap2Mat(env, bitmap);
    cv::cvtColor(src, src, cv::COLOR_RGBA2GRAY);
    cv::Mat tmp = bitmap2Mat(env, temp);
    cv::cvtColor(tmp, tmp, cv::COLOR_RGBA2GRAY);
    cv::Mat result;

    cv::matchTemplate(src, tmp, result, method);

    double minVal = -1;
    double maxVal;
    cv::Point minLoc;
    cv::Point maxLoc;
    cv::minMaxLoc(result, &minVal, &maxVal, &minLoc, &maxLoc);

    jobject matchResult;
    if (method == cv::TM_SQDIFF || method == cv::TM_SQDIFF_NORMED){
        matchResult = createMatchResult(env, minVal, minLoc.x, minLoc.y);
    } else {
        matchResult = createMatchResult(env, maxVal, maxLoc.x, maxLoc.y);
    }
    src.release();
    tmp.release();
    result.release();
    return matchResult;
}

#endif