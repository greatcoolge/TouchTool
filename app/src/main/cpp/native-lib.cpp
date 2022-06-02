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
    auto resultClass = (jclass) env->FindClass("top/bogey/auto_touch/utils/MatchResult");
    jmethodID mid = env->GetMethodID(resultClass, "<init>", "(DII)V");
    jobject result = env->NewObject(resultClass, mid, value, x * 2, y * 2);
    return result;
}

extern "C"
JNIEXPORT jobject JNICALL
Java_top_bogey_auto_1touch_utils_AppUtils_nativeMatchTemplate(JNIEnv *env, jclass clazz, jobject bitmap, jobject temp, jint method) {
    cv::Mat src = bitmap2Mat(env, bitmap);
    cv::cvtColor(src, src, cv::COLOR_RGBA2GRAY);
    cv::resize(src, src, cv::Size(src.cols / 2, src.rows / 2));
    cv::Mat tmp = bitmap2Mat(env, temp);
    cv::cvtColor(tmp, tmp, cv::COLOR_RGBA2GRAY);
    cv::resize(tmp, tmp, cv::Size(tmp.cols / 2, tmp.rows / 2));

    int resultCol = src.cols - tmp.cols + 1;
    int resultRow = src.rows - tmp.rows + 1;

    cv::Mat result;
    result.create(resultCol, resultRow, CV_32FC1);
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
    AndroidBitmap_unlockPixels(env, bitmap);
    AndroidBitmap_unlockPixels(env, temp);
    return matchResult;
}

#endif