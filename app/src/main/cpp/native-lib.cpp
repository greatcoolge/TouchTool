#ifndef NATIVE_LIB
#define NATIVE_LIB

#include <jni.h>
#include <string>

#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"

#include "android/bitmap.h"

using namespace std;
using namespace cv;

Mat bitmap2Mat(JNIEnv *env, jobject bitmap){
    AndroidBitmapInfo bitmapInfo;
    AndroidBitmap_getInfo(env, bitmap, &bitmapInfo);
    int *pixels = nullptr;
    AndroidBitmap_lockPixels(env, bitmap, (void **) &pixels);
    Mat rgba(Size(bitmapInfo.width, bitmapInfo.height), CV_8UC4, pixels);
    AndroidBitmap_unlockPixels(env, bitmap);
    return rgba;
}

jobject createMatchResult(JNIEnv *env, jdouble value, jint x, jint y, jint width, jint height){
    auto resultClass = (jclass) env->FindClass("top/bogey/touch_tool/utils/MatchResult");
    jmethodID mid = env->GetMethodID(resultClass, "<init>", "(DIIII)V");
    jobject result = env->NewObject(resultClass, mid, value, x, y, width, height);
    return result;
}

int clamp(int up, int low, int value){
    return max(low, min(up, value));
}

extern "C"
JNIEXPORT jobject JNICALL
Java_top_bogey_touch_1tool_utils_AppUtils_nativeMatchTemplate(JNIEnv *env, jclass clazz, jobject bitmap, jobject temp, jint method) {
    int scale = 2;

    Mat src = bitmap2Mat(env, bitmap);
    Mat tmp = bitmap2Mat(env, temp);
    if (src.empty() || tmp.empty()) return createMatchResult(env, 0, 0, 0, 0, 0);

    cvtColor(src, src, COLOR_RGBA2GRAY);
    resize(src, src, Size(src.cols / scale, src.rows / scale));
    cvtColor(tmp, tmp, COLOR_RGBA2GRAY);
    resize(tmp, tmp, Size(tmp.cols / scale, tmp.rows / scale));

    int resultCol = src.cols - tmp.cols + 1;
    int resultRow = src.rows - tmp.rows + 1;

    Mat result;
    result.create(resultCol, resultRow, CV_32FC1);
    matchTemplate(src, tmp, result, method);

    double minVal = -1;
    double maxVal;
    Point minLoc;
    Point maxLoc;
    minMaxLoc(result, &minVal, &maxVal, &minLoc, &maxLoc);

    jobject matchResult;
    if (method == TM_SQDIFF || method == TM_SQDIFF_NORMED){
        matchResult = createMatchResult(env, minVal, minLoc.x * scale, minLoc.y * scale, tmp.cols * scale, tmp.rows * scale);
    } else {
        matchResult = createMatchResult(env, maxVal, maxLoc.x * scale, maxLoc.y * scale, tmp.cols * scale, tmp.rows * scale);
    }
    src.release();
    tmp.release();
    result.release();
    return matchResult;
}

extern "C"
JNIEXPORT jobject JNICALL
Java_top_bogey_touch_1tool_utils_AppUtils_nativeMatchColor(JNIEnv *env, jclass clazz, jobject bitmap, jintArray hsvColor) {
    Mat src = bitmap2Mat(env, bitmap);
    if (src.empty()) return nullptr;
    cvtColor(src, src, COLOR_RGBA2BGR);
    cvtColor(src, src, COLOR_BGR2HSV);
    GaussianBlur(src, src, Size(5, 5), 0);
    erode(src, src, 3);

    jint *hsv = env->GetIntArrayElements(hsvColor, JNI_FALSE);
    Scalar color((int) hsv[0], (int) hsv[1], (int) hsv[2]);
    Scalar lowColor(clamp(180, 0, (int)color[0] - 5), clamp(255, 0, (int)color[1] - 5), clamp(255, 0, (int)color[2] - 5));
    Scalar highColor(clamp(180, 0, (int)color[0] + 5), clamp(255, 0, (int)color[1] + 5), clamp(255, 0, (int)color[2] + 5));

    Mat colorImg;
    inRange(src, lowColor, highColor, colorImg);
    vector<vector<Point> > contours;
    findContours(colorImg, contours, RETR_EXTERNAL, CHAIN_APPROX_SIMPLE);

    jclass listCls = env->FindClass("java/util/ArrayList");
    jmethodID listInit = env->GetMethodID(listCls, "<init>", "()V");
    jobject listObj = env->NewObject(listCls, listInit);
    jmethodID listAdd = env->GetMethodID(listCls, "add", "(Ljava/lang/Object;)Z");

    for (auto & contour : contours) {
        double area = contourArea(contour);
        if(area > 81){
            Rect r = boundingRect(contour);
            env->CallBooleanMethod(listObj, listAdd, createMatchResult(env, area, r.x, r.y, r.width, r.height));
        }
    }

    src.release();
    return listObj;
}

#endif