package top.bogey.auto_touch.utils;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Point;
import android.graphics.Rect;
import android.view.Surface;
import android.view.View;
import android.view.WindowManager;

public class DisplayUtils {
    public static int getAttrColor(Context context, int id, int defValue){
        int[] attrs = {id};
        TypedArray typedArray = context.getTheme().obtainStyledAttributes(attrs);
        int resourceId = typedArray.getResourceId(0, defValue);
        typedArray.recycle();
        return context.getResources().getColor(resourceId, null);
    }

    public static boolean isPortrait(Context context){
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        return manager.getDefaultDisplay().getRotation() % 2 == Surface.ROTATION_0;
    }

    public static Point getScreenSize(Context context){
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        Point point = new Point();
        manager.getDefaultDisplay().getRealSize(point);
        if (isPortrait(context)){
            if (point.x > point.y) return new Point(point.y, point.x);
        } else {
            if (point.y > point.x) return new Point(point.y, point.x);
        }
        return point;
    }

    public static Rect getScreenArea(Context context){
        Point size = getScreenSize(context);
        return new Rect(0, 0, size.x, size.y);
    }

    public static int getStatusBarHeight(View view, WindowManager.LayoutParams params){
        if (params == null) return 0;
        int[] location = new int[2];
        view.getLocationOnScreen(location);
        // 绝对坐标和相对坐标一致，则状态栏高度为0，否则就是有状态栏
        if (location[1] > params.y) return getStatusBarHeight(view.getContext());
        return 0;
    }

    public static int getStatusBarHeight(Context context){
        int id = context.getResources().getIdentifier("status_bar_height", "dimen", "android");
        if (id > 0) return context.getResources().getDimensionPixelSize(id);
        return 0;
    }

    public static int dp2px(Context context, float dp){
        return Math.round(dp * context.getResources().getDisplayMetrics().density);
    }

    public static Point px2percent(Context context, Point point){
        Point size = getScreenSize(context);
        return new Point(Math.round(point.x * 100f / size.x), Math.round(point.y * 100f / size.y));
    }

    public static Point percent2px(Context context, Point point){
        Point size = getScreenSize(context);
        return new Point(Math.round(size.x * point.x / 100f), Math.round(size.y * point.y / 100f));
    }

    public static int[] getHsvColor(Bitmap bitmap, int x, int y){
        int pixel = bitmap.getPixel(x, y);
        int red = (pixel & 0x00ff0000) >> 16;
        int green = (pixel & 0x0000ff00) >> 8;
        int blue = pixel & 0x000000ff;
        float[] hsv = new float[3];
        Color.RGBToHSV(red, green, blue, hsv);
        return new int[]{(int) (hsv[0] / 2), (int) (hsv[1] * 255), (int) (hsv[2] * 255)};
    }
}
