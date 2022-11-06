package top.bogey.touch_tool.utils;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Point;
import android.graphics.Rect;
import android.view.Display;
import android.view.Surface;
import android.view.View;
import android.view.WindowManager;

import java.util.List;

public class DisplayUtils {
    public static boolean isDefaultPortrait = true;

    public static void initParams(Context context) {
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        Display display = manager.getDefaultDisplay();
        Point point = new Point();
        display.getRealSize(point);
        if (display.getRotation() % 2 == Surface.ROTATION_0) {
            isDefaultPortrait = point.x < point.y;
        } else {
            isDefaultPortrait = point.x > point.y;
        }
    }

    public static int getAttrColor(Context context, int id, int defValue) {
        int[] attrs = {id};
        TypedArray typedArray = context.getTheme().obtainStyledAttributes(attrs);
        int resourceId = typedArray.getResourceId(0, defValue);
        typedArray.recycle();
        return context.getResources().getColor(resourceId, null);
    }

    public static boolean isPortrait(Context context) {
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        return manager.getDefaultDisplay().getRotation() % 2 == Surface.ROTATION_0;
    }

    public static Point getScreenSize(Context context) {
        WindowManager manager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        Point point = new Point();
        manager.getDefaultDisplay().getRealSize(point);
        // 为了解决有些手机横竖屏切换时，后台应用获得的屏幕大小不变这个问题。
        if (isDefaultPortrait) {
            // 适用于竖屏手机
            if (isPortrait(context)) {
                if (point.x > point.y) return new Point(point.y, point.x);
            } else {
                if (point.y > point.x) return new Point(point.y, point.x);
            }
        }
        return point;
    }

    public static Rect getScreenArea(Context context) {
        Point size = getScreenSize(context);
        return new Rect(0, 0, size.x, size.y);
    }

    public static int getScreen(Context context) {
        Point size = getScreenSize(context);
        return Math.min(size.x, size.y);
    }

    public static int getStatusBarHeight(View view, WindowManager.LayoutParams params) {
        if (params == null) return 0;
        int[] location = new int[2];
        view.getLocationOnScreen(location);
        // 绝对坐标和相对坐标一致，则状态栏高度为0，否则就是有状态栏
        if (isPortrait(view.getContext())) {
            if (location[1] > params.y) return getStatusBarHeight(view.getContext());
        } else {
            if (location[0] > params.x) return getStatusBarHeight(view.getContext());
        }
        return 0;
    }

    public static int getStatusBarHeight(Context context) {
        int id = context.getResources().getIdentifier("status_bar_height", "dimen", "android");
        if (id > 0) return context.getResources().getDimensionPixelSize(id);
        return 0;
    }

    public static int dp2px(Context context, float dp) {
        return Math.round(dp * context.getResources().getDisplayMetrics().density);
    }

    public static int[] getHsvColor(Bitmap bitmap, int x, int y) {
        int pixel = bitmap.getPixel(x, y);
        int red = (pixel & 0x00ff0000) >> 16;
        int green = (pixel & 0x0000ff00) >> 8;
        int blue = pixel & 0x000000ff;
        float[] hsv = new float[3];
        Color.RGBToHSV(red, green, blue, hsv);
        return new int[]{(int) (hsv[0] / 2), (int) (hsv[1] * 255), (int) (hsv[2] * 255)};
    }

    public static int getColorFromHsv(int[] hsv) {
        float[] tmp = new float[]{hsv[0] * 2, hsv[1] / 255f, hsv[2] / 255f};
        return Color.HSVToColor(tmp);
    }

    public static Rect calculatePointArea(List<Point> points) {
        boolean isInit = true;
        Rect area = new Rect();
        for (Point point : points) {
            if (isInit) {
                area.set(point.x, point.y, point.x, point.y);
                isInit = false;
            } else {
                if (point.x < area.left) area.left = point.x;
                if (point.x > area.right) area.right = point.x;
                if (point.y < area.top) area.top = point.y;
                if (point.y > area.bottom) area.bottom = point.y;
            }
        }
        return area;
    }
}
