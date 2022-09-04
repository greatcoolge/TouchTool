package top.bogey.touch_tool.utils.easy_float;

import android.graphics.Point;
import android.view.View;

public class FloatConfig {
    int layoutId;
    View layoutView = null;
    String tag = null;

    boolean dragEnable = true;
    boolean isDrag = false;

    boolean isAnim = false;
    FloatAnimator animator = new FloatAnimator();

    boolean hasEditText = false;

    SidePattern side = SidePattern.DEFAULT;
    boolean matchWidth = false;
    boolean matchHeight = false;

    FloatGravity gravity = FloatGravity.CENTER;
    Point offset = new Point(0, 0);

    int leftBorder = 0;
    int rightBorder = 0;
    int topBorder = 0;
    int bottomBorder = 0;

    FloatCallback callback = null;

    boolean alwaysShow = false;
}
