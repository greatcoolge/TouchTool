package top.bogey.touch_tool.room.bean.node;

import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class ColorNode extends Node{
    public ColorNode(ColorInfo colorInfo) {
        super(NodeType.COLOR, colorInfo);
    }

    @Override
    public ColorInfo getValue() {
        return (ColorInfo) value;
    }

    @Override
    public boolean isValid() {
        for (int i : getValue().getColor()) {
            if (i == -1) return false;
        }
        return true;
    }

    @Override
    public boolean checkNode(Object obj) {
        return isValid();
    }

    @Override
    public Object getNodeTarget(Object obj) {
        if (obj != null){
            MainAccessibilityService service = (MainAccessibilityService) obj;
            ColorInfo colorInfo = getValue();
            if (service.isCaptureEnabled() && service.binder != null){
                List<Rect> rectList = service.binder.matchColor(colorInfo.getColor());
                if (rectList != null && rectList.size() > 0){
                    for (int i = rectList.size() - 1; i >= 0; i--) {
                        Rect rect = rectList.get(i);
                        if (rect.width() * rect.height() < colorInfo.getMinSize()){
                            rectList.remove(i);
                        }
                    }
                    Path path = new Path();
                    for (int i = 0; i < rectList.size(); i++) {
                        Point fixedPosition = AppUtils.getFixedPosition(service, rectList.get(i).centerX(), rectList.get(i).centerY());
                        path.moveTo(fixedPosition.x, fixedPosition.y);
                    }
                    return path;
                }
            }
        }
        return null;
    }

    @Override
    public ColorInfo cloneValue() {
        ColorInfo value = getValue();
        return new ColorInfo(new int[]{value.color[0], value.color[1], value.color[2]}, value.minSize, value.maxSize, value.screen);
    }

    public String getTitle(){
        ColorInfo colorInfo = getValue();
        return colorInfo.getMinSize() + "-" + colorInfo.getMaxSize();
    }

    public static class ColorInfo{
        private int[] color = new int[]{-1, -1, -1};
        private int minSize = 0;
        private int maxSize = 81;
        private final int screen;

        public ColorInfo(Context context) {
            screen = DisplayUtils.getScreen(context);
        }

        public ColorInfo(int[] color, int minSize, int maxSize, int screen) {
            this.color = color;
            this.minSize = minSize;
            this.maxSize = maxSize;
            this.screen = screen;
        }

        public int[] getColor() {
            return color;
        }

        public void setColor(int[] color) {
            this.color = color;
        }

        public int getMinSize() {
            return minSize;
        }

        public int getMaxSize() {
            return maxSize;
        }
    }
}
