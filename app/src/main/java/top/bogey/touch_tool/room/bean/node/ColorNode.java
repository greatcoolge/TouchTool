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
                        int size = rect.width() * rect.height();
                        if (size < colorInfo.getMinSize(service) || size > colorInfo.getMaxsize(service)){
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
        return new ColorInfo(new int[]{value.color[0], value.color[1], value.color[2]}, value.minPercent, value.maxPercent, value.size, value.screen);
    }

    public String getTitle(){
        ColorInfo colorInfo = getValue();
        return colorInfo.getMinPercent() + "-" + colorInfo.getMaxPercent();
    }

    public static class ColorInfo{
        private int[] color = new int[]{-1, -1, -1};
        private int minPercent = 0;
        private int maxPercent = 100;
        private int size = 0;
        private final int screen;

        public ColorInfo(Context context) {
            screen = DisplayUtils.getScreen(context);
        }

        public ColorInfo(int[] color, int minPercent, int maxPercent, int size, int screen) {
            this.color = color;
            this.minPercent = minPercent;
            this.maxPercent = maxPercent;
            this.size = size;
            this.screen = screen;
        }

        public int[] getColor() {
            return color;
        }

        public int getMinPercent() {
            return minPercent;
        }

        public int getMaxPercent() {
            return maxPercent;
        }

        public int getSize() {
            return size;
        }

        public int getSize(Context context){
            int screen = DisplayUtils.getScreen(context);
            return (int) (size * 1f * screen / this.screen);
        }

        public int getMinSize(Context context){
            return (int) (getSize(context) * minPercent / 100f);
        }

        public int getMaxsize(Context context){
            return (int) (getSize(context) * maxPercent / 100f);
        }
    }
}
