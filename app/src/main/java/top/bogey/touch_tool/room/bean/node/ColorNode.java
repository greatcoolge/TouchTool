package top.bogey.touch_tool.room.bean.node;

import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;

import java.util.List;

import top.bogey.touch_tool.MainAccessibilityService;

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
                    Path[] paths =new Path[rectList.size()];
                    for (int i = 0; i < rectList.size(); i++) {
                        Path path = new Path();
                        Point fixedPosition = service.getFixedPosition(rectList.get(i).centerX(), rectList.get(i).centerY());
                        path.moveTo(fixedPosition.x, fixedPosition.y);
                        paths[i] = path;
                    }
                    return paths;
                }
            }
        }
        return null;
    }

    @Override
    public ColorInfo cloneValue() {
        ColorInfo value = getValue();
        return new ColorInfo(new int[]{value.color[0], value.color[1], value.color[2]}, value.minSize, value.maxSize);
    }

    public String getTitle(){
        ColorInfo colorInfo = getValue();
        return colorInfo.getMinSize() + "-" + colorInfo.getMaxSize();
    }

    public static class ColorInfo{
        private int[] color = new int[]{-1, -1, -1};
        private int minSize = 0;
        private int maxSize = 81;

        public ColorInfo() {
        }

        public ColorInfo(int[] color, int minSize, int maxSize) {
            this.color = color;
            this.minSize = minSize;
            this.maxSize = maxSize;
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

        public void setMinSize(int minSize) {
            this.minSize = minSize;
        }

        public int getMaxSize() {
            return maxSize;
        }

        public void setMaxSize(int maxSize) {
            this.maxSize = maxSize;
        }
    }
}
