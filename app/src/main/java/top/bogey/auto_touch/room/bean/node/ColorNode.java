package top.bogey.auto_touch.room.bean.node;

import android.graphics.Path;
import android.graphics.Rect;

import java.util.List;

import top.bogey.auto_touch.MainCaptureService;

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
            MainCaptureService.CaptureServiceBinder binder = (MainCaptureService.CaptureServiceBinder) obj;
            List<Rect> rectList = binder.matchColor(getValue().getColor());
            if (rectList != null && rectList.size() > 0){
                Path[] paths =new Path[rectList.size()];
                for (int i = 0; i < rectList.size(); i++) {
                    Path path = new Path();
                    path.moveTo(rectList.get(i).centerX(), rectList.get(i).centerY());
                    paths[i] = path;
                }
                return paths;
            }
        }
        return null;
    }

    public String getTitle(){
        int[] value = getValue().getColor();
        return "(" + value[0] + "," + value[1] + "," + value[2] + ")";
    }

    public static class ColorInfo{
        private int[] color = new int[]{-1, -1, -1};
        private int size = 81;

        public ColorInfo() {
        }

        public ColorInfo(int[] color, int size) {
            this.color = color;
            this.size = size;
        }

        public int[] getColor() {
            return color;
        }

        public void setColor(int[] color) {
            this.color = color;
        }

        public int getSize() {
            return size;
        }

        public void setSize(int size) {
            this.size = size;
        }
    }
}
