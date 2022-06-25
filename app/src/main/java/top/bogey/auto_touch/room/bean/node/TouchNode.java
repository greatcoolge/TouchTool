package top.bogey.auto_touch.room.bean.node;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Path;
import android.graphics.Point;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.utils.DisplayUtils;

public class TouchNode extends Node {
    public TouchNode(String value) {
        super(NodeType.TOUCH, value);
    }

    public void setValue(Context context, List<Point> value){
        List<Point> points = new ArrayList<>();
        for (Point point : value) {
            points.add(DisplayUtils.px2percent(context, point));
        }
        this.value = new Gson().toJson(points);
    }

    @Override
    public String getValue() {
        return (String) value;
    }

    @Override
    public boolean isValid() {
        List<Point> points = getPoints();
        return points != null && !points.isEmpty();
    }

    @Override
    public boolean checkNode(Object obj) {
        return isValid();
    }

    @Override
    public Object getNodeTarget(Object obj) {
        Context context = (Context) obj;
        return getPath(context);
    }

    public List<Point> getPoints(){
        List<Point> points = new ArrayList<>();
        try{
            List<Point> pointList = new Gson().fromJson(getValue(), new TypeToken<List<Point>>() {}.getType());
            if (pointList != null) points = pointList;
        } catch (JsonSyntaxException ignored){}
        return points;
    }

    public List<Point> getPoints(Context context){
        List<Point> points = new ArrayList<>();
        for (Point point : getPoints()) {
            points.add(DisplayUtils.percent2px(context, point));
        }
        return points;
    }

    public Path getPath(Context context){
        List<Point> value = getPoints();
        if (value != null && !value.isEmpty()){
            Path path = new Path();
            Point firstPoint = DisplayUtils.percent2px(context, value.get(0));
            path.moveTo(firstPoint.x, firstPoint.y);
            for (int i = 1; i < value.size(); i++) {
                Point point = DisplayUtils.percent2px(context, value.get(i));
                path.lineTo(point.x, point.y);
            }
            return path;
        }
        return null;
    }

    @SuppressLint("DefaultLocale")
    public String getTitle(){
        List<Point> value = getPoints();
        if (value == null || value.size() == 0) return "";
        StringBuilder builder = new StringBuilder();
        for (Point point : value) {
            builder.append(String.format("(%d,%d)->", point.x, point.y));
        }
        return builder.substring(0, builder.length() - 2);
    }
}
