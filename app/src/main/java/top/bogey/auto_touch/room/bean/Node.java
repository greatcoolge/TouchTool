package top.bogey.auto_touch.room.bean;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Base64;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

public class Node{
    private NodeType type;
    private String value = "";

    private transient Bitmap bitmap;

    public Node(NodeType type) {
        this.type = type;
    }

    public Node(NodeType type, String value) {
        this.type = type;
        this.value = value;
    }

    public NodeType getType() {
        return type;
    }

    public void setNull(){
        type = NodeType.NULL;
        value = "";
    }

    public void setNumber(int number){
        type = NodeType.NUMBER;
        value = String.valueOf(number);
    }

    public int getNumber(){
        return Integer.parseInt(value);
    }

    public void setDelay(int delay){
        type = NodeType.DELAY;
        value = String.valueOf(delay);
    }

    public int getDelay(){
        return Integer.parseInt(value);
    }

    public void setText(String text){
        this.value = text;
        type = NodeType.TEXT;
    }

    public String getText(){
        return value;
    }

    public void setImage(Bitmap bitmap){
        this.bitmap = bitmap;
        type = NodeType.IMAGE;
        if (bitmap == null){
            value = "";
            return;
        }
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.JPEG, 100, stream);
        byte[] bytes = stream.toByteArray();
        value = Base64.encodeToString(bytes, Base64.DEFAULT);
    }

    public Bitmap getImage(){
        if (value.isEmpty()) return null;
        if (bitmap != null) return bitmap;
        try {
            byte[] bitmapArray;
            bitmapArray = Base64.decode(value, Base64.DEFAULT);
            bitmap = BitmapFactory.decodeByteArray(bitmapArray, 0, bitmapArray.length);
        } catch (IllegalArgumentException ignored){
            return null;
        }
        return bitmap;
    }

    public void setPoses(List<Pos> poses){
        if (poses == null){
            poses = new ArrayList<>();
        }
        type = NodeType.POS;
        value = new Gson().toJson(poses);
    }

    public void setPoses(String posesString){
        if (posesString == null || posesString.isEmpty()){
            setPoses((List<Pos>) null);
            return;
        }
        try {
            List<Pos> poses = new Gson().fromJson(posesString, new TypeToken<List<Pos>>() {}.getType());
            setPoses(poses);
        } catch (JsonSyntaxException ignored){
            setPoses((List<Pos>) null);
        }
    }

    public List<Pos> getPoses(){
        try {
            return new Gson().fromJson(value, new TypeToken<List<Pos>>(){}.getType());
        } catch (JsonSyntaxException ignored){}
        return null;
    }

    public void setKey(int key){
        type = NodeType.KEY;
        value = String.valueOf(key);
    }

    public int getKey(){
        return Integer.parseInt(value);
    }

    public void setTask(SimpleTaskInfo info){
        type = NodeType.TASK;
        if (info == null){
            value = "";
            return;
        }
        value = new Gson().toJson(info);
    }

    public SimpleTaskInfo getTask(){
        try {
            return new Gson().fromJson(value, SimpleTaskInfo.class);
        } catch (JsonSyntaxException ignored){}
        return null;
    }

    @Override
    @NonNull
    public Node clone(){
        return new Node(type, value);
    }
}
