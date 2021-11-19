package top.bogey.auto_touch.room.bean;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Base64;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import java.io.ByteArrayOutputStream;
import java.util.List;

public class Node {
    public NodeType type = NodeType.WORD;
    private String value;

    public Node() {}

    public Node(String value){
        this.value = value;
    }

    public String getWord(){
        return value;
    }

    public List<Pos> getPoses(){
        try {
            return new Gson().fromJson(value, new TypeToken<List<Pos>>(){}.getType());
        } catch (JsonSyntaxException ignored){}
        return null;
    }

    public Bitmap getImage(){
        Bitmap bitmap = null;
        try {
            byte[] bitmapArray;
            bitmapArray = Base64.decode(value, Base64.DEFAULT);
            bitmap = BitmapFactory.decodeByteArray(bitmapArray, 0, bitmapArray.length);
        } catch (IllegalArgumentException ignored){}
        return bitmap;
    }

    public SimpleTaskInfo getTask(){
        try {
            return new Gson().fromJson(value, SimpleTaskInfo.class);
        } catch (JsonSyntaxException ignored){}
        return null;
    }

    public void setWord(String value){
        this.value = value;
        type = NodeType.WORD;
    }

    public void setPoses(List<Pos> poses){
        value = new Gson().toJson(poses);
        type = NodeType.POS;
    }

    public void setImage(Bitmap bitmap){
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.PNG, 100, stream);
        byte[] bytes = stream.toByteArray();
        value = Base64.encodeToString(bytes, Base64.DEFAULT);
        type = NodeType.IMAGE;
    }

    public void setTask(SimpleTaskInfo info){
        value = new Gson().toJson(info);
        type = NodeType.TASK;
    }
}
