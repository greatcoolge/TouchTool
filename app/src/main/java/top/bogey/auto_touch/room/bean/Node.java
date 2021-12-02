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
import java.util.Objects;

public class Node implements Cloneable {
    public NodeType type = NodeType.WORD;
    private String value = "";

    private transient Bitmap bitmap;

    public Node() {}

    public Node(String value){
        this.value = value;
    }

    public Node(NodeType type) {
        this.type = type;
    }

    public Node(NodeType type, String value) {
        this.type = type;
        this.value = value;
    }

    public boolean getBool(){
        return Boolean.parseBoolean(value);
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

    public SimpleTaskInfo getTask(){
        try {
            return new Gson().fromJson(value, SimpleTaskInfo.class);
        } catch (JsonSyntaxException ignored){}
        return null;
    }

    public void setNull(){
        value = "";
        type = NodeType.NULL;
    }

    public void setBool(boolean bool){
        value = String.valueOf(bool);
        type = NodeType.BOOL;
    }

    public void setWord(@NonNull String value){
        this.value = value;
        type = NodeType.WORD;
    }

    public void setPoses(List<Pos> poses){
        if (poses == null){
            poses = new ArrayList<>();
        }
        value = new Gson().toJson(poses);
        type = NodeType.POS;
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

    public void setImage(Bitmap bitmap){
        this.bitmap = bitmap;
        type = NodeType.IMAGE;
        if (bitmap == null){
            value = "";
            return;
        }
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        bitmap.compress(Bitmap.CompressFormat.PNG, 100, stream);
        byte[] bytes = stream.toByteArray();
        value = Base64.encodeToString(bytes, Base64.DEFAULT);
    }

    public void setTask(SimpleTaskInfo info){
        type = NodeType.TASK;
        if (info == null){
            value = "";
            return;
        }
        value = new Gson().toJson(info);
    }

    @NonNull
    @Override
    public Node clone() {
        try {
            Node node = (Node) super.clone();
            node.type = NodeType.values()[type.ordinal()];
            return node;
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return new Node();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Node node = (Node) o;
        return type == node.type && value.equals(node.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, value);
    }
}
