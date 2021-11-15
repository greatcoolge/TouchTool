package top.bogey.auto_touch.room.data;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.room.bean.Action;


public class MyTypeConverters {
    @TypeConverter
    public static List<Action> fromString(String json){
        try{
            return new Gson().fromJson(json, new TypeToken<List<Action>>(){}.getType());
        }catch (JsonSyntaxException ignored){}
        return new ArrayList<>();
    }

    @TypeConverter
    public static String fromAction(List<Action> actions){
        return new Gson().toJson(actions);
    }
}
