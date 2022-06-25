package top.bogey.auto_touch.room.data;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.node.ColorNode;
import top.bogey.auto_touch.room.bean.node.DelayNode;
import top.bogey.auto_touch.room.bean.node.ImageNode;
import top.bogey.auto_touch.room.bean.node.KeyNode;
import top.bogey.auto_touch.room.bean.node.Node;
import top.bogey.auto_touch.room.bean.node.NodeType;
import top.bogey.auto_touch.room.bean.node.NullNode;
import top.bogey.auto_touch.room.bean.node.NumberNode;
import top.bogey.auto_touch.room.bean.node.TouchNode;
import top.bogey.auto_touch.room.bean.node.TaskNode;
import top.bogey.auto_touch.room.bean.node.TextNode;
import top.bogey.auto_touch.room.bean.node.TimeArea;

public class CustomTypeConverts {
    @TypeConverter
    public static List<Action> actionsFromString(String json){
        Gson gson = new GsonBuilder().registerTypeAdapter(Node.class, new NodeAdapter()).create();
        try{
            return gson.fromJson(json, new TypeToken<List<Action>>(){}.getType());
        }catch (JsonSyntaxException ignored){}
        return new ArrayList<>();
    }

    @TypeConverter
    public static String stringFromAction(List<Action> actions){
        Gson gson = new GsonBuilder().registerTypeAdapter(Node.class, new NodeAdapter()).create();
        return gson.toJson(actions);
    }

    public static class NodeAdapter implements JsonDeserializer<Node>{
        @Override
        public Node deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            JsonObject jsonObject = json.getAsJsonObject();
            String type = jsonObject.get("type").getAsString();
            NodeType nodeType = NodeType.valueOf(type);
            Node node = new NullNode();
            switch (nodeType) {
                case NUMBER:
                    node = new NumberNode(jsonObject.get("value").getAsInt());
                    break;
                case DELAY:
                    JsonObject delayArea = jsonObject.get("value").getAsJsonObject();
                    node = new DelayNode(new TimeArea(delayArea.get("min").getAsInt(), delayArea.get("max").getAsInt()));
                    break;
                case TEXT:
                    node = new TextNode(jsonObject.get("value").getAsString());
                    break;
                case IMAGE:
                    JsonObject imageInfo = jsonObject.get("value").getAsJsonObject();
                    node = new ImageNode(new ImageNode.ImageInfo(imageInfo.get("image").getAsString(), imageInfo.get("value").getAsInt()));
                    break;
                case TOUCH:
                    node = new TouchNode(jsonObject.get("value").getAsString());
                    break;
                case COLOR:
                    JsonObject colorInfo = jsonObject.get("value").getAsJsonObject();
                    JsonArray colorArray = colorInfo.getAsJsonArray("color");
                    int[] color = new int[]{colorArray.get(0).getAsInt(), colorArray.get(1).getAsInt(), colorArray.get(2).getAsInt()};
                    int size = colorInfo.get("size").getAsInt();
                    node = new ColorNode(new ColorNode.ColorInfo(color, size));
                    break;
                case KEY:
                    node = new KeyNode(jsonObject.get("value").getAsInt());
                    break;
                case TASK:
                    JsonObject taskInfo = jsonObject.get("value").getAsJsonObject();
                    node = new TaskNode(new TaskNode.TaskInfo(taskInfo.get("id").getAsString(), taskInfo.get("title").getAsString()));
                    break;
            }
            JsonObject timeArea = jsonObject.get("timeArea").getAsJsonObject();
            node.getTimeArea().setMin(timeArea.get("min").getAsInt());
            node.getTimeArea().setMax(timeArea.get("max").getAsInt());
            return node;
        }
    }
}
