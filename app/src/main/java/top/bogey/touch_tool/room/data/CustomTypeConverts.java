package top.bogey.touch_tool.room.data;

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

import top.bogey.touch_tool.room.bean.Action;
import top.bogey.touch_tool.room.bean.node.ColorNode;
import top.bogey.touch_tool.room.bean.node.DelayNode;
import top.bogey.touch_tool.room.bean.node.ImageNode;
import top.bogey.touch_tool.room.bean.node.KeyNode;
import top.bogey.touch_tool.room.bean.node.Node;
import top.bogey.touch_tool.room.bean.node.NodeType;
import top.bogey.touch_tool.room.bean.node.NullNode;
import top.bogey.touch_tool.room.bean.node.NumberNode;
import top.bogey.touch_tool.room.bean.node.TaskNode;
import top.bogey.touch_tool.room.bean.node.TextNode;
import top.bogey.touch_tool.room.bean.node.TimeArea;
import top.bogey.touch_tool.room.bean.node.TouchNode;

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

            if (nodeType == NodeType.NUMBER) {
                node = new NumberNode(jsonObject.get("value").getAsInt());


            } else if (nodeType == NodeType.DELAY) {
                JsonObject delayArea = jsonObject.get("value").getAsJsonObject();
                int min = 1000, max = 1000;
                JsonElement minElement = delayArea.get("min");
                if (minElement != null) min = minElement.getAsInt();
                JsonElement maxElement = delayArea.get("max");
                if (maxElement != null) max = maxElement.getAsInt();
                node = new DelayNode(new TimeArea(min, max));


            } else if (nodeType == NodeType.TEXT) {
                node = new TextNode(jsonObject.get("value").getAsString());


            } else if (nodeType == NodeType.IMAGE) {
                JsonObject imageInfo = jsonObject.get("value").getAsJsonObject();
                String image = null;
                int value = 95, screenWidth = 1080;
                JsonElement imageElement = imageInfo.get("image");
                if (imageElement != null) image = imageElement.getAsString();
                JsonElement valueElement = imageInfo.get("value");
                if (valueElement != null) value = valueElement.getAsInt();
                JsonElement screenWidthElement = imageInfo.get("screenWidth");
                if (screenWidthElement != null) screenWidth = screenWidthElement.getAsInt();
                node = new ImageNode(new ImageNode.ImageInfo(image, value, screenWidth));


            } else if (nodeType == NodeType.TOUCH) {
                node = new TouchNode(jsonObject.get("value").getAsString());


            } else if (nodeType == NodeType.COLOR) {
                JsonObject colorInfo = jsonObject.get("value").getAsJsonObject();
                int[] color = {0, 0, 0};
                int minSize = 0, maxSize = 81;
                JsonArray colorArray = colorInfo.getAsJsonArray("color");
                if (colorArray != null)
                    color = new int[]{colorArray.get(0).getAsInt(), colorArray.get(1).getAsInt(), colorArray.get(2).getAsInt()};
                JsonElement minSizeElement = colorInfo.get("minSize");
                if (minSizeElement != null) minSize = minSizeElement.getAsInt();
                JsonElement maxSizeElement = colorInfo.get("maxSize");
                if (maxSizeElement != null) maxSize = maxSizeElement.getAsInt();
                node = new ColorNode(new ColorNode.ColorInfo(color, minSize, maxSize));


            } else if (nodeType == NodeType.KEY) {
                node = new KeyNode(jsonObject.get("value").getAsInt());


            } else if (nodeType == NodeType.TASK) {
                JsonObject taskInfo = jsonObject.get("value").getAsJsonObject();
                String id = null, title = null;
                JsonElement idElement = taskInfo.get("id");
                if (idElement != null) id = idElement.getAsString();
                JsonElement titleElement = taskInfo.get("title");
                if (titleElement != null) title = titleElement.getAsString();
                node = new TaskNode(new TaskNode.TaskInfo(id, title));
            }

            JsonObject timeArea = jsonObject.get("timeArea").getAsJsonObject();
            int min = 1000, max = 1000;
            JsonElement minElement = timeArea.get("min");
            if (minElement != null) min = minElement.getAsInt();
            JsonElement maxElement = timeArea.get("max");
            if (maxElement != null) max = maxElement.getAsInt();
            node.getTimeArea().setMin(min);
            node.getTimeArea().setMax(max);


            return node;
        }
    }
}
