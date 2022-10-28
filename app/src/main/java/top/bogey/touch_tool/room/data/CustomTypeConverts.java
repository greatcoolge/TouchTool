package top.bogey.touch_tool.room.data;

import android.graphics.Point;

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
import top.bogey.touch_tool.utils.easy_float.FloatGravity;

public class CustomTypeConverts {
    @TypeConverter
    public static List<Action> actionsFromString(String json) {
        Gson gson = new GsonBuilder().registerTypeAdapter(Node.class, new NodeAdapter()).create();
        try {
            return gson.fromJson(json, new TypeToken<List<Action>>() {
            }.getType());
        } catch (JsonSyntaxException ignored) {
        }
        return new ArrayList<>();
    }

    @TypeConverter
    public static String stringFromAction(List<Action> actions) {
        Gson gson = new GsonBuilder().registerTypeAdapter(Node.class, new NodeAdapter()).create();
        return gson.toJson(actions);
    }

    public static class NodeAdapter implements JsonDeserializer<Node> {
        @Override
        public Node deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
            JsonObject jsonObject = json.getAsJsonObject();
            String type = jsonObject.get("type").getAsString();
            NodeType nodeType = NodeType.valueOf(type);
            Node node = new NullNode();

            if (nodeType == NodeType.NUMBER) {
                node = new NumberNode(jsonObject.get("value").getAsInt());


            } else if (nodeType == NodeType.DELAY) {
                int min = 1000, max = 1000;

                JsonObject delayArea = jsonObject.get("value").getAsJsonObject();
                if (delayArea != null) {
                    JsonElement minElement = delayArea.get("min");
                    if (minElement != null) min = minElement.getAsInt();
                    JsonElement maxElement = delayArea.get("max");
                    if (maxElement != null) max = maxElement.getAsInt();
                }
                node = new DelayNode(new TimeArea(min, max));


            } else if (nodeType == NodeType.TEXT) {
                node = new TextNode(jsonObject.get("value").getAsString());


            } else if (nodeType == NodeType.IMAGE) {
                String image = null;
                int value = 95, screen = 1080;

                JsonObject imageInfo = jsonObject.get("value").getAsJsonObject();
                if (imageInfo != null) {
                    JsonElement imageElement = imageInfo.get("image");
                    if (imageElement != null) image = imageElement.getAsString();
                    JsonElement valueElement = imageInfo.get("value");
                    if (valueElement != null) value = valueElement.getAsInt();
                    JsonElement screenWidthElement = imageInfo.get("screen");
                    if (screenWidthElement != null) screen = screenWidthElement.getAsInt();
                }
                node = new ImageNode(new ImageNode.ImageInfo(image, value, screen));


            } else if (nodeType == NodeType.TOUCH) {
                List<Point> points = new ArrayList<>();
                FloatGravity gravity = FloatGravity.TOP_LEFT;
                int screen = 1080;
                Point offset = new Point(0, 0);
                boolean touchOffset = true;

                JsonObject touchPath = jsonObject.get("value").getAsJsonObject();
                if (touchPath != null) {
                    JsonArray pathArray = touchPath.getAsJsonArray("path");
                    if (pathArray != null) {
                        for (JsonElement element : pathArray) {
                            JsonObject point = element.getAsJsonObject();
                            if (point != null)
                                points.add(new Point(point.get("x").getAsInt(), point.get("y").getAsInt()));
                        }
                    }
                    JsonElement gravityElement = touchPath.get("gravity");
                    if (gravityElement != null)
                        gravity = FloatGravity.valueOf(gravityElement.getAsString());
                    JsonElement offsetElement = touchPath.get("offset");
                    if (offsetElement != null)
                        offset.set(offsetElement.getAsJsonObject().get("x").getAsInt(), offsetElement.getAsJsonObject().get("y").getAsInt());
                    JsonElement screenWidthElement = touchPath.get("screen");
                    if (screenWidthElement != null) screen = screenWidthElement.getAsInt();
                }
                node = new TouchNode(new TouchNode.TouchPath(points, gravity, offset, screen, touchOffset));

            } else if (nodeType == NodeType.COLOR) {
                int[] color = {0, 0, 0};
                int minPercent = 0, maxPercent = 100, size = 0, screen = 1080;

                JsonObject colorInfo = jsonObject.get("value").getAsJsonObject();
                if (colorInfo != null) {
                    JsonArray colorArray = colorInfo.getAsJsonArray("color");
                    if (colorArray != null)
                        color = new int[]{colorArray.get(0).getAsInt(), colorArray.get(1).getAsInt(), colorArray.get(2).getAsInt()};
                    JsonElement minSizeElement = colorInfo.get("minSize");
                    if (minSizeElement != null) minPercent = minSizeElement.getAsInt();
                    JsonElement maxSizeElement = colorInfo.get("maxSize");
                    if (maxSizeElement != null) maxPercent = maxSizeElement.getAsInt();
                    JsonElement sizeWidthElement = colorInfo.get("size");
                    if (sizeWidthElement != null) size = sizeWidthElement.getAsInt();
                    JsonElement screenWidthElement = colorInfo.get("screen");
                    if (screenWidthElement != null) screen = screenWidthElement.getAsInt();
                }
                node = new ColorNode(new ColorNode.ColorInfo(color, minPercent, maxPercent, size, screen));


            } else if (nodeType == NodeType.KEY) {
                KeyNode.KeyType keyType = KeyNode.KeyType.BACK;
                String extras = "";

                JsonObject keyTask = jsonObject.get("value").getAsJsonObject();
                if (keyTask != null) {
                    JsonElement keyTypeElement = keyTask.get("keyType");
                    if (keyTypeElement != null) keyType = KeyNode.KeyType.valueOf(keyTypeElement.getAsString());
                    JsonElement extrasElement = keyTask.get("extras");
                    if (extrasElement != null) extras = extrasElement.getAsString();

                    node = new KeyNode(new KeyNode.KeyTask(keyType, extras));
                }

            } else if (nodeType == NodeType.TASK) {
                String id = null, title = null;
                JsonObject taskInfo = jsonObject.get("value").getAsJsonObject();
                if (taskInfo != null) {
                    JsonElement idElement = taskInfo.get("id");
                    if (idElement != null) id = idElement.getAsString();
                    JsonElement titleElement = taskInfo.get("title");
                    if (titleElement != null) title = titleElement.getAsString();
                }
                node = new TaskNode(new TaskNode.TaskInfo(id, title));
            }

            int min = 1000, max = 1000;
            JsonObject timeArea = jsonObject.get("timeArea").getAsJsonObject();
            if (timeArea != null) {
                JsonElement minElement = timeArea.get("min");
                if (minElement != null) min = minElement.getAsInt();
                JsonElement maxElement = timeArea.get("max");
                if (maxElement != null) max = maxElement.getAsInt();
            }
            node.getTimeArea().setTime(min, max);

            return node;
        }
    }
}
