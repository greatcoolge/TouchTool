# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

-assumenosideeffects class android.util.Log{
    public static int d(...);
}

-keepclassmembers class top.bogey.touch_tool.room.bean.Task{
    private java.lang.String id;
    private java.lang.String pkgName;
    private java.lang.String title;
    private java.util.List actions;
    private top.bogey.touch_tool.room.bean.TaskStatus status;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.Action{
    private top.bogey.touch_tool.room.bean.ActionMode actionMode;
    private boolean enable;
    private java.util.List targets;
    private top.bogey.touch_tool.room.bean.node.Node condition;
    private int times;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.node.Node{
    protected top.bogey.touch_tool.room.bean.node.NodeType type;
    protected java.lang.Object value;
    protected top.bogey.touch_tool.room.bean.node.TimeArea timeArea;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.node.TaskNode$TaskInfo{
    private java.lang.String id;
    private java.lang.String title;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.node.ImageNode$ImageInfo{
    private java.lang.String image;
    private int value;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.node.ColorNode$ColorInfo{
    private int[] color;
    private int minSize;
}

-keepclassmembers class android.graphics.Point{
    public int x;
    public int y;
}

-keepclassmembers class top.bogey.touch_tool.room.bean.node.TimeArea{
    private int min;
    private int max;
}

-keep class top.bogey.touch_tool.utils.MatchResult{*;}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile