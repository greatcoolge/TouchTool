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

-keepclassmembers class top.bogey.auto_touch.room.bean.Task{
    private java.lang.String id;
    private java.lang.String pkgName;
    private java.lang.String title;
    private int groupId;
    private java.util.List actions;
    private top.bogey.auto_touch.room.bean.TaskStatus taskStatus;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.Action{
    private top.bogey.auto_touch.room.bean.ActionMode actionMode;
    private boolean enable;
    private java.util.List targets;
    private top.bogey.auto_touch.room.bean.Node condition;
    private top.bogey.auto_touch.room.bean.Node stop;
    private int times;
    private int interval;
    private int time;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.Node{
    private top.bogey.auto_touch.room.bean.NodeType type;
    private java.lang.String value;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.Pos{
    private int x;
    private int y;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.SimpleTaskInfo{
    private java.lang.String id;
    private java.lang.String title;
}

-keep class top.bogey.auto_touch.util.MatchResult{*;}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile