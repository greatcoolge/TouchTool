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
    public int id;
    public java.lang.String pkgName;
    public java.lang.String title;
    public int groupId;
    public java.util.List actions;
    public top.bogey.auto_touch.room.bean.TaskStatus taskStatus;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.Action{
    public top.bogey.auto_touch.room.bean.ActionMode actionMode;
    public boolean enable;
    public java.util.List keys;
    public top.bogey.auto_touch.room.bean.Node target;
    public top.bogey.auto_touch.room.bean.Node stop;
    public int delay;
    public int times;
    public int interval;
    public int time;
}

-keepclassmembers class top.bogey.auto_touch.room.bean.Node{
    public top.bogey.auto_touch.room.bean.NodeType type;
    private java.lang.String value;
}

-keep class top.bogey.auto_touch.util.MatchResult{*;}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile