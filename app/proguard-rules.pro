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

-keepclassmembers class top.bogey.touch_tool.database.bean.Task{
}

-keepclassmembers class top.bogey.touch_tool.database.bean.Behavior{
    private top.bogey.touch_tool.database.bean.BehaviorMode behaviorMode;
    private boolean enable;
    private java.util.List actions;
    private top.bogey.touch_tool.database.bean.action.Action condition;
    private int times;
    private java.lang.String title;
}

-keepclassmembers class top.bogey.touch_tool.database.bean.action.Action{
    protected top.bogey.touch_tool.database.bean.action.ActionType type;
    protected top.bogey.touch_tool.database.bean.action.TimeArea timeArea;
}

-keepclassmembers class android.graphics.Point{
    public int x;
    public int y;
}

-keepclassmembers class top.bogey.touch_tool.database.bean.action.TimeArea{
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