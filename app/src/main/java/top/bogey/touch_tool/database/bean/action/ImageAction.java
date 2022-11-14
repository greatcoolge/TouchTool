package top.bogey.touch_tool.database.bean.action;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Parcel;
import android.util.Base64;

import androidx.annotation.NonNull;

import java.io.ByteArrayOutputStream;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.R;
import top.bogey.touch_tool.database.bean.Behavior;
import top.bogey.touch_tool.database.bean.Task;
import top.bogey.touch_tool.database.data.TaskRunningInfo;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class ImageAction extends Action {
    private transient Bitmap bitmap;

    private transient Bitmap scaleBitmap;
    private transient float scale = 1;

    private int screen;
    private String image;
    private int value;

    public ImageAction() {
        super(ActionType.IMAGE);
        value = 95;
    }

    protected ImageAction(Parcel in) {
        super(ActionType.IMAGE);
        screen = in.readInt();
        image = in.readString();
        value = in.readInt();
    }

    private Rect matchImage(MainAccessibilityService service) {
        if (!service.isCaptureEnabled()) return null;

        int width = DisplayUtils.getScreen(service);
        float scale = ((float) width) / screen;
        if (scale == 1) {
            return service.binder.matchImage(getBitmap(), value);
        } else {
            return service.binder.matchImage(getScaleBitmap(scale), value);
        }
    }

    @Override
    public boolean isValid() {
        return getBitmap() != null;
    }

    @Override
    public boolean checkCondition(MainAccessibilityService service) {
        return matchImage(service) != null;
    }

    @Override
    public boolean doAction(Task task, MainAccessibilityService service, TaskRunningInfo runningInfo) {
        if (!super.doAction(task, service, runningInfo)) return false;

        if (!service.isCaptureEnabled()) return false;

        Rect rect = matchImage(service);
        if (rect == null) return false;

        Path path = new Path();
        Point fixedPosition = AppUtils.getFixedPosition(rect.centerX(), rect.centerY());
        path.moveTo(fixedPosition.x, fixedPosition.y);
        int time = getTimeArea().getRandomTime();
        service.runGesture(path, time, null);
        sleep(time);
        return true;
    }

    @Override
    public String getDescription(Context context, Task task, Behavior behavior) {
        if (context == null) return String.valueOf(value);
        String touch = context.getString(timeArea.getMax() > 100 ? R.string.long_touch : R.string.touch);
        return context.getString(R.string.action_image, touch);
    }

    @Override
    public String getConditionContent(Context context, Task task, Behavior behavior) {
        return context.getString(R.string.condition_image);
    }

    @Override
    public String getConditionHint(Context context, Task task, Behavior behavior) {
        return context.getString(R.string.condition_image_tips);
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(screen);
        dest.writeString(image);
        dest.writeInt(value);
    }

    public Bitmap getBitmap() {
        if (bitmap == null && (image != null && !image.isEmpty())) {
            try {
                byte[] bitmapArray = Base64.decode(image, Base64.DEFAULT);
                bitmap = BitmapFactory.decodeByteArray(bitmapArray, 0, bitmapArray.length);
            } catch (IllegalArgumentException ignored) {
            }
        }
        return bitmap;
    }

    public void setBitmap(Bitmap bitmap, int screen) {
        this.bitmap = bitmap;
        this.screen = screen;

        if (bitmap == null) image = "";
        else {
            ByteArrayOutputStream stream = new ByteArrayOutputStream();
            bitmap.compress(Bitmap.CompressFormat.JPEG, 100, stream);
            byte[] bytes = stream.toByteArray();
            image = Base64.encodeToString(bytes, Base64.DEFAULT);
        }

        if (scaleBitmap != null) {
            scaleBitmap.recycle();
            scaleBitmap = null;
        }
    }

    public Bitmap getScaleBitmap(float scale) {
        if (scaleBitmap == null || scale != this.scale) {
            Bitmap bitmap = getBitmap();
            if (bitmap != null) {
                Matrix matrix = new Matrix();
                matrix.postScale(scale, scale);
                scaleBitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
            }
        }
        this.scale = scale;
        return scaleBitmap;
    }

    public int getScreen() {
        return screen;
    }

    public String getImage() {
        return image;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }
}
