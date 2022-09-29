package top.bogey.touch_tool.room.bean.node;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Matrix;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.Rect;
import android.util.Base64;

import java.io.ByteArrayOutputStream;

import top.bogey.touch_tool.MainAccessibilityService;
import top.bogey.touch_tool.utils.AppUtils;
import top.bogey.touch_tool.utils.DisplayUtils;

public class ImageNode extends Node{
    public ImageNode(ImageInfo value) {
        super(NodeType.IMAGE, value);
    }

    @Override
    public ImageInfo getValue() {
        return (ImageInfo) value;
    }

    @Override
    public boolean isValid() {
        return getValue().getBitmap() != null;
    }

    @Override
    public boolean checkNode(Object obj) {
        if (obj != null){
            MainAccessibilityService service = (MainAccessibilityService) obj;
            if (service.isCaptureEnabled() && service.binder != null){
                return matchImage(service) != null;
            }
        }
        return false;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        if (obj != null){
            MainAccessibilityService service = (MainAccessibilityService) obj;
            if (service.isCaptureEnabled() && service.binder != null){
                Rect rect = matchImage(service);
                if (rect != null){
                    Path path = new Path();
                    Point fixedPosition = AppUtils.getFixedPosition(service, rect.centerX(), rect.centerY());
                    path.moveTo(fixedPosition.x, fixedPosition.y);
                    return path;
                }
            }
        }
        return null;
    }

    private Rect matchImage(MainAccessibilityService service){
        ImageInfo imageInfo = getValue();
        int width = DisplayUtils.getScreen(service);
        float scale = ((float) width) / imageInfo.screen;
        if (scale == 1){
            return service.binder.matchImage(imageInfo.getBitmap(), imageInfo.getValue());
        } else {
            Bitmap scaleBitmap = imageInfo.getScaleBitmap(scale);
            return service.binder.matchImage(scaleBitmap, imageInfo.getValue());
        }
    }

    @Override
    public ImageInfo cloneValue() {
        ImageInfo value = getValue();
        return new ImageInfo(value.image, value.value, value.screen);
    }

    public static class ImageInfo{
        private transient Bitmap bitmap;

        private transient Bitmap scaleBitmap;
        private transient float scale = 1;

        private String image;
        private int value;
        private int screen;

        public ImageInfo(int value) {
            this.value = value;
        }

        public ImageInfo(String image, int value, int screen) {
            this.image = image;
            this.value = value;
            this.screen = screen;
        }

        public Bitmap getBitmap() {
            if (bitmap == null && (image != null && !image.isEmpty())){
                try {
                    byte[] bitmapArray = Base64.decode(image, Base64.DEFAULT);
                    bitmap = BitmapFactory.decodeByteArray(bitmapArray, 0, bitmapArray.length);
                } catch (IllegalArgumentException ignored){}
            }
            return bitmap;
        }

        public void setBitmap(Bitmap bitmap, int screenWidth) {
            if (bitmap == null) image = "";
            else {
                ByteArrayOutputStream stream = new ByteArrayOutputStream();
                bitmap.compress(Bitmap.CompressFormat.JPEG, 100, stream);
                byte[] bytes = stream.toByteArray();
                image = Base64.encodeToString(bytes, Base64.DEFAULT);
            }
            this.bitmap = bitmap;
            if (scaleBitmap != null){
                scaleBitmap.recycle();
                scaleBitmap = null;
            }
            this.screen = screenWidth;
        }

        public Bitmap getScaleBitmap(float scale) {
            if (scaleBitmap == null || scale != this.scale){
                Bitmap bitmap = getBitmap();
                if (bitmap != null){
                    Matrix matrix = new Matrix();
                    matrix.postScale(scale, scale);
                    scaleBitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
                }
            }
            this.scale = scale;
            return scaleBitmap;
        }

        public int getValue() {
            return value;
        }

        public void setValue(int value) {
            this.value = value;
        }

        public String getImage() {
            return image;
        }

        public int getScreen() {
            return screen;
        }
    }
}
