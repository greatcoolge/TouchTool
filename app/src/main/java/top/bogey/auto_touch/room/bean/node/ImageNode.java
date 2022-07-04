package top.bogey.auto_touch.room.bean.node;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Path;
import android.graphics.Rect;
import android.util.Base64;

import java.io.ByteArrayOutputStream;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainCaptureService;

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
                return service.binder.matchImage(getValue().getBitmap(), getValue().getValue()) != null;
            }
        }
        return false;
    }

    @Override
    public Object getNodeTarget(Object obj) {
        if (obj != null){
            MainAccessibilityService service = (MainAccessibilityService) obj;
            if (service.isCaptureEnabled() && service.binder != null){
                Rect rect = service.binder.matchImage(getValue().getBitmap(), getValue().getValue());
                if (rect != null){
                    Path path = new Path();
                    path.moveTo(rect.centerX(), rect.centerY());
                    return path;
                }
            }
        }
        return null;
    }

    public static class ImageInfo{
        private transient Bitmap bitmap;
        private String image;
        private int value;

        public ImageInfo(int value) {
            this.value = value;
        }

        public ImageInfo(String image, int value) {
            this.image = image;
            this.value = value;
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

        public void setBitmap(Bitmap bitmap) {
            if (bitmap == null) image = "";
            else {
                ByteArrayOutputStream stream = new ByteArrayOutputStream();
                bitmap.compress(Bitmap.CompressFormat.JPEG, 100, stream);
                byte[] bytes = stream.toByteArray();
                image = Base64.encodeToString(bytes, Base64.DEFAULT);
            }
            this.bitmap = bitmap;
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

        public void setImage(String image) {
            this.image = image;
        }
    }
}
