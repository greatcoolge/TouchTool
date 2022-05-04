package top.bogey.auto_touch.ui.picker;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.PorterDuffXfermode;
import android.graphics.Rect;
import android.graphics.RectF;
import android.view.MotionEvent;
import android.view.View;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.SelectCallback;

public class ImagePickerView extends View {
    private SelectCallback callback;

    private final Paint bgPaint, markPaint, vertexPaint, bitPaint;
    public Rect markArea;
    private final Rect confirmArea, cancelArea;
    private final RectF ltArea, rbArea;
    private final Bitmap confirmBitmap, cancelBitMap;

    private boolean isMarked, isDrag, isComplete = false;
    private int adjustMode = 0;

    private int lastX, lastY, startX, startY, endX, endY = 0;

    private final int vertexWidth, btnWidth;

    public ImagePickerView(Context context) {
        super(context);
        bgPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        bgPaint.setColor(Color.parseColor("#88000000"));

        markPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        markPaint.setStyle(Paint.Style.FILL_AND_STROKE);
        markPaint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.CLEAR));
        markPaint.setColor(Color.parseColor("#00000000"));

        vertexPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        vertexPaint.setColor(Color.parseColor("#009688"));

        bitPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
        bitPaint.setFilterBitmap(true);
        bitPaint.setDither(true);

        markArea = new Rect();
        ltArea = new RectF();
        rbArea = new RectF();
        confirmArea = new Rect();
        cancelArea = new Rect();

        confirmBitmap = BitmapFactory.decodeResource(getResources(), R.mipmap.ic_save);
        cancelBitMap = BitmapFactory.decodeResource(getResources(), R.mipmap.ic_close);

        vertexWidth = AppUtil.dp2px(context, 20);
        btnWidth = confirmBitmap.getWidth();
    }

    public ImagePickerView(Context context, SelectCallback callback) {
        this(context);
        this.callback = callback;
    }

    public Rect getMarkArea(){
        int realStatusBarHeight = AppUtil.getRealStatusBarHeight(MainApplication.getActivity());
        markArea.top += realStatusBarHeight;
        markArea.bottom += realStatusBarHeight;
        return markArea;
    }

    @Override
    public void draw(Canvas canvas) {
        if (isComplete) {
            super.draw(canvas);
            if (callback != null){
                callback.onEnter();
                callback = null;
            }
            return;
        }

        canvas.drawRect(0, 0, getWidth(), getHeight(), bgPaint);

        if (isMarked){
            canvas.drawRect(markArea, markPaint);
            if (!isDrag){
                canvas.drawRect(ltArea, vertexPaint);
                canvas.drawRect(rbArea, vertexPaint);

                if (adjustMode == 0){
                    canvas.drawBitmap(confirmBitmap, null, confirmArea, bitPaint);
                    canvas.drawBitmap(cancelBitMap, null, cancelArea, bitPaint);
                }
            }
        } else {
            if (Math.abs(endX - startX) > 0 && Math.abs(endY - startY) > 0){
                canvas.drawRect(Math.min(startX, endX), Math.min(startY, endY), Math.max(startX, endX), Math.max(startY, endY), markPaint);
            }
        }

        super.draw(canvas);
    }

    public void onTouch(MotionEvent event) {
        int x = (int) event.getX();
        int y = (int) event.getY();
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                isDrag = false;
                if (isMarked) {
                    lastX = x;
                    lastY = y;
                    adjustMode = 0;
                    if (confirmArea.contains(lastX, lastY)) {
                        isComplete = true;
                    } else if (cancelArea.contains(lastX, lastY)) {
                        isMarked = false;
                        markArea.set(0, 0, 0, 0);
                        refreshButtonArea();
                    } else if (ltArea.contains(x, y)) {
                        adjustMode = 1;
                    } else if (rbArea.contains(x, y)) {
                        adjustMode = 2;
                    } else if (markArea.contains(x, y)) {
                        isDrag = true;
                    }
                } else {
                    startX = endX = lastX = x;
                    startY = endY = lastY = y;
                }
                break;

            case MotionEvent.ACTION_MOVE:
                if (isMarked){
                    adjustMark(x, y);
                    lastX = x;
                    lastY = y;
                } else {
                    endX = lastX = x;
                    endY = lastY = y;
                    isDrag = true;
                }
                break;

            case MotionEvent.ACTION_UP:
                if (isMarked){
                    isDrag = false;
                    adjustMode = 0;
                } else {
                    if (isDrag){
                        markArea.set(Math.min(startX, endX), Math.min(startY, endY), Math.max(startX, endX), Math.max(startY, endY));
                        ltArea.set(markArea.left - vertexWidth, markArea.top - vertexWidth, markArea.left, markArea.top);
                        rbArea.set(markArea.right, markArea.bottom, markArea.right + vertexWidth, markArea.bottom + vertexWidth);
                        isMarked = true;
                        isDrag = false;
                    }
                }
                refreshButtonArea();
                break;
        }
        postInvalidate();
    }

    private void adjustMark(int x, int y){
        int dx = x - lastX;
        int dy = y - lastY;
        if (isDrag){
            startX = Math.max(0, startX + dx);
            startY = Math.max(0, startY + dy);
            endX = Math.min(getWidth(), endX + dx);
            endY = Math.min(getHeight(), endY + dy);
        } else {
            switch (adjustMode){
                case 1:
                    startX = Math.max(0, startX + dx);
                    startY = Math.max(0, startY + dy);
                    break;
                case 2:
                    endX = Math.min(getWidth(), endX + dx);
                    endY = Math.min(getHeight(), endY + dy);
                    break;
            }
        }
        markArea.set(Math.min(startX, endX), Math.min(startY, endY), Math.max(startX, endX), Math.max(startY, endY));
        ltArea.set(markArea.left - vertexWidth, markArea.top - vertexWidth, markArea.left, markArea.top);
        rbArea.set(markArea.right, markArea.bottom, markArea.right + vertexWidth, markArea.bottom + vertexWidth);
    }

    private void refreshButtonArea(){
        startX = markArea.left;
        startY = markArea.top;
        endX = markArea.right;
        endY = markArea.bottom;

        if (isMarked){
            int left = Math.max(endX - vertexWidth - btnWidth * 2, 0);

            if (startY > btnWidth * 3){
                // 顶部
                confirmArea.set(left, startY - btnWidth - vertexWidth, left + btnWidth, startY - vertexWidth);
                cancelArea.set(left + btnWidth + vertexWidth, startY - btnWidth - vertexWidth, left + btnWidth * 2 + vertexWidth, startY - vertexWidth);
            } else {
                // 底部
                confirmArea.set(left, endY + vertexWidth, left + btnWidth, endY + vertexWidth + btnWidth);
                cancelArea.set(left + btnWidth + vertexWidth, endY + vertexWidth, left + btnWidth * 2 + vertexWidth, endY + vertexWidth + btnWidth);
            }
        }
    }
}
