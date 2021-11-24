package top.bogey.auto_touch.ui.picker;

import android.annotation.SuppressLint;
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
        vertexPaint.setColor(Color.parseColor("#ffc107"));

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

    @Override
    public void draw(Canvas canvas) {
        if (isComplete) {
            super.draw(canvas);
            if (callback != null){
                callback.onEnter();
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

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            int x = (int) event.getX();
            int y = (int) event.getY();

            if (isMarked) {
                lastX = x;
                lastY = y;
                isDrag = false;
                adjustMode = 0;
                if (confirmArea.contains(lastX, lastY)){
                    isComplete = true;
                } else if (cancelArea.contains(lastX, lastY)){
                    isMarked = false;
                    markArea.set(0, 0, 0, 0);
                    refreshButtonArea();
                }else if (ltArea.contains(x, y)) {
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
        }

        postInvalidate();
        return true;
    }

    public void onDrag(MotionEvent event){
        int x = (int) event.getX();
        int y = (int) event.getY();

        if (isMarked){
            if (event.getAction() == MotionEvent.ACTION_MOVE) {
                adjustMark(x, y);
                lastX = x;
                lastY = y;
            }
        } else {
            if (event.getAction() == MotionEvent.ACTION_MOVE) {
                endX = lastX = x;
                endY = lastY = y;
            }
        }
        postInvalidate();
    }

    public void onDragEnd(){
        if (isMarked){
            isDrag = false;
            adjustMode = 0;
        } else {
            markArea.set(Math.min(startX, endX), Math.min(startY, endY), Math.max(startX, endX), Math.max(startY, endY));
            ltArea.set(markArea.left, markArea.top, markArea.left + vertexWidth, markArea.top + vertexWidth);
            rbArea.set(markArea.right - vertexWidth, markArea.bottom - vertexWidth, markArea.right, markArea.bottom);
            isMarked = true;
        }
        refreshButtonArea();
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
        ltArea.set(markArea.left, markArea.top, markArea.left + vertexWidth, markArea.top + vertexWidth);
        rbArea.set(markArea.right - vertexWidth, markArea.bottom - vertexWidth, markArea.right, markArea.bottom);
    }

    private void refreshButtonArea(){
        startX = markArea.left;
        startY = markArea.top;
        endX = markArea.right;
        endY = markArea.bottom;

        if (isMarked){
            int left = Math.max(endX - vertexWidth * 2 - btnWidth * 2, 0);
            int outLeft = left + vertexWidth;

            if (markArea.width() > btnWidth * 5 && markArea.height() > btnWidth * 5){
                // 内部
                confirmArea.set(left, endY - btnWidth - vertexWidth, left + btnWidth, endY - vertexWidth);
                cancelArea.set(left + btnWidth + vertexWidth, endY - btnWidth - vertexWidth, left + btnWidth * 2 + vertexWidth, endY - vertexWidth);
            } else if (startY > btnWidth * 3){
                // 顶部
                confirmArea.set(outLeft, startY - btnWidth - vertexWidth, outLeft + btnWidth, startY - vertexWidth);
                cancelArea.set(outLeft + btnWidth + vertexWidth, startY - btnWidth - vertexWidth, outLeft + btnWidth * 2 + vertexWidth, startY - vertexWidth);
            } else {
                // 底部
                confirmArea.set(outLeft, endY + vertexWidth, outLeft + btnWidth, endY + vertexWidth + btnWidth);
                cancelArea.set(outLeft + btnWidth + vertexWidth, endY + vertexWidth, outLeft + btnWidth * 2 + vertexWidth, endY + vertexWidth + btnWidth);
            }
        }
    }
}
