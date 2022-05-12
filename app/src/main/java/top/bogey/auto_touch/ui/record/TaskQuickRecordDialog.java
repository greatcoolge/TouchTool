package top.bogey.auto_touch.ui.record;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.widget.FrameLayout;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import top.bogey.auto_touch.MainAccessibilityService;
import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.R;
import top.bogey.auto_touch.databinding.FloatFragmentRecordQuickBinding;
import top.bogey.auto_touch.room.bean.Action;
import top.bogey.auto_touch.room.bean.Node;
import top.bogey.auto_touch.room.bean.NodeType;
import top.bogey.auto_touch.room.bean.Pos;
import top.bogey.auto_touch.room.bean.Task;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.picker.FloatShowCallback;
import top.bogey.auto_touch.util.AppUtil;
import top.bogey.auto_touch.util.CompleteCallback;
import top.bogey.auto_touch.util.DouglasPeucker;

@SuppressLint("ViewConstructor")
public class TaskQuickRecordDialog extends FrameLayout {
    private final List<Action> actions;

    private final List<Point> currPoints = new ArrayList<>();
    private float lastX = 0;
    private float lastY = 0;
    private boolean isDrag = false;
    private boolean addEnable = true;

    int[] location = new int[2];

    private final Paint paint;

    private long startTime = 0;
    private long costTime = 0;

    public TaskQuickRecordDialog(Context context, Task task, CompleteCallback callback){
        super(context);
        actions = task.getActions();

        FloatFragmentRecordQuickBinding binding = FloatFragmentRecordQuickBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());

        binding.saveButton.setOnClickListener(v -> {
            addEnable = false;
            actions.remove(actions.size() - 1);
            task.setActions(actions);
            if (callback != null){
                callback.onComplete();
            }
            dismiss();
        });

        paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setColor(AppUtil.getAttrColor(getContext(), R.attr.colorSecondary, R.color.amber_500));
        paint.setStrokeWidth(10);
        paint.setStrokeCap(Paint.Cap.ROUND);
    }

    @Override
    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        super.onLayout(changed, left, top, right, bottom);
        getLocationOnScreen(location);
    }

    @Override
    protected void dispatchDraw(Canvas canvas) {
        super.dispatchDraw(canvas);

        if (currPoints.size() >= 2){
            for (int i = 0; i < currPoints.size() - 1; i++) {
                canvas.drawLine(currPoints.get(i).x, currPoints.get(i).y - location[1], currPoints.get(i + 1).x, currPoints.get(i + 1).y - location[1], paint);
            }
        }

        if (currPoints.size() >= 1) {
            Point point = currPoints.get(currPoints.size() - 1);
            canvas.drawCircle(point.x, point.y - location[1], 12, paint);
        }
    }

    public void onTouch(MotionEvent event){
        float x = event.getRawX();
        float y = event.getRawY();
        switch (event.getAction()){
            case MotionEvent.ACTION_DOWN:
                costTime = System.currentTimeMillis();
                // 未知原因，导致重复进入两次按下
                if (startTime != 0 && Math.abs(startTime - costTime) > 5){
                    Node node = new Node(NodeType.DELAY);
                    node.setDelay((int) (costTime - startTime));
                    Action action = new Action();
                    action.setTargets(new ArrayList<>(Collections.singletonList(node)));
                    action.setCondition(new Node(NodeType.NULL));
                    actions.add(action);
                    startTime = costTime;
                }
                lastX = x;
                lastY = y;
                isDrag = false;
                currPoints.clear();
                currPoints.add(new Point((int) x, (int) y));
                break;
            case MotionEvent.ACTION_MOVE:
                float dx = x - lastX;
                float dy = y - lastY;
                if (!isDrag && dx * dx + dy * dy < 81) return;
                isDrag = true;
                currPoints.add(new Point((int) x, (int) y));
                lastX = x;
                lastY = y;
                break;
            case MotionEvent.ACTION_UP:
                isDrag = false;
                costTime = System.currentTimeMillis() - costTime;
                addEnable = true;
                postDelayed(() -> {
                    if (addEnable){
                        Node node = new Node(NodeType.POS);
                        node.setPoses(getPoses(DouglasPeucker.compress(currPoints)));
                        node.setTime((int) costTime);
                        Action action = new Action();
                        action.setTargets(new ArrayList<>(Collections.singletonList(node)));
                        action.setCondition(new Node(NodeType.NULL));
                        actions.add(action);
                        MainAccessibilityService service = MainApplication.getService();
                        if (service != null){
                            EasyFloat.hide(TaskQuickRecordDialog.class.getCanonicalName());
                            post(() -> service.runGesture(getPath(node.getPoses()), node.getTime(), (result) -> {
                                EasyFloat.show(TaskQuickRecordDialog.class.getCanonicalName());
                                startTime = System.currentTimeMillis();
                                currPoints.clear();
                                postInvalidate();
                            }));
                        }
                    }
                }, 50);
                break;
        }
        postInvalidate();
    }

    private Path getPath(List<Pos> poses){
        if (poses != null && !poses.isEmpty()){
            Path path = new Path();
            Pos firstPos = AppUtil.percent2px(getContext(), poses.get(0));
            path.moveTo(firstPos.getX(), firstPos.getY());
            for (int i = 1; i < poses.size(); i++) {
                Pos pos = AppUtil.percent2px(getContext(), poses.get(i));
                path.lineTo(pos.getX(), pos.getY());
            }
            return path;
        }
        return null;
    }

    private List<Pos> getPoses(List<Point> points) {
        ArrayList<Pos> poses = new ArrayList<>();
        for (Point point : points) {
            poses.add(AppUtil.px2percent(getContext(), new Pos(point.x, point.y)));
        }
        return poses;
    }

    public void show(){
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(TaskQuickRecordDialog.class.getCanonicalName())
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(new TouchCallback())
                .setAnimator(null)
                .show();
    }

    public void dismiss() {
        EasyFloat.dismiss(TaskQuickRecordDialog.class.getCanonicalName());
    }

    private class TouchCallback extends FloatShowCallback {
        @Override
        public void onTouch(MotionEvent motionEvent) {
            super.onTouch(motionEvent);
            TaskQuickRecordDialog.this.onTouch(motionEvent);
        }
    }
}
