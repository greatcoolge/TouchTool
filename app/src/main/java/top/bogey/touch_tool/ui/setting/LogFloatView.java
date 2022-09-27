package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.google.android.material.card.MaterialCardView;

import top.bogey.touch_tool.databinding.FloatLogBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewHelper;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

public class LogFloatView extends FrameLayout implements FloatViewInterface {
    private final FloatLogBinding binding;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    private boolean isMin = false;

    private boolean isExpand = false;

    @SuppressLint("ClickableViewAccessibility")
    public LogFloatView(@NonNull Context context) {
        super(context);
        binding = FloatLogBinding.inflate(LayoutInflater.from(context), this, true);

        LogRecyclerViewAdapter adapter = new LogRecyclerViewAdapter(this);
        binding.recyclerView.setAdapter(adapter);

        binding.recyclerView.setOnTouchListener((v, event) -> {
            ViewParent parent = getParent();
            if (parent != null){
                switch (event.getAction()){
                    case MotionEvent.ACTION_DOWN:
                        lastY = event.getY();
                        parent.requestDisallowInterceptTouchEvent(true);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        checkPosition(event.getY());
                        if (isToBottom || isToTop){
                            parent.requestDisallowInterceptTouchEvent(false);
                            return false;
                        } else {
                            parent.requestDisallowInterceptTouchEvent(true);
                        }
                        lastY = event.getY();
                        break;
                    case MotionEvent.ACTION_UP:
                    case MotionEvent.ACTION_CANCEL:
                        parent.requestDisallowInterceptTouchEvent(false);
                        break;
                }
            }
            return false;
        });

        binding.closeButton.setOnClickListener(v -> dismiss());

        binding.minButton.setOnClickListener(v -> {
            isMin = !isMin;
            doAni();
        });

        binding.minButton.setOnLongClickListener(v -> {
            isExpand = !isExpand;
            doAni();
            return true;
        });
    }

    @Override
    public void show() {
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(LogFloatView.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.CENTER, 0, 0)
                .hasEditText(true)
                .setAnimator(null)
                .setAlwaysShow(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(LogFloatView.class.getCanonicalName());
    }

    private void checkPosition(float nowY){
        LinearLayoutManager layoutManager = (LinearLayoutManager)binding.recyclerView.getLayoutManager();
        if (layoutManager != null){
            if (layoutManager.getItemCount() > 3){
                isToTop = false;
                isToBottom =false;
                int first = layoutManager.findFirstCompletelyVisibleItemPosition();
                int last = layoutManager.findLastCompletelyVisibleItemPosition();

                if (layoutManager.getChildCount() > 0){
                    if (last == layoutManager.getItemCount() - 1){
                        if (canScrollVertically(-1) && nowY < lastY){
                            isToBottom = true;
                        }
                    } else if (first == 0){
                        if (canScrollVertically(1) && nowY > lastY){
                            isToTop = true;
                        }
                    }
                }
            } else {
                isToTop = true;
                isToBottom = true;
            }
        }
    }

    private void doAni(){
        FloatViewHelper helper = EasyFloat.getHelper(LogFloatView.class.getCanonicalName());

        int buttonX = DisplayUtils.dp2px(getContext(), isMin ? 4 : 8);
        int buttonY = DisplayUtils.dp2px(getContext(), isMin ? 4 : 2);
        int bgWidth = DisplayUtils.dp2px(getContext(), isMin ? 32 : isExpand ? 320 : 240);
        int bgHeight = DisplayUtils.dp2px(getContext(), isMin ? 32 : isExpand ? 480 : 240);

        MaterialCardView root = binding.getRoot();
        ViewGroup.LayoutParams rootLayoutParams = root.getLayoutParams();

        if (isMin){
            binding.closeButton.setVisibility(GONE);
            binding.markBox.setVisibility(GONE);
            binding.drag.setVisibility(GONE);
            binding.minButton.setRotation(180);
        } else {
            binding.closeButton.setVisibility(VISIBLE);
            binding.markBox.setVisibility(VISIBLE);
            binding.drag.setVisibility(VISIBLE);
            binding.minButton.setRotation(0);
        }
        binding.minButton.setX(buttonX);
        binding.minButton.setY(buttonY);
        rootLayoutParams.width = bgWidth;
        rootLayoutParams.height = bgHeight;
        root.setLayoutParams(rootLayoutParams);
        helper.params.width = bgWidth;
        helper.params.height = bgHeight;
        helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
    }
}
