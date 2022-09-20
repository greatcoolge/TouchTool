package top.bogey.touch_tool.ui.setting;

import android.animation.ValueAnimator;
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

import top.bogey.touch_tool.databinding.FloatDebugBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewHelper;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

public class DebugFloatView extends FrameLayout implements FloatViewInterface {
    private final FloatDebugBinding binding;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    private boolean isMin = false;

    @SuppressLint("ClickableViewAccessibility")
    public DebugFloatView(@NonNull Context context) {
        super(context);
        binding = FloatDebugBinding.inflate(LayoutInflater.from(context), this, true);

        DebugRecyclerViewAdapter adapter = new DebugRecyclerViewAdapter(this);
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

            FloatViewHelper helper = EasyFloat.getHelper(DebugFloatView.class.getCanonicalName());

            int buttonY = DisplayUtils.dp2px(context, 2);
            int buttonX = buttonY * 4;
            int buttonXY = buttonY * 3;
            int minWidth = buttonX * 4;
            int maxWidth = buttonX * 30;

            MaterialCardView root = binding.getRoot();
            ViewGroup.LayoutParams rootLayoutParams = root.getLayoutParams();

            ValueAnimator valueAnimator;
            if (isMin){
                valueAnimator = ValueAnimator.ofInt(100, 0);
                binding.closeButton.setVisibility(GONE);
                binding.markBox.setVisibility(GONE);
                binding.shapeableImageView.setVisibility(GONE);

            } else {
                valueAnimator = ValueAnimator.ofInt(0, 100);
                binding.closeButton.setVisibility(VISIBLE);
                binding.markBox.setVisibility(VISIBLE);
                binding.shapeableImageView.setVisibility(VISIBLE);

                helper.params.width = maxWidth;
                helper.params.height = maxWidth;
                helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
            }

            valueAnimator.addUpdateListener(animation -> {
                int value = (int) animation.getAnimatedValue();
                int size = minWidth + (int) ((maxWidth - minWidth) * value / 100f);
                rootLayoutParams.width = size;
                rootLayoutParams.height = size;
                root.setLayoutParams(rootLayoutParams);

                int x = buttonXY + (int) ((buttonX - buttonXY) * value / 100f);
                int y = buttonXY + (int) ((buttonY - buttonXY) * value / 100f);

                binding.minButton.setX(x);
                binding.minButton.setY(y);
                binding.minButton.setRotation(180 * (100 - value) / 100f);

                if (isMin && value == 0){
                    helper.params.width = minWidth;
                    helper.params.height = minWidth;
                    helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
                }
            });
            valueAnimator.start();
        });
    }

    @Override
    public void show() {
        EasyFloat.with(getContext())
                .setLayout(this)
                .setTag(DebugFloatView.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.CENTER, 0, 0)
                .hasEditText(true)
                .setAnimator(null)
                .setAlwaysShow(true)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(DebugFloatView.class.getCanonicalName());
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
}
