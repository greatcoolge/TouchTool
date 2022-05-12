package top.bogey.auto_touch.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.recyclerview.widget.LinearLayoutManager;

import top.bogey.auto_touch.MainApplication;
import top.bogey.auto_touch.databinding.FloatFragmentDebugBinding;
import top.bogey.auto_touch.ui.easy_float.EasyFloat;
import top.bogey.auto_touch.ui.easy_float.FloatGravity;
import top.bogey.auto_touch.ui.picker.FloatCallbackImpl;
import top.bogey.auto_touch.ui.picker.NodePickerInterface;

@SuppressLint("ViewConstructor")
public class DebugDialog extends FrameLayout implements NodePickerInterface {
    private FloatFragmentDebugBinding binding;
    private DebugRecyclerViewAdapter adapter;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    public DebugDialog(Context context){
        super(context);
    }

    @Override
    public void show(){
        initView();
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(DebugDialog.class.getCanonicalName())
                .setDragEnable(true)
                .setGravity(FloatGravity.CENTER, 0, 0)
                .setCallback(new FloatCallbackImpl())
                .hasEditText(true)
                .setAnimator(null)
                .show();
    }

    @Override
    public void dismiss() {
        EasyFloat.dismiss(DebugDialog.class.getCanonicalName());
    }

    @SuppressLint("ClickableViewAccessibility")
    public void initView() {
        binding = FloatFragmentDebugBinding.inflate(LayoutInflater.from(getContext()));
        addView(binding.getRoot());

        adapter = new DebugRecyclerViewAdapter();
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
    }

    public void addTips(String tip){
        post(() -> {
            adapter.addTip(tip);
            Log.d("TAG", "addTips: " + tip);
            binding.recyclerView.scrollToPosition(adapter.getItemCount() - 1);
        });
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
