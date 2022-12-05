package top.bogey.touch_tool.ui.setting;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.res.ColorStateList;
import android.graphics.Point;
import android.net.Uri;
import android.text.Editable;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.core.content.FileProvider;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.google.android.material.button.MaterialButton;
import com.google.android.material.card.MaterialCardView;
import com.tencent.mmkv.MMKV;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import top.bogey.touch_tool.R;
import top.bogey.touch_tool.databinding.FloatLogBinding;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.TextChangedListener;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;
import top.bogey.touch_tool.utils.easy_float.FloatGravity;
import top.bogey.touch_tool.utils.easy_float.FloatViewHelper;
import top.bogey.touch_tool.utils.easy_float.FloatViewInterface;

public class LogFloatView extends FrameLayout implements FloatViewInterface {
    private static final String LOG_LEVEL = "log_level";

    private final FloatLogBinding binding;

    private float lastY = 0f;
    private boolean isToBottom = false;
    private boolean isToTop = true;

    private boolean isExpand = true;

    private boolean isZoom = false;

    private int level = (1 << LogLevel.values().length) - 1;

    @SuppressLint("ClickableViewAccessibility")
    public LogFloatView(@NonNull Context context) {
        super(context);
        binding = FloatLogBinding.inflate(LayoutInflater.from(context), this, true);

        level = MMKV.defaultMMKV().decodeInt(LOG_LEVEL, level);

        LogRecyclerViewAdapter adapter = new LogRecyclerViewAdapter(level);
        binding.recyclerView.setAdapter(adapter);

        binding.recyclerView.setOnTouchListener((v, event) -> {
            ViewParent parent = getParent();
            if (parent != null) {
                switch (event.getAction()) {
                    case MotionEvent.ACTION_DOWN:
                        lastY = event.getY();
                        parent.requestDisallowInterceptTouchEvent(true);
                        break;
                    case MotionEvent.ACTION_MOVE:
                        checkPosition(event.getY());
                        if (isToBottom || isToTop) {
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

        binding.closeButton.setOnLongClickListener(v -> {
            String SAVE_FILE = "error.txt";
            try (FileOutputStream fileOutputStream = context.openFileOutput(SAVE_FILE, Context.MODE_PRIVATE)) {
                fileOutputStream.write(adapter.getShowLogs().getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }

            Intent intent = new Intent(Intent.ACTION_SEND);
            File file = new File(context.getFilesDir(), SAVE_FILE);
            Uri fileUri = null;
            try {
                fileUri = FileProvider.getUriForFile(context, context.getPackageName() + ".file_provider", file);
            } catch (IllegalArgumentException ignored) {
            }
            if (fileUri != null) {
                intent.putExtra(Intent.EXTRA_STREAM, fileUri);
                String type = context.getContentResolver().getType(fileUri);
                intent.setType(type);
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_GRANT_READ_URI_PERMISSION);
                context.startActivity(Intent.createChooser(intent, context.getString(R.string.export_task_tips)));
            }
            return true;
        });

        binding.expandButton.setOnClickListener(v -> {
            isExpand = !isExpand;
            refreshUI();
        });

        binding.zoomButton.setOnClickListener(v -> {
            isZoom = !isZoom;
            refreshUI();
        });

        MaterialButton[] buttons = {binding.lowButton, binding.middleButton, binding.highButton};
        for (int i = 0; i < buttons.length; i++) {
            buttons[i].setAlpha((level & (1 << i)) > 0 ? 1f : 0.25f);
            buttons[i].setBackgroundTintList(ColorStateList.valueOf(LogLevel.values()[i].getLevelColor(context)));
        }

        binding.highButton.setOnClickListener(v -> {
            int value = 1 << LogLevel.HIGH.ordinal();
            level = level ^ value;
            binding.highButton.setAlpha((level & value) > 0 ? 1f : 0.25f);
            MMKV.defaultMMKV().encode(LOG_LEVEL, level);
            adapter.setLevel(level);
        });

        binding.middleButton.setOnClickListener(v -> {
            int value = 1 << LogLevel.MIDDLE.ordinal();
            level = level ^ value;
            binding.middleButton.setAlpha((level & value) > 0 ? 1f : 0.25f);
            MMKV.defaultMMKV().encode(LOG_LEVEL, level);
            adapter.setLevel(level);
        });

        binding.lowButton.setOnClickListener(v -> {
            int value = 1 << LogLevel.LOW.ordinal();
            level = level ^ value;
            binding.lowButton.setAlpha((level & value) > 0 ? 1f : 0.25f);
            MMKV.defaultMMKV().encode(LOG_LEVEL, level);
            adapter.setLevel(level);
        });

        binding.include.titleEdit.addTextChangedListener(new TextChangedListener() {
            @Override
            public void afterTextChanged(Editable s) {
                adapter.setSearchText(String.valueOf(s));
                if (s.length() > 0)
                    binding.include.textInputLayout.setHint(R.string.setting_running_log_search_hint);
                else
                    binding.include.textInputLayout.setHint(R.string.setting_running_log_search_tips);
            }
        });
        binding.include.textInputLayout.setHint(R.string.setting_running_log_search_tips);
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

    private void checkPosition(float nowY) {
        LinearLayoutManager layoutManager = (LinearLayoutManager) binding.recyclerView.getLayoutManager();
        if (layoutManager != null) {
            if (layoutManager.getItemCount() > 3) {
                isToTop = false;
                isToBottom = false;
                int first = layoutManager.findFirstCompletelyVisibleItemPosition();
                int last = layoutManager.findLastCompletelyVisibleItemPosition();

                if (layoutManager.getChildCount() > 0) {
                    if (last == layoutManager.getItemCount() - 1) {
                        if (canScrollVertically(-1) && nowY < lastY) {
                            isToBottom = true;
                        }
                    } else if (first == 0) {
                        if (canScrollVertically(1) && nowY > lastY) {
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

    private void refreshUI() {
        FloatViewHelper helper = EasyFloat.getHelper(LogFloatView.class.getCanonicalName());

        Point size = DisplayUtils.getScreenSize(getContext());
        int height = DisplayUtils.getStatusBarHeight(getContext());

        int bgWidth = DisplayUtils.dp2px(getContext(), isExpand ? (isZoom ? 320 : 240) : 32);
        int bgHeight = DisplayUtils.dp2px(getContext(), isExpand ? (isZoom ? 640 : 240) : 30);
        bgHeight = Math.min(bgHeight, size.y - height);

        MaterialCardView root = binding.getRoot();
        ViewGroup.LayoutParams rootLayoutParams = root.getLayoutParams();

        if (isExpand) {
            binding.markBox.setVisibility(VISIBLE);
            binding.drag.setVisibility(VISIBLE);
            binding.zoomButton.setVisibility(VISIBLE);
            binding.closeButton.setVisibility(VISIBLE);
            binding.highButton.setVisibility(VISIBLE);
            binding.middleButton.setVisibility(VISIBLE);
            binding.lowButton.setVisibility(VISIBLE);
            binding.include.getRoot().setVisibility(VISIBLE);
            binding.expandButton.setIconResource(R.drawable.icon_remove);
        } else {
            binding.markBox.setVisibility(GONE);
            binding.drag.setVisibility(GONE);
            binding.zoomButton.setVisibility(GONE);
            binding.closeButton.setVisibility(GONE);
            binding.highButton.setVisibility(GONE);
            binding.middleButton.setVisibility(GONE);
            binding.lowButton.setVisibility(GONE);
            binding.include.getRoot().setVisibility(GONE);
            binding.expandButton.setIconResource(R.drawable.icon_add);
        }

        binding.zoomButton.setIconResource(isZoom ? R.drawable.icon_zoom_in : R.drawable.icon_zoom_out);

        rootLayoutParams.width = bgWidth;
        rootLayoutParams.height = bgHeight;
        root.setLayoutParams(rootLayoutParams);
        helper.params.width = bgWidth;
        helper.params.height = bgHeight;
        helper.manager.updateViewLayout(helper.floatViewParent, helper.params);
        postDelayed(helper::initGravity, 50);
    }
}
