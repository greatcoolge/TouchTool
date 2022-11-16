package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Configuration;
import android.text.Editable;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;

import java.util.List;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.databinding.FloatPickerAppBinding;
import top.bogey.touch_tool.ui.app.AppInfo;
import top.bogey.touch_tool.ui.app.AppRecyclerViewAdapter;
import top.bogey.touch_tool.ui.app.SelectAppCallback;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.TextChangedListener;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

@SuppressLint("ViewConstructor")
public class AppPickerFloatView extends BasePickerFloatView implements SelectAppCallback {
    private final FloatPickerAppBinding binding;
    private final MainViewModel viewModel;
    private final AppRecyclerViewAdapter adapter;
    private String searchText = "";
    private AppInfo selectApp = null;

    public AppPickerFloatView(Context context, PickerCallback pickerCallback) {
        super(context, pickerCallback);
        binding = FloatPickerAppBinding.inflate(LayoutInflater.from(context), this, true);
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
        adapter = new AppRecyclerViewAdapter(this, null);
        binding.appsView.setAdapter(adapter);
        refreshSpawnCount();
        adapter.refreshApps(viewModel.searchAppList(searchText, false));

        binding.refreshButton.setOnClickListener(v -> {
            viewModel.showSystem.setValue(Boolean.FALSE.equals(viewModel.showSystem.getValue()));
            viewModel.refreshAppList();
            adapter.refreshApps(viewModel.searchAppList(searchText, false));
        });

        binding.titleEdit.addTextChangedListener(new TextChangedListener() {
            @Override
            public void afterTextChanged(Editable s) {
                searchText = s.toString();
                adapter.refreshApps(viewModel.searchAppList(searchText, false));
            }
        });
    }

    @Override
    public void show() {
        EasyFloat.with(MainApplication.getActivity())
                .setLayout(this)
                .setTag(tag)
                .setDragEnable(false)
                .setMatch(true, true)
                .setCallback(floatCallback)
                .setAnimator(null)
                .hasEditText(true)
                .show();
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        refreshSpawnCount();
    }

    private void refreshSpawnCount() {
        GridLayoutManager layoutManager = (GridLayoutManager) binding.appsView.getLayoutManager();
        if (layoutManager != null) {
            if (!DisplayUtils.isPortrait(getContext())) layoutManager.setSpanCount(5);
            else layoutManager.setSpanCount(3);
        }
    }

    public AppInfo getSelectApp() {
        return selectApp;
    }

    @Override
    public void onSelectApps(List<AppInfo> apps) {
        if (apps != null && apps.size() > 0){
            selectApp = apps.get(0);
            pickerCallback.onComplete(this);
            dismiss();
        }
    }
}
