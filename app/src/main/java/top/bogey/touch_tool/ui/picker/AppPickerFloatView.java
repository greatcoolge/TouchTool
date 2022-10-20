package top.bogey.touch_tool.ui.picker;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Configuration;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.GridLayoutManager;

import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.MainViewModel;
import top.bogey.touch_tool.databinding.FloatPickerAppBinding;
import top.bogey.touch_tool.ui.apps.AppInfo;
import top.bogey.touch_tool.utils.DisplayUtils;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

@SuppressLint("ViewConstructor")
public class AppPickerFloatView extends BasePickerFloatView {
    private final FloatPickerAppBinding binding;
    private final MainViewModel viewModel;
    private final AppPickerRecyclerViewAdapter adapter;
    private String searchText = "";
    private AppInfo selectApp =  null;

    public AppPickerFloatView(Context context, PickerCallback pickerCallback) {
        super(context, pickerCallback);
        binding = FloatPickerAppBinding.inflate(LayoutInflater.from(context), this, true);
        viewModel = new ViewModelProvider(MainApplication.getActivity()).get(MainViewModel.class);
        adapter = new AppPickerRecyclerViewAdapter(this);
        binding.appsView.setAdapter(adapter);
        refreshSpawnCount();
        adapter.refreshApps(viewModel.searchAppList(searchText));

        binding.refreshButton.setOnClickListener(v -> {
            viewModel.showSystem.setValue(Boolean.FALSE.equals(viewModel.showSystem.getValue()));
            viewModel.refreshAppList();
            adapter.refreshApps(viewModel.searchAppList(searchText));
        });

        binding.titleEdit.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                searchText = s.toString();
                adapter.refreshApps(viewModel.searchAppList(searchText));
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

    private void refreshSpawnCount(){
        GridLayoutManager layoutManager = (GridLayoutManager) binding.appsView.getLayoutManager();
        if (layoutManager != null) {
            if (!DisplayUtils.isPortrait(getContext()))layoutManager.setSpanCount(5);
            else layoutManager.setSpanCount(3);
        }
    }

    public void setSelectApp(AppInfo info){
        selectApp = info;
        pickerCallback.onComplete(this);
        dismiss();
    }

    public AppInfo getSelectApp(){
        return selectApp;
    }
}
