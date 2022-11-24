package top.bogey.touch_tool.ui.record;

import android.content.Intent;
import android.service.quicksettings.Tile;
import android.service.quicksettings.TileService;
import android.view.View;

import top.bogey.touch_tool.MainActivity;
import top.bogey.touch_tool.MainApplication;
import top.bogey.touch_tool.utils.easy_float.EasyFloat;

public class QuickRecordService extends TileService {

    @Override
    public void onClick() {
        super.onClick();
        Tile tile = getQsTile();
        if (tile == null) return;
        int state = tile.getState();
        MainActivity activity = MainApplication.getActivity();
        if (state == Tile.STATE_ACTIVE) {
            EasyFloat.dismiss(RecordFloatView.class.getCanonicalName());
            tile.setState(Tile.STATE_INACTIVE);
        } else {
            if (activity != null) {
                activity.showQuickMenu();
            } else {
                Intent intent = new Intent(this, MainActivity.class);
                intent.putExtra("IsBackground", true);
                intent.putExtra("ShowQuickMenu", true);
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(intent);
            }
            tile.setState(Tile.STATE_ACTIVE);
        }
        tile.updateTile();
    }

    @Override
    public void onStartListening() {
        super.onStartListening();
        Tile tile = getQsTile();
        if (tile == null) return;
        View view = EasyFloat.getView(RecordFloatView.class.getCanonicalName());
        tile.setState(view == null ? Tile.STATE_INACTIVE : Tile.STATE_ACTIVE);
        tile.updateTile();
    }
}
