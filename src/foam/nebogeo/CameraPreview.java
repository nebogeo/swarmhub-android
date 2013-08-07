package foam.nebogeo;

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import android.app.Activity;
import android.content.pm.PackageManager;
import android.hardware.Camera;
import android.hardware.Camera.CameraInfo;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.Surface;
import android.widget.Toast;
import java.io.IOException;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.Display;
import android.view.WindowManager;
import android.content.Context;
import android.hardware.Camera.Parameters;
import android.hardware.Camera.PictureCallback;

public class CameraPreview extends SurfaceView implements
        SurfaceHolder.Callback {

    private SurfaceHolder mSurfaceHolder;
    PictureTaker mPictureTaker;
    Context mCtx;

    // Constructor that obtains context and camera
    @SuppressWarnings("deprecation")
    public CameraPreview(Context context, PictureTaker picturetaker) {
        super(context);
        mCtx=context;
        mPictureTaker=picturetaker;
        Log.i("DORIS","CameraPreview ctr");
        this.mSurfaceHolder = this.getHolder();
        this.mSurfaceHolder.addCallback(this);
        this.mSurfaceHolder.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
    }

    public void TakePicture(PictureCallback cb) {
        Log.i("starwisp","taking picture");
        mPictureTaker.TakePicture(this,cb);
    }

    public void Shutdown() {
        mPictureTaker.Shutdown();
    }

    @Override
    public void surfaceCreated(SurfaceHolder surfaceHolder) {
        Log.i("DORIS","CameraPreview surface created");
        mSurfaceHolder=surfaceHolder;
        Log.i("DORIS",""+mPictureTaker);
        mPictureTaker.Startup(this);
    }

    @Override
    public void surfaceDestroyed(SurfaceHolder surfaceHolder) {
        Log.i("DORIS","preview surfaceDestroyed");
    }

    @Override
    public void surfaceChanged(SurfaceHolder surfaceHolder, int format,
            int width, int height) {
        Log.i("DORIS","preview surfacechanged");
        mSurfaceHolder=surfaceHolder;

        mPictureTaker.mCam.stopPreview();
        Parameters parameters = mPictureTaker.mCam.getParameters();
        Display display = ((WindowManager)mCtx.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay();

        if(display.getRotation() == Surface.ROTATION_0)
        {
            parameters.setPreviewSize(height, width);
            mPictureTaker.mCam.setDisplayOrientation(90);
        }

        if(display.getRotation() == Surface.ROTATION_90)
        {
            parameters.setPreviewSize(width, height);
        }

        if(display.getRotation() == Surface.ROTATION_180)
        {
            parameters.setPreviewSize(height, width);
        }

        if(display.getRotation() == Surface.ROTATION_270)
        {
            parameters.setPreviewSize(width, height);
            mPictureTaker.mCam.setDisplayOrientation(180);
        }

        mPictureTaker.mCam.setParameters(parameters);
        mPictureTaker.mCam.startPreview();

    }
}