import System.Environment
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

data Window

foreign import ccall "gtk/gtk.h gtk_init" c_gtkInit ::
	Ptr CInt -> Ptr CString -> IO ()
foreign import ccall "gtk/gtk.h gtk_main" c_gtkMain :: IO ()

foreign import ccall "gtk/gtk.h gtk_window_new" c_gtkWindowNew ::
	CInt -> IO (Ptr Window)

foreign import ccall "gtk/gtk.h gtk_widget_set_size_request"
	c_gtkWidgetSetSizeRequest :: Ptr Window -> CInt -> CInt -> IO ()

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnect ::
	Ptr Window -> CString -> FunPtr (Ptr Window -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall "gtk/gtk.h gtk_widget_show" c_gtkWidgetShow ::
	Ptr Window -> IO ()

foreign import ccall "gtk/gtk.h &gtk_main_quit" c_gtkMainQuit ::
	FunPtr (Ptr Window -> Ptr () -> IO ())

main :: IO ()
main = do
	args <- getArgs
	allocaArray (length args) $ \ptr -> alloca $ \argc -> do
		mapM newCString args >>= pokeArray ptr
		poke argc $ fromIntegral $ length args
		c_gtkInit argc ptr
		w <- c_gtkWindowNew 0
		c_gtkWidgetSetSizeRequest w 300 200
		destroy <- newCString "destroy"
		c_gSignalConnect w destroy c_gtkMainQuit nullPtr nullPtr 0
		c_gtkWidgetShow w
		c_gtkMain
