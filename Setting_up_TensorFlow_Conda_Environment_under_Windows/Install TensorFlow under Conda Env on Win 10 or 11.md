     
     # To start, you first need to see your system's NVIDIA graphics card as a GPU under Task Manager on Windows 10 or 11:
     # That should not be a problem in Windows 10 or 11, see the link below for some general info, but use the install code below.
     #      https://hackmd.io/@husohome/Byb6kP6WP 
      
     # See here:
     #     https://towardsdatascience.com/setting-up-tensorflow-gpu-with-cuda-and-anaconda-onwindows-2ee9c39b5c44 
        
     # for a picture, but don't follow that site past installing Anaconda:
     #     https://www.anaconda.com/products/distribution
     
     # After the Anaconda installation follow the steps below.
     
     
     
     # TensorRT?????
     # https://david-littlefield.medium.com/how-to-install-the-nvidia-cuda-driver-toolkit-cudnn-and-tensorrt-on-windows-10-3fcf97e54522
     
      
     
     
     # ------------------------------- Install TensorFlow and CUDA in Conda under Windows 10 & 11; GPU works in Python and in R. Using a mix of conda-forge and pip. ------------------------------------
     
     # Bex T. in the the link below does not install the CUDA and cuDNN under Conda, but I get the Jupyter test from him
     # https://towardsdatascience.com/how-to-finally-install-tensorflow-gpu-on-windows-10-63527910f255   
     
     # Think of cuDNN (NVIDIA CUDA® Deep Neural Network library) as a library for Deep Learning using CUDA and CUDA as a way to talk to the GPU.
     
         
     # In the Anaconda Powershell Prompt window
     conda create -n tf python==3.8 conda=4.8
     
     conda info --envs
     
     conda activate tf
     
     conda install -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
     
     # Check CUDA installation 
     nvcc --version
     
     # Anything above 2.10.* is not supported on the GPU on Windows Native
     pip install "tensorflow<2.11"
     
     # Find version number of tensorflow (2.10.1)
     python -c "import tensorflow as tf;print(tf.__version__)"
     
     
     # Verfiy TensorFlow - single line approach
     python -c "import tensorflow as tf;print('\n\n\n====================== \n GPU Devices: ',tf.config.list_physical_devices('GPU'), '\n======================')"
     python -c "import tensorflow as tf;print('\n\n\n====================== \n', tf.reduce_sum(tf.random.normal([1000, 1000])), '\n======================' )"
     
     
     # Verfiy TensorFlow - interactive approach
     python
     >>> 
     import tensorflow as tf
     print(tf.__version__)
     print(tf)
     
     tf.config.list_physical_devices('CPU')
     tf.config.list_physical_devices('GPU')
     len(tf.config.list_physical_devices('GPU'))
     
     a = tf.constant(7)
     b = tf.constant(10)
     print(tf.add(a,b))
     
     quit()
     >>> 
     
     
     # Extra test in Jupyter 
     pip install jupyterlab ipykernel
     
     ipython kernel install --user --name=tf
     
     # Use ctrl-c to end
     mkdir jupyter_folder
     jupyter-notebook --notebook-dir jupyter_folder
     
     jupyter notebook
     
     # Create a new notebook and run this 4-line snippet:
     import tensorflow as tf
     from tensorflow.python.client import device_lib
     
     print("Num GPUs Available: ", len(tf.config.list_physical_devices('GPU')))
     device_lib.list_local_devices()
     
     
     # Back in the Anaconda Powershell Prompt
     ctrl-c
     
     conda deactivate
     

