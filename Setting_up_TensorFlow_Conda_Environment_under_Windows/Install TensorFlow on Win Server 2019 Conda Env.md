     
     # To start you first need to see the Nvidia graphics card as a GPU on your Windows Server 2019 client:
     #     https://community.esri.com/t5/implementing-arcgis-questions/enabling-gpu-rendering-on-windows-server-2016/td-p/658522
     
     # See here:
     #     https://towardsdatascience.com/setting-up-tensorflow-gpu-with-cuda-and-anaconda-onwindows-2ee9c39b5c44 
        
     # for a picture, but don't follow that site past installing Anaconda:
     #     https://www.anaconda.com/products/distribution
     
     # After the Anaconda installation follow the steps below.
     
     # TensorRT?????
     # https://david-littlefield.medium.com/how-to-install-the-nvidia-cuda-driver-toolkit-cudnn-and-tensorrt-on-windows-10-3fcf97e54522
     
     
     # ------------------------------- Tensorflow CPU only, no GPU. Using conda-forge. -----------------------------------------------------------
     
     # *** The no GPU version works in R under Windows Server 2019.  ***  (Note that there is a GPU version that works in R for Windows 10 & 11.)
     
     conda create -y -p tf_cpu_only python=3.8
     
     conda env list
     
     conda activate tf_cpu_only
     
     python --version
     
     conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
     
     # If versions and builds are not listed below, something went wrong
     conda list cudatoolkit
     conda list cudnn
     
     # TensorFlow CPU works in R under Windows Server 2019
     # https://conda-forge.org/blog/posts/2021-11-03-tensorflow-gpu/
     conda install tensorflow -c conda-forge
     
     
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
     
     conda deactivate
     
     
     
     # ------------------------------- tf_gpu_py; GPU works in Python, but not in R. Using conda-forge. -----------------------------------------------------------
     
     conda create -y -p tf_gpu_py python=3.8
     
     conda env list
     
     conda activate tf_gpu_py
     
     python --version
     
     conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
     
     conda list cudatoolkit
     conda list cudnn
     
     
     # TensorFlow GPU does work in Python under Windows Server 2019, but not R.
     # So here
     #     https://github.com/conda-forge/tensorflow-feedstock/pull/111
     # from here:
     #     https://conda-forge.org/blog/posts/2021-11-03-tensorflow-gpu/
     # is not updated, or they are nost using: cudatoolkit=11.2 cudnn=8.1.0?
     
     conda install tensorflow-gpu -c conda-forge
     
     
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
     
     conda deactivate
     
     
     
     # ------------- Mamba install - CPU only -------------------------
     
     # See Comment 3 for Linux here: https://stackoverflow.com/questions/54271094/conda-install-c-conda-forge-tensorflow-just-stuck-in-solving-environment
     
     conda create -y --name tf_gpu_3 python=3.8
     
     conda env list
     
     conda activate tf_gpu_3
     
     
     # tensorflow (mamba tensorflow-gpu failed under Windows Server 2019 and Windows 10)
     # Mamba install should include cudatoolkit and cudnn (so less steps)
     conda install -y -c conda-forge mamba
     mamba install -y -c conda-forge tensorflow 
     
     
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
     
     conda deactivate
     
     


