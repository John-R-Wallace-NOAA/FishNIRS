     
    # To start, you first need to see the Nvidia graphics card as a GPU on your Windows Server 2019 client:
    #     https://community.esri.com/t5/implementing-arcgis-questions/enabling-gpu-rendering-on-windows-server-2016/td-p/658522
    
    # For a picture, see here:
    #     https://towardsdatascience.com/setting-up-tensorflow-gpu-with-cuda-and-anaconda-onwindows-2ee9c39b5c44 
       
    # but don't follow that site past installing Anaconda:
    #     https://www.anaconda.com/products/distribution
    
    # After the Anaconda installation follow the steps below.
    
    # Here is a reference:
    #     https://conda-forge.org/blog/posts/2021-11-03-tensorflow-gpu/
    # but again, following the steps below works for me.
    
    # In the first two sections of code below, the only real difference is that for the CPU version "tensorflow" is installed, 
    #    and for the gpu version "tensorflow-gpu" is installed.
    
    
    # ---------- Tensorflow CPU only, no GPU nor TensorRT. (Using the conda-forge package manager.) --------------
    
    # *** The CPU only version does work in R under Windows Server 2019.  *** 
    # See the other markdown doc in this folder for a GPU version that works in R for Windows 10 & 11.
    
    conda create -y -p tf_cpu_only python=3.8
    
    conda env list
    
    conda activate tf_cpu_only
    
    python --version
    
    conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
    
    # If versions and builds are not listed below, something went wrong
    conda list cudatoolkit
    conda list cudnn
         
    # Check CUDA installation 
    nvcc --version
    conda list cuda
        
    conda install tensorflow -c conda-forge
    
    # Find version number of tensorflow (2.10.1)
    conda list tensorflow
              
    # Verfiy TensorFlow using a single line submission to python approach
    python -c "import tensorflow as tf;print('\n\n\n====================== \n GPU Devices: ',tf.config.list_physical_devices('GPU'), '\n======================')"
    python -c "import tensorflow as tf;print('\n\n\n====================== \n', tf.reduce_sum(tf.random.normal([1000, 1000])), '\n======================' )"
    
    
    # Verfiy TensorFlow using an interactive approach
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
    
 
    # ----------- Tensorflow GPU works in Python, but not in R. (Using the conda-forge package manager.) ---------------------
    
    conda create -y -p tf_gpu_py python=3.8
    
    conda env list
    
    conda activate tf_gpu_py
    
    python --version
    
    conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
    
    conda list cudatoolkit
    conda list cudnn
    
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
    
    
    
    # ------------- Mamba Package Manager install - only CPU worked for me ------------------
    
    # Main reference for Mamba:
         https://github.com/mamba-org/mamba
    
    # For code see the second Comment 3 by prerakmody here (I'm not sure why there are two Comment 3's.): 
         https://stackoverflow.com/questions/54271094/conda-install-c-conda-forge-tensorflow-just-stuck-in-solving-environment
    
    conda create -y --name tf_mamba python=3.8
    
    conda env list
    
    conda activate tf_mamba
    
    
    # Mamba install includes cudatoolkit and cudnn, so there are less steps and a nice interface, but 'tensorflow-gpu' failed  
    #       for me under Windows Server 2019 and Windows 10.
    conda install -y -c conda-forge mamba
    #  mamba install -y -c conda-forge tensorflow-gpu   
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
     
   
