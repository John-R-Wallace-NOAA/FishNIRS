     
   
    # In a bash shell under Linux
    
    # Install Miniconda
    bash Miniconda3-latest-Linux-x86_64.sh
    
    # In the first two sections of code below, the only real difference is that for the CPU version "tensorflow" is installed, 
    #    and for the gpu version "tensorflow-gpu" is installed.
       
    
    # ---------- Tensorflow CPU only, no GPU nor TensorRT. (Using the conda-forge package manager.) --------------
    
    conda create -y -p tf_cpu_only python=3.8
    
    conda env list

    # If there is no name given using 'conda env list' use the full path, e.g.: conda activate /more_home/h_jwallace/Python/tf_cpu_only
    conda activate tf_cpu_only 
    
    python --version
    
    conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
    
    # If versions and builds are not listed below, something went wrong
    conda list cudatoolkit
    
    conda list cudnn
         
    # Check CUDA installation 
    nvcc --version # If nvcc is installed
    conda list cuda
        
    conda install tensorflow -c conda-forge
    
    # Find version number of tensorflow (2.10.1)
    conda list tensorflow
        
    # Verfiy TensorFlow using an interactive approach
    python
    >>> 
    import tensorflow as tf
    print(tf.__version__)
    print(tf)
    
    tf.config.list_physical_devices('CPU')
    tf.config.list_physical_devices('GPU')
    
    a = tf.constant(7)
    b = tf.constant(10)
    print(tf.add(a,b))
    
    tf.reduce_sum(tf.random.normal([1000, 1000]))
    
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
    
    # Check CUDA installation 
    nvcc --version # If nvcc is installed
    conda list cuda
    
    conda install tensorflow-gpu -c conda-forge
      
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
    
    tf.reduce_sum(tf.random.normal([1000, 1000]))
    
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
    
    tf.reduce_sum(tf.random.normal([1000, 1000]))
    
    quit()
    >>> 
    
    conda deactivate
     
   
