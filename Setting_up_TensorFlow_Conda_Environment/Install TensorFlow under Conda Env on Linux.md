     
   
    # In a bash shell under Linux
    
    # Install Miniconda
    bash Miniconda3-latest-Linux-x86_64.sh

    # If you'd prefer that conda's base environment not be activated on startup, 
    #   set the auto_activate_base parameter to false: 

    conda config --set auto_activate_base false

    
    # In the first two sections of code below, the only real difference is that for the CPU version "tensorflow" is installed, 
    #    and for the gpu version "tensorflow-gpu" is installed.
       
    
    # ---------- Tensorflow CPU only, no GPU nor TensorRT. (Using the conda-forge package manager.) --------------
    
    conda create -y -p tf_cpu_only python=3.8
    
    conda env list

    # If there is no name given using 'conda env list' use the full path, 
    #     e.g.: conda activate /more_home/h_jwallace/Python/tf_cpu_only
    conda activate tf_cpu_only 
    
    python --version
    
    conda install -y -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
    
    # If versions and builds are not listed below, something went wrong
    conda list cudatoolkit
    
    conda list cudnn
         
    # Check CUDA installation 
    nvcc --version # If nvcc is installed
    conda list cuda

    # conda install tensorflow -c conda-forge    
    conda install -y -c conda-forge tensorflow=2.10.1
    
    # Find version number of tensorflow
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
    
    print(tf.reduce_sum(tf.random.normal([1000, 1000])))
    
    quit()
    >>> 
    
    conda deactivate
    
 
     
   
