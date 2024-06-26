     
    # -- Install TensorFlow and CUDA in Conda under Win 10 & 11; GPU works in Python and in R --
    #        This install uses a mix of conda-forge and pip package managers.
         
    # To start, you first need to see your system's NVIDIA graphics card as a GPU under Task Manager on Windows 10 or 11:
    # That should not be a problem in Windows 10 or 11, see the link below for some general info, but use the install code below.
    #      https://hackmd.io/@husohome/Byb6kP6WP 
     
    # For a picture see here:
    #     https://towardsdatascience.com/setting-up-tensorflow-gpu-with-cuda-and-anaconda-onwindows-2ee9c39b5c44 
       
    # but don't follow that site past installing the Community Verson of MS Visual Studio, if needed:
    #     https://visualstudio.microsoft.com/vs/community/

    # The Anaconda software is here:
    #    https://www.anaconda.com/products/distribution
        
    # After the Anaconda installation follow the steps below.
             
    # In the Anaconda Powershell Prompt window
    conda create -n tf python==3.8 conda=4.8
    
    conda env list
    
    conda activate tf
    
    conda install -c conda-forge cudatoolkit=11.2 cudnn=8.1.0
    
    # Check CUDA installation 
    nvcc --version
    conda list cuda
    
    
    # Anything above 2.10.* is not supported on the GPU on Windows Native
    pip install "tensorflow<2.11"
    
    # Find version number of tensorflow (2.10.1)
    conda list tensorflow
              
    # Verfiy TensorFlow - single-line approach
    python -c "import tensorflow as tf; print('\n===== \n GPU Devices: ',tf.config.list_physical_devices('GPU'), '\n=====\n')"
    python -c "import tensorflow as tf; print('\n\n=====\n', tf.reduce_sum(tf.random.normal([1000, 1000])), '\n======\n' )"
         
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

    print(tf.reduce_sum(tf.random.normal([1000, 1000])))
    
    quit()
    >>> 
    
    
    # ------ Extra testing using Jupyter Notebook -----
    
    # Bex T. in the the link below does not install the CUDA and cuDNN under Conda, but below is a quote and 
    #   a Jupyter Notebook test:
    #      https://towardsdatascience.com/how-to-finally-install-tensorflow-gpu-on-windows-10-63527910f255   
    
    # "Think of cuDNN (NVIDIA CUDA® Deep Neural Network library) as a library for Deep Learning 
    #     using CUDA and CUDA as a way to talk to the GPU."
    
    pip install jupyterlab ipykernel
    
    ipython kernel install --user --name=tf
    
    # Use ctrl-c to end
    mkdir jupyter_folder
    jupyter-notebook --notebook-dir jupyter_folder
  
    # Create a new notebook and Run (from the tab above) this 4-line snippet:
    import tensorflow as tf
    from tensorflow.python.client import device_lib
    
    print("Num GPUs Available: ", len(tf.config.list_physical_devices('GPU')))
    device_lib.list_local_devices()
    
    
    # Back in the Anaconda Powershell Prompt
    ctrl-c
    
    conda deactivate
    
    
    # ----- The installs above do not include the optional TensorRT (not for lack of trying under conda on native Windows). -----
    
    # What is TensorRT?
    #      https://catalog.ngc.nvidia.com/orgs/nvidia/containers/tensorrt
    
   
