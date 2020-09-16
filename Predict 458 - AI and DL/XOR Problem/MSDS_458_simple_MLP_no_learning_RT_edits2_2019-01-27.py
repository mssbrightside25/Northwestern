# MSDS_458_simple_MLP_no_learning_RT_edits.py
# this is Dr. Maren's codeset "simple_MLP-fixed-size_Python-3-pt-6_tutorial_code_no-learning_debug_2017-03-25.py" edited


# This is the Simple MLP no backprop code. I have removed all backprop code.



# -*- coding: utf-8 -*-
# We will randomly define initial values for connection weights, and also randomly select
#   which training data that we will use for a given run.
import random

# We want to use the exp function (e to the x); it's part of our transfer function definition
from math import exp

# Biting the bullet and starting to use NumPy for arrays
import numpy as np

# debug near line 583 - deltas did not get the mirror image fix

####################################################################################################
####################################################################################################
#
# Procedure to welcome the user and identify the code
#
####################################################################################################
####################################################################################################

# This procedure is called from the 'main' program.
# Notice that it has an empty parameter list. 
# Procedures require a parameter list, but it can be empty. 

def welcome ():


    print()
    print('starting the welcome() function')
    print("******************************************************************************")
    print()
    print("Welcome to the Multilayer Perceptron Neural Network")
    print("  trained using the backpropagation method.")
    print("Version 1.0, 03/25/2017, A.J. Maren")
    print("For comments, questions, or bug-fixes, contact: alianna.maren@northwestern.edu")
    print()
    print(  "******************************************************************************")
    print()
    print('ending the welcome() function and returning to main')
    print()
    return()

# The above 'return()' statement takes us back to the 'main' module. There have been no parameters
#   defined in this procedure, there is nothing new to return, so the list is still empty.         

####################################################################################################
####################################################################################################
#
# A collection of worker-functions, designed to do specific small tasks
#
####################################################################################################
####################################################################################################

   
#------------------------------------------------------#    

# Compute neuron activation using sigmoid transfer function
def computeTransferFnctn(summedNeuronInput, alpha):
    print()
    print('starting the computeTransferFnction() function')
    activation = 1.0 / (1.0 + exp(-alpha*summedNeuronInput))
    print('The activation is ', activation)
    print('ending the computeTransferFnction() function and returning the activation' )
    print()
    return activation   

#------------------------------------------------------# 
    
# Compute derivative of transfer function
def computeTransferFnctnDeriv(NeuronOutput, alpha):
    return alpha*NeuronOutput*(1.0 -NeuronOutput)     

####################################################################################################
####################################################################################################
#
# Function to obtain the neural network size specifications
#
####################################################################################################
####################################################################################################

def obtainNeuralNetworkSizeSpecs ():
    print('Starting the obtainNeuralNetworkSizeSpecs() function')

# This is a function, not a procedure, because it returns a single value (which really is a list of 
#    three values). It is called directly from 'main.'
#        
# This function specifies the size of the input (I), hidden (H), 
#    and output (O) layers.  
# In a more general version of a neural network, this function should ask the user for the numbers
#    of nodes at each layer of the network. However, our goal is to start with a very simple program. 

# Notice that the three values are being stored in a list, the arraySizeList. 
# This list will be used (by two different functions) to specify the sizes of two different weight arrays:
#   - wWeights; the Input-to-Hidden array, and
#   - vWeights; the Hidden-to-Output array. 

# The following is the kind of code that you would use if you wanted to actually give the network 
#    sizes for each layer yourself. They are commented out in this program, but you could convert them
#    to work and obtain your inputs. 

# Commented-out code: 
#    print 'Define the numbers of input, hidden, and output nodes'          
#    x = input('Enter the number of input nodes here: ')
#    numInputNodes = int(x)
#    print 'The number of input nodes you have entered is', numInputNodes 
#    print 'Thank you; the number of input nodes that will be used is temporarilly hard-coded to 2.' 
#    print #to get a line space between requests
                
#    x = input('Enter the number of hidden nodes here: ')
    # Notice that we are re-using the variable x; it can be reused because it is just catching the 
    #    user's input and then the real variable of interest is defined using it. 
    # Notice that by specifying 'int(x)', we make sure that whatever we catch from the user is 
    #    stored as an integer, not as a float or a string variable.     
#    numHiddenNodes = int(x)
#    print 'The number of hidden nodes you have entered is', numInputNodes 
#    print 'Thank you; the number of hidden nodes that will be used is temporarilly hard-coded to 2.' 
#    print #to get a line space between requests   
        
#    x = input('Enter the number of output nodes here: ')
#    numOutputNodes = int(x)
#    print 'The number of output nodes you have entered is', numInputNodes 
#    print 'Thank you; the number of output nodes that will be used is temporarilly hard-coded to 2.' 
#    print #to get a line space between requests    

    numInputNodes = 2
    numHiddenNodes = 2
    numOutputNodes = 2   
    print()
    print("This network is set up to run the X-OR problem.")
    print("The numbers of nodes in the input, hidden, and output layers have been set to 2 each.") 
            
# We create a list containing the crucial SIZES for the connection weight arrays                
    arraySizeList = (numInputNodes, numHiddenNodes, numOutputNodes)
    print('The number of input nodes is ', numInputNodes)
    print('The number of hidden nodes is ', numHiddenNodes)
    print('The number of output nodes is ', numOutputNodes)
    
    print('Ending the obtainNeuralNetworkSizeSpecs() function')
    print()
    
# We return this list to the calling procedure, 'main'.       
    return (arraySizeList)  

####################################################################################################
####################################################################################################
#
# Function to initialize a specific connection weight with a randomly-generated number between 0 & 1
#
####################################################################################################
####################################################################################################

def InitializeWeight ():
    
    print('starting the InitializeWeight() function')
    randomNum = random.random()
    weight=1-2*randomNum
    print('weight = ', weight)
    print('ending the InitializWeight() function')
    print()
    
    return (weight)  

####################################################################################################
####################################################################################################
#
# Function to initialize the connection weight arrays
#
####################################################################################################
####################################################################################################

def initializeWeightArray (weightArraySizeList, debugInitializeOff=True):
    print('starting the initializeWeightArray() function')

# This function is also called directly from 'main.'
#        
# This function takes in the two parameters, the number of nodes on the bottom (of any two layers), 
#   and the number of nodes in the layer just above it. 
#   It will use these two sizes to create a weight array.
# The weights will initially be given randomly-assigned values, so that we can trace the creation and 
#   transfer of this array back to the 'main' procedure. 
  
# These connection weight values will be stored in an array, the weightArray. 

# This code segment is a function, not a procedure, because it returns a single value (which is actually an array). 
#     The returned value is the new connection weight matrix. 
#     Right now, for simplicity and test purposes, the weightArray is set to a single value. 

# Note my variable-naming convention: 
#    - If it is an array, I call it variableNameArray
#    - If it is a list, I call it variableNameList
#    - It's a lot like calling your dog Roger "rogerDog"
#        and your cat Fluffy "fluffyCat"
#        but until we're better at telling cats from dogs, this helps. 
      
    numBottomNodes = weightArraySizeList[0]
    numUpperNodes = weightArraySizeList[1]
                

# Initialize the weight variables with random weights
    print('initialize wt00 by calling initializeWeight() function')
    wt00=InitializeWeight ()
    print('initialize wt01 by calling initializeWeight() function')
    wt01=InitializeWeight ()
    print('initialize wt02 by calling initializeWeight() function')
    wt10=InitializeWeight ()
    print('initialize wt03 by calling initializeWeight() function')
    wt11=InitializeWeight ()    
    weightArray=np.array([[wt00,wt10],[wt01,wt11]])
    
# Debug mode: if debug is set to False, then we DO NOT do the prints
    if not debugInitializeOff:
# Print the weights
        print ()
        print ("  Inside initializeWeightArray")
# The following two prints are commented out because we have fixed values for 
#    the numbers of nodes in each layer for this version of the program        
#        print '    The number of lower nodes is', numBottomNodes 
#        print '    The number of upper nodes is', numUpperNodes  
        print ()
        print ("    The weights just initialized are: ")
        print ("      weight00 = %.4f,"  % wt00)
        print ("      weight01 = %.4f,"  % wt01)
        print ("      weight10 = %.4f,"  % wt10)
        print ("      weight11 = %.4f,"  % wt11)


# Remember, this is a tutorial program.
#    The following print statements are designed to help you understand how Python
#      stores values in an array, and how it prints these out. 

# The weight array is set up so that it lists the rows, and within the rows, the columns:
#    wt00   wt10
#    wt01   wt11

# The sum of weighted terms should be carried out as follows: 
#      Matrix Formula     =>   Actual Multiplication     Results
#   [wt 00  wt01] * [node0] = wt00*node0 + wt10*node1 = sum-weighted-nodes-to-higher-node0       
#   [wt 10  wt11] * [node1] = wt10*node0 + wt11*node1 = sum-weighted-nodes-to-higher-node1

# Notice that the weight positions are being labeled according to how Python numbers elements in an array
#    ... so the first one is in position [0,0].

# Notice that the position of the weights in the weightArray is not as would be expected:
#  Row 0, Col 0: wt00 = weight connecting 0th lower-level node to 0th upper-level node = weightArray [0,0]
#  Row 0, Col 1: wt10 = weight connecting 1st lower-level node to 0th upper-level node = weightArray [0,1]
#  Row 1, Col 0: wt01 = weight connecting 0th lower-level node to 1st upper-level node = weightArray [1,0]
#  Row 1, Col 1: wt11 = weight connecting 1st lower-level node to 1st upper-level node = weightArray [1,1]
# Notice that wt01 & wt10 are reversed from what we'd expect 

# Debug mode: if debug is set to False, then we DO NOT do the prints
    if not debugInitializeOff:
# Print the entire weights array. 
        print ()
        print ("    The weightArray just established is: ", weightArray)
        print () 
        print ("    Within this array: ") 
        print ("       weight00 = %.4f    weight10 = %.4f"  % (weightArray[0,0], weightArray[0,1]) )
        print ("       weight01 = %.4f    weight11 = %.4f"  % (weightArray[1,0], weightArray[1,1]) )   
        print ("  Returning to calling procedure") 
        print()
    
    print('ending the initializeWeightArray() function and returning to main')
    print()
# We return the array to the calling procedure, 'main'.       
    return (weightArray)  




#------------------------------------------------------# 

# Just a few notes to myself ... on array numbering an printing ... 

# To print a specific element in the weight array; 
#   recall that Python arrays start with
#   numbering the first element as 0-position;
#   so index for an array with two elements goes from 0..1

# The following prints out the weight connecting the first input node (node 0)
#   to the second hidden node (node number 1, as they are numbered 0 .. 1)
#    print wWeightArray[0,1]

####################################################################################################
####################################################################################################
#
# Function to initialize the bias weight arrays
#
####################################################################################################
####################################################################################################

def initializeBiasWeightArray (weightArray1DSize):
    print('starting initializeBiasWeightArray() function')

# This procedure is also called directly from 'main.'       
# This procedure takes in a single parameters; the number of nodes in a given layer. 


# This will be more useful when we move to array operations; right now, it is a placeholder step        
    numBiasNodes = weightArray1DSize
                              
# Hard-coding bias values for two nodes; this code really SHOULD be upgraded to deal with 
#   the number of nodes specified by weightArray1DSize; see subsequent versions for that upgrade. 

# Initialize the bias weight variables with random weights
    print('initialize biasWeight0 by calling initializeWeight() function')
    biasWeight0=InitializeWeight ()
    print('initialize biasWeight1 by calling initializeWeight() function')
    biasWeight1=InitializeWeight ()

# Store the two bias weights in a 1-D array            
    biasWeightArray=np.array([biasWeight0,biasWeight1])

# Notice that the weight positions are being labeled according to how Python numbers elements in a 1-D array
#    ... so the first one is in position [0].

# Print the entire weights array.
    print()
    print('biasWeight0 = ', biasWeight0)
    print('biasWeight1 = ', biasWeight1)
    print('The biasWeightArray = ', biasWeightArray)

    print('ending initializeBiasWeightArray() function')
    print('returning to main')
    print()
    
# We return the array to the calling procedure, 'main'.       
    return (biasWeightArray)  


####################################################################################################
####################################################################################################
#
# Function to obtain a randomly-selected training data set list, which will contain the two input 
#   values (each either 0 or 1), and two output values (see list below), and a third value which is 
#   the number of the training data set, in the range of (0..3). (There are a total of four training 
#   data sets.) 
#
####################################################################################################
####################################################################################################

def obtainRandomXORTrainingValues ():
    
    print('starting obtainRandomXORTrainingValues() function')

   
# The training data list will have four values for the X-OR problem:
#   - First two valuea will be the two inputs (0 or 1 for each)
#   - Second two values will be the two outputs (0 or 1 for each)
# There are only four combinations of 0 and 1 for the input data
# Thus there are four choices for training data, which we'll select on random basis
    
# The fifth element in the list is the NUMBER of the training set; setNumber = 1..3
# The setNumber is used to assign the Summed Squared Error (SSE) with the appropriate
#   training set
# This is because we need ALL the SSE's to get below a certain minimum before we stop
#   training the network
    
    trainingDataSetNum = random.randint(1, 4)
    if trainingDataSetNum >1.1: # The selection is for training lists between 2 & 4
        if trainingDataSetNum > 2.1: # The selection is for training lists between 3 & 4
            if trainingDataSetNum > 3.1: # The selection is for training list 4
                trainingDataList = (1,1,0,1,3) # training data list 4 selected
            else: trainingDataList = (1,0,1,0,2) # training data list 3 selected   
        else: trainingDataList = (0,1,1,0,1) # training data list 2 selected     
    else: trainingDataList = (0,0,0,1,0) # training data list 1 selected 

    print('The trainingDataList = ', trainingDataList)
    print('ending obtainRandomXORTrainingValues() function and returning to main')
    return (trainingDataList)  

####################################################################################################
####################################################################################################
#
# Compute neuron activation - this is the summed weighted inputs after passing through transfer fnctn
#
####################################################################################################
####################################################################################################

def computeSingleNeuronActivation(alpha, wt0, wt1, input0, input1, bias, debugComputeSingleNeuronActivationOff):
    print()
    print('starting computeSingleNeuronActivation() function')
    
# Obtain the inputs into the neuron; this is the sum of weights times inputs
    summedNeuronInput = wt0*input0+wt1*input1+bias

# Pass the summedNeuronActivation and the transfer function parameter alpha into the transfer function
    print('compute the activation by calling computeTransferFnctn()')
    activation = computeTransferFnctn(summedNeuronInput, alpha)

    if not debugComputeSingleNeuronActivationOff:        
        print()
        print("  In computeSingleNeuronActivation with input0, input 1 given as: ", input0, ", ", input1)
        print("    The summed neuron input is %.4f" % summedNeuronInput)   
        print("    The activation (applied transfer function) for that neuron is %.4f" % activation) 
        
    print('ending computeSingleNeuronActivation() function and returning activation')
    print()
    return activation;
   
####################################################################################################
####################################################################################################
#
# Perform a single feedforward pass
#
####################################################################################################
####################################################################################################


def ComputeSingleFeedforwardPass (alpha, inputDataList, wWeightArray, vWeightArray, 
biasHiddenWeightArray, biasOutputWeightArray, debugComputeSingleFeedforwardPassOff):

# Some prints to verify that the input data and the weight arrays have been transferred:
    print()
    print('Starting ComputeSingleFeedforwardPass() function')
    input0 = inputDataList[0]
    input1 = inputDataList[1]      
    print( 'The inputs transferred in are: ')
    print('Input0 = ', input0)
    print('Input1 = ', input1)     
    print()
    print('The initial weights for this neural network are:')
    print('     Input-to-Hidden             Hidden-to-Output')
    # need to correct this code 
    # switching the w(0,1) value in below statement from wWeightArray[0,1], to [1,0]
    # switching the v(0,1) value in below statement from vWeightArray[0,1], to [1,0]
    print('w(0,0) = %.3f   w(0,1) = %.3f         v(0,0) = %.3f   v(0,1) = %.3f' % (wWeightArray[0,0], 
    wWeightArray[0,1], vWeightArray[0,0], vWeightArray [0,1]))
    # need to correct this code
    # switching the w(1,0) value in below statement from wWeightArray[1,0], to [0,1]
    # switching the v(1,0) value in below statement from vWeightArray[1,0], to [0,1]    
    print('w(1,0) = %.3f   w(1,1) = %.3f         v(1,0) = %.3f   v(1,1) = %.3f' % (wWeightArray[1,0], 
    wWeightArray[1,1], vWeightArray[1,0], vWeightArray [1,1]))   
#    actualOutputList = (0,0) 

# Recall from InitializeWeightArray: 
# Notice that the position of the weights in the weightArray is not as would be expected:
#  Row 0, Col 1: wt00 = weight connecting 0th lower-level node to 0th upper-level node = weightArray [0,0]
#  Row 0, Col 1: wt10 = weight connecting 1st lower-level node to 0th upper-level node = weightArray [0,1]
#  Row 0, Col 1: wt01 = weight connecting 0th lower-level node to 1st upper-level node = weightArray [1,0]
#  Row 0, Col 1: wt11 = weight connecting 1st lower-level node to 1st upper-level node = weightArray [1,1]
# Notice that wt01 & wt10 are reversed from what we'd expect 
    
            
# Assign the input-to-hidden weights to specific variables
    wWt00 = wWeightArray[0,0]
    wWt10 = wWeightArray[0,1]
    wWt01 = wWeightArray[1,0]       
    wWt11 = wWeightArray[1,1]
    
# Assign the hidden-to-output weights to specific variables
    vWt00 = vWeightArray[0,0]
    vWt10 = vWeightArray[0,1]
    vWt01 = vWeightArray[1,0]       
    vWt11 = vWeightArray[1,1]    
    
    biasHidden0 = biasHiddenWeightArray[0]
    biasHidden1 = biasHiddenWeightArray[1]
    biasOutput0 = biasOutputWeightArray[0]
    biasOutput1 = biasOutputWeightArray[1]
    
# Obtain the activations of the hidden nodes  
    if not debugComputeSingleFeedforwardPassOff:
        debugComputeSingleNeuronActivationOff = False
    else: 
        debugComputeSingleNeuronActivationOff = True
        
    if not debugComputeSingleNeuronActivationOff:
        print()
        print("  For hiddenActivation0 from input0, input1 = ", input0, ", ", input1)
    
    print()
    print('computing hiddenActivation0 by calling computeSingleNeuronActivation')
    hiddenActivation0 = computeSingleNeuronActivation(alpha, wWt00, wWt10, input0, input1, biasHidden0,
    debugComputeSingleNeuronActivationOff)
    
    if not debugComputeSingleNeuronActivationOff:
        print()
        print("  For hiddenActivation1 from input0, input1 = ", input0, ", ", input1 )
        
    print('computing hiddenActivation1 by calling computeSingleNeuronActivation')
    hiddenActivation1 = computeSingleNeuronActivation(alpha, wWt01, wWt11, input0, input1, biasHidden1,
    debugComputeSingleNeuronActivationOff)
    
    if not debugComputeSingleFeedforwardPassOff: 
        print()
        print("  In computeSingleFeedforwardPass: ")
        print("  Input node values: ", input0, ", ", input1)
        print("  The activations for the hidden nodes are:")
        print("    Hidden0 = %.4f" % hiddenActivation0, "Hidden1 = %.4f" % hiddenActivation1)
        print()


# Obtain the activations of the output nodes 
    print('computing the activation of output node0 by calling computeSingleNeuronActivation')
    outputActivation0 = computeSingleNeuronActivation(alpha, vWt00, vWt10, hiddenActivation0, 
        hiddenActivation1, biasOutput0, debugComputeSingleNeuronActivationOff)
    
    print('computing the activation of output node1 by calling computeSingleNeuronActivation')
    outputActivation1 = computeSingleNeuronActivation(alpha, vWt01, vWt11, hiddenActivation0, 
        hiddenActivation1, biasOutput1, debugComputeSingleNeuronActivationOff)
    if not debugComputeSingleFeedforwardPassOff: 
        print()
        print("  Computing the output neuron activations") 
        print()        
        print("  Back in ComputeSingleFeedforwardPass (for hidden-to-output computations)")
        print("  The activations for the output nodes are:")
        print("    Output0 = %.4f" % outputActivation0, "Output1 = %.4f" % outputActivation1)


    print()
    print('creating the actualAllNodesOutputList')            
    actualAllNodesOutputList = (hiddenActivation0, hiddenActivation1, outputActivation0, outputActivation1)
    print('the hiddenActivation0 = ', hiddenActivation0)
    print('the hiddenActivation1 = ', hiddenActivation1)
    print('the outputActivation0 = ', outputActivation0)
    print('the outputActivation1 = ', outputActivation1)
    print()
    print('ending ComputeSingleFeedforwardPass() function and returning to main')
    
    return (actualAllNodesOutputList);
  
####################################################################################################
####################################################################################################
#
# Determine initial Summed Squared Error (SSE) and Total Summed Squared Error (TotalSSE) as an
#   array, SSE_Array (SSE[0], SSE[1], SSE[2], SSE[3], Total_SSE), return array to calling procedure
# Note: This function returns the SSE_Array
# Note: This function USES computeSingleFeedforwardPass
#
####################################################################################################
####################################################################################################

def computeSSE_Values (alpha, SSE_InitialArray, wWeightArray, vWeightArray, biasHiddenWeightArray, 
biasOutputWeightArray, debugSSE_InitialComputationOff): 
    
    print('starting the computeSSE_Values() function')

    if not debugSSE_InitialComputationOff:
        debugComputeSingleFeedforwardPassOff = False
    else: 
        debugComputeSingleFeedforwardPassOff = True
              
# Compute a single feed-forward pass and obtain the Actual Outputs for zeroth data set
    print()
    print('Compute a single feed-forward pass and obtain the Actual Outputs for zeroth data set')
    inputDataList = (0, 0)           
    actualAllNodesOutputList = ComputeSingleFeedforwardPass (alpha, inputDataList, wWeightArray, vWeightArray,
        biasHiddenWeightArray, biasOutputWeightArray, debugComputeSingleFeedforwardPassOff)        
    actualOutput0 = actualAllNodesOutputList [2]
    actualOutput1 = actualAllNodesOutputList [3] 
    error0 = 0.0 - actualOutput0
    error1 = 1.0 - actualOutput1
    SSE_InitialArray[0] = error0**2 + error1**2

# debug print for function:
    if not debugSSE_InitialComputationOff:
        print()
        print("  In computeSSE_Values")

# debug print for (0,0):
    if not debugSSE_InitialComputationOff: 
        input0 = inputDataList [0]
        input1 = inputDataList [1]
        print()
        print("  Actual Node Outputs for (0,0) training set:")
        print("      input0 = ", input0, "   input1 = ", input1)
        print("      actualOutput0 = %.4f   actualOutput1 = %.4f" %(actualOutput0, actualOutput1))
        print("      error0 =        %.4f   error1 =        %.4f" %(error0, error1))
        print("   Initial SSE for (0,0) = %.4f" % SSE_InitialArray[0])
    print()
    print('Ending a single feed-forward pass to obtain the Actual Outputs for zeroth data set')
        

# Compute a single feed-forward pass and obtain the Actual Outputs for first data set
    print()
    print('Compute a single feed-forward pass and obtain the Actual Outputs for first data set')
    inputDataList = (0, 1)           
    actualAllNodesOutputList = ComputeSingleFeedforwardPass (alpha, inputDataList, wWeightArray, vWeightArray,
    biasHiddenWeightArray, biasOutputWeightArray, debugComputeSingleFeedforwardPassOff)        
    actualOutput0 = actualAllNodesOutputList [2]
    actualOutput1 = actualAllNodesOutputList [3] 
    error0 = 1.0 - actualOutput0
    error1 = 0.0 - actualOutput1
    SSE_InitialArray[1] = error0**2 + error1**2

# debug print for (0,1):
    if not debugSSE_InitialComputationOff: 
        input0 = inputDataList [0]
        input1 = inputDataList [1]
        print()
        print("  Actual Node Outputs for (0,1) training set:")
        print("     input0 = ", input0, "   input1 = ", input1)
        print("     actualOutput0 = %.4f   actualOutput1 = %.4f" %(actualOutput0, actualOutput1))
        print("     error0 =        %.4f   error1 =        %.4f" %(error0, error1))
        print("  Initial SSE for (0,1) = %.4f" % SSE_InitialArray[1])
    print()
    print('Ending a single feed-forward pass to obtain the Actual Outputs for first data set')   
                                                        
# Compute a single feed-forward pass and obtain the Actual Outputs for second data set
    print()
    print('Compute a single feed-forward pass and obtain the Actual Outputs for second data set')
    inputDataList = (1, 0)           
    actualAllNodesOutputList = ComputeSingleFeedforwardPass (alpha, inputDataList, wWeightArray, vWeightArray, 
    biasHiddenWeightArray, biasOutputWeightArray, debugComputeSingleFeedforwardPassOff)        
    actualOutput0 = actualAllNodesOutputList [2]
    actualOutput1 = actualAllNodesOutputList [3] 
    error0 = 1.0 - actualOutput0
    error1 = 0.0 - actualOutput1
    SSE_InitialArray[2] = error0**2 + error1**2
    
# debug print for (1,0):
    if not debugSSE_InitialComputationOff: 
        input0 = inputDataList [0]
        input1 = inputDataList [1]
        print()
        print("  Actual Node Outputs for (1,0) training set:")
        print("     input0 = ", input0, "   input1 = ", input1)
        print("     actualOutput0 = %.4f   actualOutput1 = %.4f" %(actualOutput0, actualOutput1))
        print("     error0 =        %.4f   error1 =        %.4f" %(error0, error1))
        print("  Initial SSE for (1,0) = %.4f" % SSE_InitialArray[2] )   
    print()
    print('Ending a single feed-forward pass to obtain the Actual Outputs for second data set')
    
# Compute a single feed-forward pass and obtain the Actual Outputs for third data set
    print()
    print('Compute a single feed-forward pass and obtain the Actual Outputs for third data set')
    inputDataList = (1, 1)           
    actualAllNodesOutputList = ComputeSingleFeedforwardPass (alpha, inputDataList, wWeightArray, vWeightArray,
    biasHiddenWeightArray, biasOutputWeightArray, debugComputeSingleFeedforwardPassOff)        
    actualOutput0 = actualAllNodesOutputList [2]
    actualOutput1 = actualAllNodesOutputList [3] 
    error0 = 0.0 - actualOutput0
    error1 = 1.0 - actualOutput1
    SSE_InitialArray[3] = error0**2 + error1**2

# debug print for (1,1):
    if not debugSSE_InitialComputationOff: 
        input0 = inputDataList [0]
        input1 = inputDataList [1]
        print()
        print("  Actual Node Outputs for (1,1) training set:")
        print("     input0 = ", input0, "   input1 = ", input1)
        print("     actualOutput0 = %.4f   actualOutput1 = %.4f" %(actualOutput0, actualOutput1))
        print("     error0 =        %.4f   error1 =        %.4f" %(error0, error1))
        print("  Initial SSE for (1,1) = %.4f" % SSE_InitialArray[3])
    print()
    print('Ending a single feed-forward pass to obtain the Actual Outputs for third data set')
    
# Initialize an array of SSE values
    print()
    print('Initializing array of SSE values')
    SSE_InitialTotal = SSE_InitialArray[0] + SSE_InitialArray[1] +SSE_InitialArray[2] + SSE_InitialArray[3]
    print('the SSE_InitialArray[0] = ', SSE_InitialArray[0], 'the SSE_InitialArray[1] = ', SSE_InitialArray[1])
    print('the SSE_InitialArray[2] = ', SSE_InitialArray[2], 'the SSE_InitialArray[3] = ', SSE_InitialArray[3])

# debug print for SSE_InitialTotal:
    if not debugSSE_InitialComputationOff: 
        print()
        print("  The initial total of the SSEs is %.4f" %SSE_InitialTotal)

    SSE_InitialArray[4] = SSE_InitialTotal
    print()
    print('ending the computeSSE_Values() function and returning to main')
    print()
    
    return SSE_InitialArray

####################################################################################################
#**************************************************************************************************#
####################################################################################################
#
# The MAIN module comprising of calls to:
#   (1) Welcome
#   (2) Obtain neural network size specifications for a three-layer network consisting of:
#       - Input layer
#       - Hidden layer
#       - Output layer (all the sizes are currently hard-coded to two nodes per layer right now)
#   (3) Initialize connection weight values
#       - w: Input-to-Hidden nodes
#       - v: Hidden-to-Output nodes
#   (4) Determine the initial Sum Squared Error (SSE) for each training pair, and also the total SSE
# 
#
####################################################################################################
#**************************************************************************************************#
####################################################################################################


def main():

####################################################################################################
# Obtain unit array size in terms of array_length (M) and layers (N)
####################################################################################################                

# This calls the procedure 'welcome,' which just prints out a welcoming message. 
# All procedures need an argument list. 
# This procedure has a list, but it is an empty list; welcome().

    print('Starting main() function')
    print()
    print('Calling welcome() function')

    welcome()
    

# Parameter definitions, to be replaced with user inputs
    print('setting alpha, summedInput, maxNumIterations, and eta')
    alpha = 1.0             # parameter governing steepness of sigmoid transfer function
    summedInput = 1
    maxNumIterations = 4    # You can adjust this parameter; 10,000 typically gives good results when training. 
#   To debug, or to see detailed step-by-step results, change maxNumIterations to a very small number
#     e.g., maxNumIterations = 10000
#   And then set the debug parameters below to be "False"      
    eta = 0.5               # training rate     
    
# Set default values for debug parameters
#    We are setting the debug parameters to be "Off" (debugxxxOff = True)
#    This means that we will NOT see most of the print statements
#    If we want to see a lot of interim print statements, change either or both of the 
#      debugxxxOff parameters to be False, e.g., "debugInitializeOff = False"
    print('Setting the debug_xxx_off statements to be false, so will print extra')
    debugCallInitializeOff = False
    debugInitializeOff = False    

# Right now, for simplicity, we're going to hard-code the numbers of layers that we have in our 
#   multilayer Perceptron (MLP) neural network. 
# We will have an input layer (I), an output layer (O), and a single hidden layer (H). 

# This defines the variable arraySizeList, which is a list. It is initially an empty list. 
# Its purpose is to store the size of the array.

    arraySizeList = list() # empty list

# Notice that I'm using the same variable name, 'arraySizeList' both here in main and in the 
#    called procedure, 'obtainNeuralNetworkSizeSpecs.' 
# I don't have to use the same name; the procedure returns a list and I'm assigning it HERE 
#    to the list named arraySizeList in THIS 'main' procedure. 
# I could use different names. 
# I'm keeping the same name so that it is easier for us to connect what happens in the called procedure
#    'obtainNeuralNetworkSizeSpecs' with this procedure, 'main.' 
       
# Obtain the array sizes (arraySizeList) by calling the appropriate function
    print()
    print('calling the obtainNeuralNetworkSizeSpecs() function')
    arraySizeList = obtainNeuralNetworkSizeSpecs()

# Unpack the list; ascribe the various elements of the list to the sizes of different network layers    
    inputArrayLength = arraySizeList[0]
    hiddenArrayLength = arraySizeList [1]
    outputArrayLength = arraySizeList [2]
    
# In addition to connection weights, we also use bias weights:
#   - one bias term for each of the hidden nodes
#   - one bias term for each of the output nodes
#   This means that a 1-D array of bias weights for the hidden nodes will have the same dimension as 
#      the hidden array length, and
#   also a 1-D array of bias weights for the output nodes will have the same dimension as
#      the output array length. 
    biasHiddenWeightArraySize = hiddenArrayLength
    biasOutputWeightArraySize = outputArrayLength     

# I have all sorts of debug statements left in this, so you can trace the code moving into and out of
#   various procedures and functions.    
            
    print('Flow-of-control trace: Back in main')
    print('I = number of nodes in input layer is', inputArrayLength)
    print('H = number of nodes in hidden layer is', hiddenArrayLength)         
    print('O = number of nodes in output layer is', outputArrayLength)                             


# Initialize the training list
# Note: The training list has, in order, the two input nodes, the two output nodes (this is a two-output
#   version of the X-OR problem), and the data set number (0..3), meaning that each data set is numbered. 
#   This helps in going through the entire data set once the initial weights are established to get a 
#   total sum (across all data sets) of the Summed Squared Error, or SSE.

# Thus, I am initializing the training data list with FIVE values; 
#  -- Zeroth and first values: two inputs into the input layer
#  -- Second and third values: desired values for the two different nodes in the output layer
#  -- Fourth value: the NUMBER of the training data set (which runs from 0 .. 3, for four different data sets)
# All of these are being given an initial value of "0"; we will get actual values
#     when we call a function to return an actual (randomly-selected) training data set. 
    trainingDataList = (0,0,0,0,0)
           

####################################################################################################
# Initialize the weight arrays for two sets of weights; w: input-to-hidden, and v: hidden-to-output
####################################################################################################                

#
# The wWeightArray is for Input-to-Hidden
# The vWeightArray is for Hidden-to-Output
# The wWeightArray is for Input-to-Hidden
# The vWeightArray is for Hidden-to-Output

# We have a single function to initialize weights in a connection weight matrix (2-D array).
#   This function needs to know the sizes (lengths) of the lower and the upper sets of nodes.
#   These form the [row, column] size specifications for the returned weight matrices (2-D arrays). 
#   We will store these sizes in each of two different lists. 

# Specify the sizes for the input-to-hidden connection weight matrix (2-D array)
    wWeightArraySizeList = (inputArrayLength, hiddenArrayLength)
# Specify the sizes for the hidden-to-output connection weight matrix (2-D array)    
    vWeightArraySizeList = (hiddenArrayLength, outputArrayLength)
    

# The node-to-node connection weights are stored in a 2-D array    

# Debug parameter for examining results within initializeWeightArray is currently set to False

    if not debugCallInitializeOff:
        print()
        print("Calling initializeWeightArray for input-to-hidden weights")

# Obtain the actual (randomly-initialized) values for the input-to-hidden connection weight matrix.

    wWeightArray = initializeWeightArray(wWeightArraySizeList, debugInitializeOff)

    if not debugCallInitializeOff:
        print()
        print("Calling initializeWeightArray for Hidden-to-Output weights")    

# Obtain the actual (randomly-initialized) values for the hidden-to-output connection weight matrix.    
    vWeightArray = initializeWeightArray(vWeightArraySizeList, debugInitializeOff)

# Now, we similarly need to obtain randomly-initialized values for the two sets of bias weights. 
#    Each set of bias weights is stored in its respective 1-D array 
#    Recall that we have previously initialized the SIZE for each of these 1-D arrays.  

    print()
    print('calling initializeBiasWeightArray() function for Hidden nodes')
    print()
    biasHiddenWeightArray = initializeBiasWeightArray (biasHiddenWeightArraySize)
    print()
    print('calling initializeBiasWeightArray() function for Output nodes')
    print()
    biasOutputWeightArray = initializeBiasWeightArray (biasOutputWeightArraySize) 


# In a typical program, we would start changing the values of all the connection and bias weights.
#    Here, we want to keep track of our initial values, so we can see how much they change. 
#    To do this, we create separate weightArray matrices, and separate biasArrays, and set them equal to
#    the initial values that we've just obtained. 

    initialWWeightArray = wWeightArray[:]
    initialVWeightArray = vWeightArray[:]
    initialBiasHiddenWeightArray = biasHiddenWeightArray[:]   
    initialBiasOutputWeightArray = biasOutputWeightArray[:] 
    
    print()
    print("The initial weights for this neural network are:")
    print("       Input-to-Hidden                            Hidden-to-Output")
    print("  w(0,0) = %.4f   w(1,0) = %.4f         v(0,0) = %.4f   v(1,0) = %.4f" % (initialWWeightArray[0,0], 
        initialWWeightArray[0,1], initialVWeightArray[0,0], initialVWeightArray[0,1]))
    print("  w(0,1) = %.4f   w(1,1) = %.4f         v(0,1) = %.4f   v(1,1) = %.4f" % (initialWWeightArray[1,0], 
        initialWWeightArray[1,1], initialVWeightArray[1,0], initialVWeightArray[1,1])) 
    print()
    print("       Bias at Hidden Layer                          Bias at Output Layer")
    print("       b(hidden,0) = %.4f                           b(output,0) = %.4f" % (biasHiddenWeightArray[0],
        biasOutputWeightArray[0] ) )                 
    print("       b(hidden,1) = %.4f                           b(output,1) = %.4f" % (biasHiddenWeightArray[1],
        biasOutputWeightArray[1] )  )
  
# Establish some parameters just before we start training
    print()
    print('Establishing parameters - epsilon, iteration counter, SSE_InitialTotal')
    print()
    epsilon = 0.2 # epsilon determines when we are done training; 
                  # for each presentation of a training data set, we get a new value for the summed squared error (SSE)
                  # and we will terminate the run when any ONE of these SSEs is < epsilon;
                  # Note that this is a very crude stopping criterion, we can refine it in later versions. 
                  # must b
    iteration = 0 # This counts the number of iterations that we've made through the training cycle.
    SSE_InitialTotal = 0.0 # We initially set the SSE to be zero before any training pass; we accumulate inputs
                           # into the SSE once we have a set of weights, and push the input data through the network,
                           # generating a set of outputs. 
                           # We compare the generated outputs (actuals) with the desired, obtain errors, square them, 
                           # and sum across all the outputs. This gives our SSE for that particular data set, for that 
                           # particular training pass. 
                           # If the SSE is low enough (< epsilon), we stop training. 


####################################################################################################
# Next step - Get an initial value for the Total Summed Squared Error (Total_SSE)
#   The function will return an array of SSE values, SSE_Array[0] ... SSE_Array[3] are the initial SSEs
#   for training sets 0..3; SSE_Array[4] is the sum of the SSEs. 
####################################################################################################                
        
                        
# Initialize an array of SSE values
# The first four SSE values are the SSE's for specific input/output pairs; 
#   the fifth is the sum of all the SSE's.
    print('Initialize SSE_InitialArray to zeros')
    SSE_InitialArray = [0,0,0,0,0]
    
# Before starting the training run, compute the initial SSE Total 
#   (sum across SSEs for each training data set) 
    debugSSE_InitialComputationOff = False

    print('calling the computeSSE_Values() function to initialize the SSE_InitialArray')
    SSE_InitialArray = computeSSE_Values(alpha, SSE_InitialArray, wWeightArray, vWeightArray, 
                                          biasHiddenWeightArray, biasOutputWeightArray, debugSSE_InitialComputationOff)    

# Start the SSE_Array at the same values as the Initial SSE Array
    SSE_Array = SSE_InitialArray[:] 
    SSE_InitialTotal = SSE_Array[4] 
    print('The SSE_Array = ', SSE_Array)
    print('The SSE_InitialTotal = ', SSE_InitialTotal)
    
# Optionally, print a summary of the initial SSE Total (sum across SSEs for each training data set) 
#   and the specific SSE values 
# Set a local debug print parameter 
    debugSSE_InitialComputationReportOff = False    

    if not debugSSE_InitialComputationReportOff:
        print()
        print("In main, SSE computations completed, Total of all SSEs = %.4f" % SSE_Array[4])
        print("  For input nodes (0,0), SSE_Array[0] = %.4f" % SSE_Array[0])                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
        print("  For input nodes (0,1), SSE_Array[1] = %.4f" % SSE_Array[1]) 
        print("  For input nodes (1,0), SSE_Array[2] = %.4f" % SSE_Array[2]) 
        print("  For input nodes (1,1), SSE_Array[3] = %.4f" % SSE_Array[3]) 
    
    print()
    print()
    print("About to enter the while loop for ", maxNumIterations, " iterations")
    print() 
    
# start the while loop

    while iteration < maxNumIterations:
        if iteration == 0:
            print()
            print('starting at top of while loop for first time')
        else:
            print()
            print('starting at the top of the while loop again')
           

####################################################################################################
# Next step - Obtain a single set of input values for the X-OR problem; two integers - can be 0 or 1
####################################################################################################                

# Randomly select one of four training sets; the inputs will be randomly assigned to 0 or 1
        print()
        print('create random trainingDataList by calling obtainRandomXORTrainingValues() function')
        trainingDataList = obtainRandomXORTrainingValues () 
        input0 = trainingDataList[0]
        input1 = trainingDataList[1] 
        desiredOutput0 = trainingDataList[2]
        desiredOutput1 = trainingDataList[3]
        setNumber = trainingDataList[4] # obtain the number (0 ... 3) of the training data set.       
        print()
        print("Iteration number ", iteration)
        print()
        print("Randomly selected training data set number ", trainingDataList[4]) 
        print("The inputs and desired outputs for the X-OR problem from this data set are:")
        print("          Input0 = ", input0,         "            Input1 = ", input1 )  
        print(" Desired Output0 = ", desiredOutput0, "   Desired Output1 = ", desiredOutput1 )   
        print()
         

####################################################################################################
# Compute a single feed-forward pass
####################################################################################################                
 
# Initialize the error list
        errorList = (0,0)
    
# Initialize the actualOutput list
#    Remember, we've hard-coded the number of hidden nodes and output nodes in this version;
#      numHiddenNodes = 2; numOutputNodes = 2. 
#    We want to see the ACTUAL VALUES ("activations") of both the hidden AND the output nodes;
#      this is just to satisfy our own interest. 
#    In just a few lines down, we will use the function "ComputeSingleFeedforwardPass" to get us all 
#      of those activations. 
        actualAllNodesOutputList = (0,0,0,0)     

# Create the inputData list      
        inputDataList = (input0, input1)         
    
# Compute a single feed-forward pass and obtain the Actual Outputs
        debugComputeSingleFeedforwardPassOff = False
        print('calling ComputeSingleFeedforwardPass to obtain actual outputs for training set')
        actualAllNodesOutputList = ComputeSingleFeedforwardPass(alpha, inputDataList, 
            wWeightArray, vWeightArray, biasHiddenWeightArray, biasOutputWeightArray,debugComputeSingleFeedforwardPassOff)

# Assign the hidden and output values to specific different variables
        actualHiddenOutput0 = actualAllNodesOutputList [0] 
        actualHiddenOutput1 = actualAllNodesOutputList [1] 
        actualOutput0 = actualAllNodesOutputList [2]
        actualOutput1 = actualAllNodesOutputList [3] 
    
# Determine the error between actual and desired outputs

        error0 = desiredOutput0 - actualOutput0
        error1 = desiredOutput1 - actualOutput1
        errorList = (error0, error1)
    
# Compute the Summed Squared Error, or SSE
        SSE0 = error0**2
        SSE1 =  error1**2
        SSEInitial = SSE0 + SSE1
        
                           
        debugMainComputeForwardPassOutputsOff = False
        
# Debug print: the actual outputs from the two output neurons
        if not debugMainComputeForwardPassOutputsOff:
            print()
            print("In main; have just completed a feedfoward pass with training set inputs", input0, input1)
            print("  The activations (actual outputs) for the two hidden neurons are:")
            print("    actualHiddenOutput0 = %.4f" % actualHiddenOutput0)
            print("    actualHiddenOutput1 = %.4f" % actualHiddenOutput1)   
            print("  The activations (actual outputs) for the two output neurons are:")
            print("    actualOutput0 = %.4f" % actualOutput0)
            print("    actualOutput1 = %.4f'" % actualOutput1 )
            print("  Initial SSE (before backpropagation) = %.6f" % SSEInitial)
            print("  Corresponding SSE (from initial SSE determination) = %.6f" % SSE_Array[setNumber])    
  

# Assign the SSE to the SSE for the appropriate training set
        SSE_Array[setNumber] = SSEInitial


# Compute the new sum of SSEs (across all the different training sets)
#   ... this will be different because we've changed one of the SSE's
        print()
        print("SSE_Array[0] = ", SSE_Array[0], "SSE_Array[1] = ", SSE_Array[1])
        print("SSE_Array[2] = ", SSE_Array[2], "SSE_Array[3] = ", SSE_Array[3])
        print("SSE_Array[4] = ", SSE_Array[4])
        
        newSSE_Total = SSE_Array[0] + SSE_Array[1] +SSE_Array[2] + SSE_Array[3]
        
        print("    For node 0: Desired Output = ",desiredOutput0,  " New Output = %.4f" % actualOutput0 )
        print("    For node 1: Desired Output = ",desiredOutput1,  " New Output = %.4f" % actualOutput1 ) 
        print("              Error(0) = %.4f,           Error(1) = %.4f" %(error0, error1))
        print("     Squared Error (0) = %.4f,   Squared Error(1) = %.4f" %(SSE0, SSE1))  
        
# Assign the new SSE to the final place in the SSE array
        SSE_Array[4] = newSSE_Total
        print()
        print("  The sum of these squared errors (SSE) for training set ", trainingDataList[4], " is %.4f" %newSSE_Total )  
        
        print('Will now add 1 to the iteration counter, and go back to the top of the while loop')
        iteration = iteration + 1

        if newSSE_Total < epsilon:
            print('newSSE_Total is now less than epsilon')

            break
            
    print()
    print("Out of while loop after ", maxNumIterations, " iterations")     


    print(' ')
    print('The initial weights for this neural network are:')
    print('     Input-to-Hidden                       Hidden-to-Output')
    print('w(0,0) = %.3f   w(0,1) = %.3f         v(0,0) = %.3f   v(0,1) = %.3f' % (initialWWeightArray[0,0], 
        initialWWeightArray[0,1], initialVWeightArray[0,0], initialVWeightArray[0,1]))
    print('w(1,0) = %.3f   w(1,1) = %.3f         v(1,0) = %.3f   v(1,1) = %.3f' % (initialWWeightArray[1,0], 
        initialWWeightArray[1,1], initialVWeightArray[1,0], initialVWeightArray[1,1]))        

                                                                                    
    print(' ')
    print('The final weights for this neural network are:')
    print('     Input-to-Hidden                       Hidden-to-Output')
    print('w(0,0) = %.3f   w(0,1) = %.3f         v(0,0) = %.3f   v(0,1) = %.3f' % (wWeightArray[0,0], 
        wWeightArray[0,1], vWeightArray[0,0], vWeightArray[0,1]))
    print('w(1,0) = %.3f   w(1,1) = %.3f         v(1,0) = %.3f   v(1,1) = %.3f' % (wWeightArray[1,0], 
        wWeightArray[1,1], vWeightArray[1,0], vWeightArray[1,1]))        
                                                                                    
   
# Print the SSE's at the beginning of training
    print(' ')
    print('The SSE values at the beginning of training were: ')
    print('  SSE_Initial[0] = %.4f' % SSE_InitialArray[0])
    print('  SSE_Initial[1] = %.4f' % SSE_InitialArray[1])
    print('  SSE_Initial[2] = %.4f' % SSE_InitialArray[2])
    print('  SSE_Initial[3] = %.4f' % SSE_InitialArray[3])   
    print(' ')
    print('The total of the SSE values at the beginning of training is %.4f' % SSE_InitialTotal) 


# Print the SSE's at the end of training
    print(' ')
    print('The SSE values at the end of training were: ')
    print('  SSE[0] = %.4f' % SSE_Array[0])
    print('  SSE[1] = %.4f' % SSE_Array[1])
    print('  SSE[2] = %.4f' % SSE_Array[2])
    print('  SSE[3] = %.4f' % SSE_Array[3])    
    print(' ')
    print('The total of the SSE values at the end of training is %.4f' % SSE_Array[4])                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
# Print comparison of previous and new outputs if using backprop            
#    print(' ') 
#    print('Values for the new outputs compared with previous, given only a partial backpropagation training:')
#    print('     Old:', '   ', 'New:', '   ', 'nu*Delta:')
#    print('Output 0:  Desired = ', desiredOutput0, 'Old actual =  %.4f' % actualOutput0, 'Newactual  %.4f' % newOutput0)
#    print('Output 1:  Desired = ', desiredOutput1, 'Old actual =  %.4f' % actualOutput1, 'Newactual  %.4f' % newOutput1)                                                                        
    
    print()
    print('ending main() function')   


####################################################################################################
# Conclude specification of the MAIN procedure
#################################################################################################### 
# This command is what executes the main() function

print('Calling the main() procedure from the program')

if __name__ == "__main__": main()

####################################################################################################
# End program
#################################################################################################### 
