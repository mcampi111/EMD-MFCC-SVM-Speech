# Speech-Experiment
Repository with data, code related to the speech experiments conducted on both Datasets of the paper. 

In the test below, references, tables and figures are provided in the body of the paper. 

In the experiments we consider two classes of study: the first involved is a real data set specifically constructed for this study to assess several aspects of the
proposed methodology in detail; the second study is a bench mark data set widely used in speech analysis testing of speaker identification.

The real data experiments were comprised of two sources of real speech from a female (speaker 1) and a male (speaker 2) and two sources of synthetic speech. The female
and male synthetic speech were selected to have an English accent and are obtained through an online text-to-speech system (http://www.fromtexttospeech.com/). The
voice recordings were sampled at 44.1kHz without significant channel or background noise, in order to develop a text-dependent scenario relevant for speaker verification
tasks [30]. Recording environments of training and testing voice samples were identical to avoid mismatched conditions see [29] and [30]. Common sentences were used for each
speaker and for the synthetic voice. The training and testing sets of data were then partitioned into training data and testing data.

The duration of each sentences speech recording was approximately 15sec to 1min maximum giving samples between 661k and 2646k samples. The start and end of each
sample were trimmed to remove any non-speech segments and then decimated to a set of 60k total samples. Then each set of 60k samples for a speech signal for one sentences
was then windowed into non-overlaping collections of 5,000 samples passed to the EMD sifting procedure per window to obtain for each window the required sample summary
statistics outlined previously in Table 1. We note that in some cases, we found that for high frequency instantaneous frequency features it would be advantageous to also
apply a median filter (we used a window of 2ms). In the first real data study the total number of recorded sentences was 480 sentences, equally proportioned samples of the same sentences across all voice recordings, with 70% randomly selected for training and the rest for testing. The sentence data were selected to be challenging to reect a real ASV setting in which they are not phonetically balanced and obtained from the first text (Inferno) that makes up Dante Alighieri "The Divine Comedy".

The second real data study is a reference case study based on the IEEE Recommended Practices for Speech Quality Measurements as described in [27]. This data sets out seventy-two lists of ten phrases each which are described as the 1965 Revised List of Phonetically Balanced Sentences otherwise known as the "Harvard Sentences". They are widely used in research on telecommunications, speech, and acoustics, where standardized and repeatable sequences of speech are needed. The training data comprised the first sentence from each of the 72 lists of Harvard sentences and the testing data comprised the second sentence from each of the 72 lists of Harvard sentences. To reduce the total space required to present results, we focus on presenting key aspects of the out-of-sample analysis that represent the most challenging cases for assessing our proposed EMD-MFCC methodology.



The folders within this repository are organised as follows:
- Data:
       the speech signals are provided within two different folders, one for each experiment. Both in-samples and out-of-samples are provided. In the body of the paper,
       we described the results for Speaker 1 (the female voice) only, therefore we put in these folders the female voices. Male speech signals may also be available under              request.

- Code: 
       - For each speaker, both synthetic and real voices, we produce a file creating the required model or time-series in R called, for example, "SPEAKER1_MODEL" for the real            Speaker or "SYNTHETIC_MODEL", for the synthetic one. Afterwards, the EMD is applied and the IMFs are extracted. Note that when the code is run, results need to be saved          by the user. A second file is then the one extracting the features required for the SVM, called "Speaker1_Extraxtion_feeature" or "Synthetic_Extraxtion_feeature" These          files extract the instantaneous frequencies, the statistics and the spline coefficients. These features are then passed through the file "Preprocessing_1", which                standardise the feature. For the EMD-MFCCs, another file is generated, called "SYNT_SPEAK1_CEPSTRUM_IMF_FEATURE". Note that depending on the experiment, the directory            needs to be updated along with the m parameter representing the number of sentences (i.e. for Ex.1 m = 100, while for Ex.2 m = 72). This also applies in the case of the          in-sample-analysis (as before) and out-of-sample analysis (i.e. for Ex.1 m = 20, while for Ex.2 m = 72). Once that the features are extracted, they can be passed                through the code for the SVM provided in the following folders.       
       - InSample_Code:
         In this folder, we entered the code used for the in-sample analysis. As above, directory and the m parameter have to be updated when running the code. The folder                contains one file for the SVMs related to each features and R files generating the final tables.         
       - OutOfSample_Code:
         In this folder, we entered the code used for the out-of-sample analysis. As above, directory and the m parameter have to be updated when running the code. The folder            contains one file for the SVMs related to each features and R files generating the final tables.         
       - MultiKernelLearning:
         In this folder, we entered the code used for the Multi-Kernel Learning Experiments. Results are provided in the body of the paper. We divided the folders with                    respect to the two experiments, given that we computed the weights and the functions used for the MKL. Such functions are passed to the SVM main function. 
         
         
For any queries, I am avaliable at marta.campi.15@ucl.ac.uk or marta.campi.11@gmail.com

