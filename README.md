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
- Data: the speech signals are provided 
- Code: 

