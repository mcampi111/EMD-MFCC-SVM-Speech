# EMD-MFCC-SVM-Speech — Robust speaker verification against synthetic speech

Code and data for Campi, Peters, Azzaoui & Matsui (2021), *Machine learning mitigants for
speech based cyber risk*, **IEEE Access** 9, 136831–136860.
[IEEE Xplore](https://ieeexplore.ieee.org/document/9555610)

Text-to-speech systems can now imitate a speaker well enough to fool automatic speaker
verification. This project builds features that separate a real speaker from synthetic
imitations of the same text, under realistic conditions: ordinary microphones, noisy and
reverberant rooms, sentences that are not phonetically balanced.

The approach combines **Empirical Mode Decomposition (EMD)** with classical cepstral
features. Each utterance is decomposed into intrinsic mode functions (IMFs); from each IMF
we extract instantaneous-frequency statistics, spline coefficients and **IMF-level MFCCs**,
and classify real versus synthetic speech with **support vector machines** across six kernels
and with **multiple-kernel learning**.

## Experiments

| | Experiment 1 | Experiment 2 |
|---|---|---|
| Material | Dante, *Inferno* — deliberately not phonetically balanced | Harvard Sentences (IEEE 1965 phonetically balanced lists) |
| Utterances | 960 (80 / 20 train–test split) | 1,152 (first vs second sentence of each of 72 lists) |
| Voices | Real female speaker; five TTS voices | Real female speaker; TTS voice |

Recordings at 44.1 kHz, 15 s – 1 min each, trimmed and decimated to 60,000 samples,
then split into non-overlapping 5,000-sample windows before EMD sifting.

## Repository layout

| Folder | Content |
|---|---|
| `Data/Experiment 1`, `Data/Experiment 2` | Female speaker and synthetic recordings, training and test sets (compressed) |
| `Code/` | EMD and IMF extraction, feature extraction, standardisation, IMF-cepstral features |
| `Code/InSample_Code/<Kernel>` | In-sample SVM analysis, one folder per kernel (Bessel, Laplace, Linear, Polynomial, RBF, Sigmoid) |
| `Code/OutOfSample_Code/<Kernel>` | Out-of-sample SVM analysis, same structure |
| `Code/Multi_Kernel_Learning/MKL_Ex1`, `MKL_Ex2` | Multiple-kernel learning weights and kernel functions |

### Running

1. **Signals → IMFs.** Run the model script for each voice (e.g. `SPEAKER1_MODEL`,
   `SYNTHETIC_MODEL`); EMD is applied and the IMFs extracted.
2. **IMFs → features.** Run the extraction scripts (`*_Extraction_feature*`), then
   `Preprocessing_1` to standardise. IMF-cepstral features come from
   `SYNT_SPEAK1_CEPSTRUM_IMF_FEATURE`.
3. **Classification.** Run the SVM scripts in the kernel folders, or the MKL scripts.

In each script, set the working directory and the number of sentences `m`
(Experiment 1: 100 in-sample, 20 out-of-sample; Experiment 2: 72).

Main R packages: `hht` (EMD), `seewave`, `kernlab`, `caret`, `pROC`, `ROCR`, `splines`, `entropy`.

## Citation

```bibtex
@article{campi2021machine,
  title   = {Machine learning mitigants for speech based cyber risk},
  author  = {Campi, Marta and Peters, Gareth W. and Azzaoui, Nourddine and Matsui, Tomoko},
  journal = {IEEE Access},
  volume  = {9},
  pages   = {136831--136860},
  year    = {2021},
  publisher = {IEEE}
}
```

## Contact and license

Marta Campi — marta.campi@uzh.ch. Male-speaker recordings are available on request.
Code: MIT — see [LICENSE](LICENSE).
