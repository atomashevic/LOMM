# Applied behavioral-science entry points for the RR introduction

Date: 2026-05-18

## Scope

This note supports the targeted introduction paragraph in `paper/Paper_emotion_dynamics_HGolino_RR.qmd`. The goal is not a broad survey of facial-expression recognition, but a motivation bridge for behavioral-science readers: video-derived FER-like score series are becoming usable in applied behavioral settings, yet the methodological claim should remain about latent dynamics, measurement error, and dimensional recovery.

Local sources inspected:

- `/home/atomasevic/lab/methods/dlo-paper-rr.md`
- `paper-plan.md`
- `style-guide.md`
- `paper/lit-review/lomm_model_landscape_review.md`

Search path:

- Ran `ob sync` before inspecting the lab vault.
- Used the `literature-review` skill workflow with `paper-search semanticscholar` queries around emotion dynamics, automated facial-expression analysis, social interaction video, and affect dynamics.
- `paper-search google scholar` was unavailable because `SERPER_API_KEY` is not set.
- Some Semantic Scholar queries returned `RetryError`, so final citation details were checked against primary or publisher pages where possible.

## Best applied motivation

The strongest applied frame is not "FER makes emotion visible." It is:

> Behavioral scientists increasingly have video records of affectively meaningful behavior, but their substantive claims are about latent regulation, coordination, persuasion, clinical functioning, or social interaction. LOMM fits as a validation scaffold between frame-level model scores and psychology-readable temporal structure.

This keeps the paragraph aligned with the revision strategy: the paper is a methods paper for video-rich behavioral research, not a general validation of zero-shot FER.

## Citation clusters

### 1. Emotion dynamics and behavioral relevance

- Kuppens & Verduyn (2017), *Emotion dynamics*, is the clean review citation for the idea that emotion dynamics concerns trajectories, patterns, regularities, underlying processes, and downstream consequences. DOI: `10.1016/j.copsyc.2017.06.004`.
- Houben, Van Den Noortgate, & Kuppens (2015), *The relation between short-term emotion dynamics and psychological well-being: A meta-analysis*, supports the applied importance of short-term affect dynamics for well-being and psychopathology. DOI: `10.1037/a0038822`.
- Hollenstein (2007), *State space grids: Analyzing dynamics across development*, is useful for the behavioral/developmental tradition of real-time socioemotional dynamics. DOI: `10.1177/0165025407077765`.

Candidate QMD keys:

- `@kuppensEmotionDynamics2017`
- `@houbenRelationShorttermEmotion2015`
- `@hollensteinStateSpaceGrids2007`

### 2. Video/facial-analysis use cases in behavioral domains

- Halamova et al. (2023), *Automated facial expression analysis of participants self-criticising via the two-chair technique*, gives a clinical/therapy task example where self-report limitations motivate behavioral facial markers. DOI: `10.3389/fpsyg.2023.1138916`.
- Manelis-Baram et al. (2025), *Comparing three algorithms of automated facial expression analysis in autistic children*, uses full ADOS-2 clinical assessment videos and compares iMotions, FaceReader, and Py-Feat. DOI: `10.1186/s13229-025-00685-x`.
- Li et al. (2024), *Decoding Group Emotional Dynamics in a Web-Based Collaborative Environment*, gives a collaborative-learning/group-dynamics example using multi-person FER. DOI: `10.1080/10447318.2024.2338614`.
- Holiday et al. (2023), *A Multimodal Emotion Perspective on Social Media Influencer Marketing*, gives a social-media/consumer-behavior video example using facial-expression and linguistic analysis on influencer posts. DOI: `10.1177/10949968231171104`.
- Hofling & Alpers (2023), *Automatic facial coding predicts self-report of emotion, advertisement and brand effects elicited by video commercials*, is a secondary consumer-behavior option. DOI: `10.3389/fnins.2023.1125983`.

Candidate QMD keys:

- `@halamovaAutomatedFacialExpression2023`
- `@manelisbaramComparingThreeAlgorithms2025`
- `@liDecodingGroupEmotional2024`
- `@holidayMultimodalEmotionPerspective2023`
- `@hoflingAutomaticFacialCoding2023`

### 3. Measurement and interpretation caution

- McKeown & Allison (2024), *Are facial expression technologies tools for emotion recognition or social interaction analysis, or both?*, is a useful conceptual caution: facial-expression technologies can support social-interaction analysis and should not be framed only as direct emotion recognition.
- Kankaanpaa et al. (2026), *Comparing Multiple-Indicator Approaches to Account for Measurement Error in Dynamic Networks*, is the key psychometric/network-methods bridge. It supports the explicit measurement layer by showing that measurement error biases dynamic-network estimates and that multiple-indicator approaches can mitigate the problem. DOI: `10.1007/s10608-026-10719-0`.

Candidate QMD keys:

- `@mckeownAreFacialExpression2024`
- `@kankaanpaa2026multipleindicator`

## Paragraph strategy

Use one paragraph after the existing bridge paragraph. It should:

1. Name the applied domains without turning the paper into any one of them.
2. State the actual behavioral questions: recovery, volatility, return after perturbation, and lower-dimensional expressive structure.
3. Use recent applied facial-analysis studies only as evidence that video-based behavioral scoring is now entering real applied workflows.
4. End by making LOMM the validation scaffold: not frame-level emotion classification, but testing recoverable latent temporal structure under measurement noise.
