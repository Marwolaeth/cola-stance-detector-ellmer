# COLA Stance Detector

COLA Stance Detector is an R implementation of the COLA (**C**ollaborative r**O**le-infused **L**LM-based **A**gents – see Lan et al. (2024)) framework for automated stance detection in social media texts. The system employs a three-stage collaborative approach where specialised LLM-based agents analyse texts from linguistic, domain-specific, and social media perspectives to determine whether an author's position is in favour, against, or neutral towards a given target.

The framework operates through multidimensional text analysis, reasoning-enhanced debating, and structured conclusion stages. By designating distinct roles to LLM agents, the system achieves transparent, explainable stance classification without requiring additional annotated training data or model fine-tuning. This approach has proven effective for detecting implicit stances where viewpoints are embedded rather than explicitly stated (see the corresponding [section](https://arxiv.org/html/2310.10467v2#Sx4 "Stance Detection with Collaborative Role-Infused LLM-Based Agents: Experiments") of the [paper](https://arxiv.org/html/2310.10467v2 "Stance Detection with Collaborative Role-Infused LLM-Based Agents")).

This repository provides the core COLA implementation in R, with plans to extend it into a production-ready Shiny application using the Golem framework. The modular architecture supports both single-text and batch processing workflows, making it suitable for large-scale content analysis in web and social media research.

## License

This project is licensed under the MIT License — see the LICENSE file for details.

## Citation & Attribution

This implementation is based on the COLA framework from:

Lan, X., Gao, C., Jin, D., & Li, Y. Stance Detection with Collaborative Role-Infused LLM-Based Agents. [Original paper licensed under CC BY 4.0]

When using this code, please cite the original COLA paper.
