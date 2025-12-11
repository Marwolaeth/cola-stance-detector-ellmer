programming_tweets <- tibble::tibble(
    tweet = c(
        # Julia - positive technical
        "Julia's SciML ecosystem is absolutely phenomenal for solving complex differential equations. The performance and expressiveness combined is unmatched. #JuliaLang",
        "Just solved a system of stiff ODEs in Julia that would've taken hours in Python. The multiple dispatch paradigm is a game-changer. #ScientificComputing",
        
        # Julia - neutral/technical
        "Julia 1.9 released with improved compilation times. Still learning the language but the type system is quite elegant.",
        "Trying Julia for the first time. The syntax is familiar but the performance characteristics are very different from what I'm used to.",
        "Julia's package manager is solid. Reproducible environments are important for research code.",
        
        # Julia - negative
        "Julia's first-run compilation time is absolutely brutal. Waiting 30 seconds for a simple script to execute is not acceptable. #JuliaProblems",
        "Spent an hour debugging Julia code only to realize it was a type instability issue. The error messages could be way more helpful.",
        
        # Python - positive
        "Python's simplicity is why it dominates data science. The ecosystem is just unbeatable. #DataScience #Python",
        "NumPy + SciPy + Matplotlib = the holy trinity of scientific computing in Python. Been using this stack for 10 years.",
        "Django and Flask make web development so accessible. Python really democratized programming.",
        
        # Python - neutral/technical
        "Python's GIL is still a bottleneck for parallel processing. Working around it with multiprocessing.",
        "Learning FastAPI. The type hints integration is really nice for building APIs.",
        "Python 3.12 performance improvements are solid but still not matching compiled languages for heavy computation.",
        "pip, conda, poetry... the Python packaging ecosystem is fragmented but getting better.",
        
        # Python - negative
        "Python's list comprehensions and nested brackets are getting out of hand. [[x for x in [y for y in range(10)]]] is not readable code.",
        "Why does Python need so many ways to do the same thing? The syntax is bloated and inconsistent. Readability is suffering.",
        
        # R - positive
        "R is simpler to both read and write and is way more concise than Python. The tidyverse changed everything for data wrangling.",
        "ggplot2 is still the best data visualization library I've ever used. Nothing comes close.",
        "R's functional programming paradigm makes data pipelines so elegant. dplyr + magrittr = chef's kiss",
        
        # R - neutral/technical
        "Just migrated a Shiny app to production. The reactive programming model takes getting used to but it's powerful.",
        "R's vectorization is beautiful when you understand it. Loops are for other languages.",
        "Working with tidymodels for machine learning in R. The API design is really thoughtful.",
        "R's documentation could be better but the community is incredibly helpful.",
        
        # R - negative
        "Whoever calls R a programming language ffs. It's a statistical toolkit masquerading as one. The inconsistencies are maddening.",
        
        # Comparisons
        "R for stats, Python for general purpose, Julia for performance. Each language has its niche.",
        "Why choose? I use Python for data collection, R for analysis, and Julia for simulations. Polyglot programming FTW.",
        "Python is more readable than R for beginners, but R's data manipulation syntax is superior once you learn it.",
        
        # General/mixed
        "The best programming language is the one that solves your problem. Stop the language wars.",
        "Learning multiple languages makes you a better programmer. Perspective matters.",
        "Open source scientific computing has come so far. Amazing what the community has built.",
        "Code review is more important than which language you use. Good practices transcend syntax.",
        "Performance optimization is premature until you've profiled. Write clear code first.",
        "The learning curve for Julia is steeper but the payoff for numerical computing is real.",
        "Reproducible research requires reproducible code. Language choice matters less than version control.",
        "Just finished a project using all three: R for EDA, Python for ML pipeline, Julia for numerical solving. Great experience!"
    )
)

programming_tweets
